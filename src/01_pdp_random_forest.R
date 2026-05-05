library(dplyr)
library(ggplot2)
library(ranger)
library(readr)
library(scales)
library(tidyr)

set.seed(20260505)

pdp_sample_size <- 50
house_sample_size <- 1500

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

predict_ranger <- function(model, newdata) {
  predict(model, data = as.data.frame(newdata))$predictions
}

grid_values <- function(x, n = 50, discrete_limit = 30) {
  x <- x[!is.na(x)]
  lower <- as.numeric(quantile(x, 0.01, names = FALSE))
  upper <- as.numeric(quantile(x, 0.99, names = FALSE))
  x_trimmed <- x[x >= lower & x <= upper]
  unique_values <- sort(unique(x_trimmed))

  if (length(unique_values) <= discrete_limit) {
    unique_values
  } else {
    seq(lower, upper, length.out = n)
  }
}

pdp_1d <- function(model, sample_data, feature, grid) {
  bind_rows(lapply(grid, function(value) {
    newdata <- sample_data
    newdata[[feature]] <- value
    tibble(
      feature = feature,
      value = value,
      prediction = mean(predict_ranger(model, newdata))
    )
  }))
}

pdp_2d <- function(model, sample_data, feature_x, feature_y, grid_x, grid_y) {
  grid <- expand.grid(
    value_x = grid_x,
    value_y = grid_y,
    KEEP.OUT.ATTRS = FALSE
  )

  bind_rows(lapply(seq_len(nrow(grid)), function(i) {
    newdata <- sample_data
    newdata[[feature_x]] <- grid$value_x[i]
    newdata[[feature_y]] <- grid$value_y[i]
    tibble(
      !!feature_x := grid$value_x[i],
      !!feature_y := grid$value_y[i],
      prediction = mean(predict_ranger(model, newdata))
    )
  }))
}

pdp_summary <- function(pdp_data, dataset_name) {
  pdp_data %>%
    group_by(feature) %>%
    arrange(value, .by_group = TRUE) %>%
    summarise(
      dataset = dataset_name,
      min_prediction = min(prediction),
      value_at_min = value[which.min(prediction)],
      max_prediction = max(prediction),
      value_at_max = value[which.max(prediction)],
      first_grid_prediction = first(prediction),
      last_grid_prediction = last(prediction),
      edge_delta = last(prediction) - first(prediction),
      .groups = "drop"
    ) %>%
    select(dataset, everything())
}

model_metrics <- function(model, dataset_name, target, n_training, n_pdp) {
  tibble(
    dataset = dataset_name,
    target = target,
    n_training = n_training,
    n_pdp_sample = n_pdp,
    trees = model$num.trees,
    oob_mse = model$prediction.error,
    oob_rmse = sqrt(model$prediction.error),
    oob_r2 = model$r.squared
  )
}

importance_table <- function(model, dataset_name) {
  tibble(
    dataset = dataset_name,
    feature = names(model$variable.importance),
    importance = as.numeric(model$variable.importance)
  ) %>%
    arrange(dataset, desc(importance))
}

sample_pdp_rows <- function(data, n = pdp_sample_size) {
  if (nrow(data) < n) {
    stop("The PDP sample must contain exactly ", n, " rows.")
  }

  slice_sample(data, n = n)
}

theme_xai <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
}

theme_reference <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9),
      panel.grid.minor = element_blank(),
      legend.position = "right"
    )
}

save_reference_plot <- function(plot, filename, width = 9, height = 5.5) {
  ggsave(
    file.path("outputs/figures", filename),
    plot,
    width = width,
    height = height,
    dpi = 300,
    bg = "white"
  )
}

plot_pdp_1d_reference <- function(
    pdp_data,
    sample_data,
    feature_name,
    title,
    x_label,
    y_label,
    y_scale = label_comma(),
    line_color = "#1f4e79") {
  ggplot(filter(pdp_data, feature == feature_name), aes(value, prediction)) +
    geom_line(color = line_color, linewidth = 1) +
    geom_rug(
      data = sample_data,
      aes(x = .data[[feature_name]]),
      inherit.aes = FALSE,
      sides = "b",
      alpha = 0.24,
      color = "#7a6651"
    ) +
    scale_y_continuous(labels = y_scale) +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_reference()
}

# Exercise 1 and 2: bike rentals --------------------------------------------

bike <- read_csv("day.csv", show_col_types = FALSE) %>%
  mutate(
    dteday = as.Date(dteday),
    days_since_2011 = as.integer(dteday - as.Date("2011-01-01")) + 1,
    season = factor(season),
    holiday = factor(holiday),
    weekday = factor(weekday),
    workingday = factor(workingday),
    weathersit = factor(weathersit)
  )

bike_features <- c(
  "days_since_2011",
  "season",
  "holiday",
  "weekday",
  "workingday",
  "weathersit",
  "temp",
  "hum",
  "windspeed"
)

bike_train <- bike %>%
  select(cnt, all_of(bike_features))

bike_model <- ranger(
  cnt ~ .,
  data = as.data.frame(bike_train),
  num.trees = 500,
  mtry = 3,
  min.node.size = 5,
  importance = "permutation",
  seed = 20260505,
  respect.unordered.factors = "order"
)

bike_pdp_sample <- bike_train %>%
  sample_pdp_rows()

bike_pdp_features <- c("days_since_2011", "temp", "hum", "windspeed")
bike_pdp_1d <- bind_rows(lapply(bike_pdp_features, function(feature) {
  pdp_1d(
    bike_model,
    bike_pdp_sample %>% select(all_of(bike_features)),
    feature,
    grid_values(bike_train[[feature]], n = 60)
  )
}))

bike_distribution <- bike_pdp_sample %>%
  select(all_of(bike_pdp_features)) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "value")

bike_feature_labels <- c(
  days_since_2011 = "Days since 2011",
  temp = "Temperature",
  hum = "Humidity",
  windspeed = "Wind speed"
)

bike_pdp_plot <- ggplot(bike_pdp_1d, aes(value, prediction)) +
  geom_line(color = "#1f77b4", linewidth = 1) +
  geom_rug(
    data = bike_distribution,
    aes(x = value),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.18
  ) +
  facet_wrap(
    ~feature,
    scales = "free",
    labeller = as_labeller(bike_feature_labels)
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Bike rentals: one-dimensional partial dependence",
    subtitle = "Random forest trained on all daily records; PDP averaged on a 50-day sample",
    x = NULL,
    y = "Predicted bike count"
  ) +
  theme_xai()

save_reference_plot(
  plot_pdp_1d_reference(
    bike_pdp_1d,
    bike_pdp_sample,
    "days_since_2011",
    "Partial Dependence of Bike Rentals on Days Since 2011",
    "Days since 2011-01-01",
    "Predicted Bike count"
  ),
  "bike_pdp_days_since_2011.png"
)

save_reference_plot(
  plot_pdp_1d_reference(
    bike_pdp_1d,
    bike_pdp_sample,
    "temp",
    "Partial Dependence of Bike Rentals on Temperature",
    "Temperature (normalized)",
    "Predicted Bike count"
  ),
  "bike_pdp_temperature.png"
)

save_reference_plot(
  plot_pdp_1d_reference(
    bike_pdp_1d,
    bike_pdp_sample,
    "hum",
    "Partial Dependence of Bike Rentals on Humidity",
    "Humidity (normalized)",
    "Predicted Bike count"
  ),
  "bike_pdp_humidity.png"
)

save_reference_plot(
  plot_pdp_1d_reference(
    bike_pdp_1d,
    bike_pdp_sample,
    "windspeed",
    "Partial Dependence of Bike Rentals on Wind Speed",
    "Wind speed (normalized)",
    "Predicted Bike count"
  ),
  "bike_pdp_windspeed.png"
)

ggsave(
  "outputs/figures/bike_pdp_1d.png",
  bike_pdp_plot,
  width = 10,
  height = 6,
  dpi = 300
)

temp_grid <- grid_values(bike_train$temp, n = 15)
hum_grid <- grid_values(bike_train$hum, n = 15)
tile_width <- diff(range(temp_grid)) / (length(temp_grid) - 1) * 1.03
tile_height <- diff(range(hum_grid)) / (length(hum_grid) - 1) * 1.03

bike_pdp_2d <- pdp_2d(
  bike_model,
  bike_pdp_sample %>% select(all_of(bike_features)),
  "temp",
  "hum",
  temp_grid,
  hum_grid
)

bike_pdp_2d_plot <- ggplot(bike_pdp_2d, aes(temp, hum, fill = prediction)) +
  geom_tile(width = tile_width, height = tile_height) +
  geom_rug(
    data = bike_pdp_sample,
    aes(temp, hum),
    inherit.aes = FALSE,
    sides = "bl",
    outside = TRUE,
    length = grid::unit(0.045, "npc"),
    linewidth = 0.25,
    alpha = 0.42,
    color = "#111111"
  ) +
  scale_fill_gradientn(
    colours = c("#4f3a2b", "#5d526b", "#638ce5", "#74a9ff"),
    labels = comma
  ) +
  scale_x_continuous(expand = expansion(mult = 0), breaks = pretty_breaks(5)) +
  scale_y_continuous(expand = expansion(mult = 0), breaks = pretty_breaks(5)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "2D Partial Dependence Plot: Temperature and Humidity",
    subtitle = "Random sample of 50 observations; heatmap style following the class slides",
    x = "Temperature (normalized)",
    y = "Humidity (normalized)",
    fill = expression(hat(y))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.height = grid::unit(1.05, "cm"),
    legend.key.width = grid::unit(0.45, "cm"),
    plot.margin = margin(8, 24, 24, 24)
  )

save_reference_plot(
  bike_pdp_2d_plot,
  "bike_pdp_2d_temperature_humidity_heatmap.png",
  width = 9,
  height = 6.5
)

ggsave(
  "outputs/figures/bike_pdp_temp_hum_2d.png",
  bike_pdp_2d_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# Exercise 3: house prices ---------------------------------------------------

houses <- read_csv("kc_house_data.csv", show_col_types = FALSE)

house_features <- c(
  "bedrooms",
  "bathrooms",
  "sqft_living",
  "sqft_lot",
  "floors",
  "yr_built"
)

house_model_data <- houses %>%
  select(price, all_of(house_features)) %>%
  drop_na() %>%
  filter(bedrooms <= 8)

house_train <- house_model_data %>%
  slice_sample(n = house_sample_size)

house_model <- ranger(
  price ~ .,
  data = as.data.frame(house_train),
  num.trees = 500,
  mtry = 3,
  min.node.size = 5,
  importance = "permutation",
  seed = 20260505
)

house_pdp_features <- c("bedrooms", "bathrooms", "sqft_living", "floors")
house_pdp_1d <- bind_rows(lapply(house_pdp_features, function(feature) {
  pdp_1d(
    house_model,
    house_train %>% select(all_of(house_features)),
    feature,
    grid_values(house_train[[feature]], n = 60)
  )
}))

house_distribution <- house_train %>%
  select(all_of(house_pdp_features)) %>%
  pivot_longer(everything(), names_to = "feature", values_to = "value")

house_feature_labels <- c(
  bedrooms = "Bedrooms",
  bathrooms = "Bathrooms",
  sqft_living = "Living area",
  floors = "Floors"
)

house_pdp_plot <- ggplot(house_pdp_1d, aes(value, prediction)) +
  geom_line(color = "#8c2d04", linewidth = 1) +
  geom_point(color = "#8c2d04", size = 1.2, alpha = 0.75) +
  geom_rug(
    data = house_distribution,
    aes(x = value),
    inherit.aes = FALSE,
    sides = "b",
    alpha = 0.18
  ) +
  facet_wrap(
    ~feature,
    scales = "free",
    labeller = as_labeller(house_feature_labels)
  ) +
  scale_y_continuous(labels = dollar) +
  labs(
    title = "House prices: one-dimensional partial dependence",
    subtitle = "Random forest trained and explained on a 1,500-house sample",
    x = NULL,
    y = "Predicted price"
  ) +
  theme_xai()

save_reference_plot(
  plot_pdp_1d_reference(
    house_pdp_1d,
    house_train,
    "bedrooms",
    "Partial Dependence of House Price on Bedrooms",
    "Bedrooms",
    "Predicted House price",
    y_scale = label_dollar()
  ),
  "house_pdp_bedrooms.png"
)

save_reference_plot(
  plot_pdp_1d_reference(
    house_pdp_1d,
    house_train,
    "bathrooms",
    "Partial Dependence of House Price on Bathrooms",
    "Bathrooms",
    "Predicted House price",
    y_scale = label_dollar()
  ),
  "house_pdp_bathrooms.png"
)

save_reference_plot(
  plot_pdp_1d_reference(
    house_pdp_1d,
    house_train,
    "sqft_living",
    "Partial Dependence of House Price on Living Area",
    "Living area (sqft)",
    "Predicted House price",
    y_scale = label_dollar()
  ),
  "house_pdp_sqft_living.png"
)

save_reference_plot(
  plot_pdp_1d_reference(
    house_pdp_1d,
    house_train,
    "floors",
    "Partial Dependence of House Price on Number of Floors",
    "Floors",
    "Predicted House price",
    y_scale = label_dollar()
  ),
  "house_pdp_floors.png"
)

ggsave(
  "outputs/figures/house_pdp_1d.png",
  house_pdp_plot,
  width = 10,
  height = 6,
  dpi = 300
)

# Tables ---------------------------------------------------------------------

metrics <- bind_rows(
  model_metrics(
    bike_model,
    "Bike rentals",
    "cnt",
    nrow(bike_train),
    nrow(bike_pdp_sample)
  ),
  model_metrics(
    house_model,
    "House prices",
    "price",
    nrow(house_train),
    nrow(house_train)
  )
)

importance <- bind_rows(
  importance_table(bike_model, "Bike rentals"),
  importance_table(house_model, "House prices")
)

pdp_summaries <- bind_rows(
  pdp_summary(bike_pdp_1d, "Bike rentals"),
  pdp_summary(house_pdp_1d, "House prices")
)

write_csv(metrics, "outputs/tables/model_metrics.csv")
write_csv(importance, "outputs/tables/feature_importance.csv")
write_csv(pdp_summaries, "outputs/tables/pdp_summary.csv")
write_csv(bike_pdp_1d, "outputs/tables/bike_pdp_1d.csv")
write_csv(bike_pdp_2d, "outputs/tables/bike_pdp_temp_hum_2d.csv")
write_csv(house_pdp_1d, "outputs/tables/house_pdp_1d.csv")

message("Analysis completed. Figures and tables written to outputs/.")

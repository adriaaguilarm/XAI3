# XAI3 - Model-agnostic PDP

Repository for the XAI3 exercise on model-agnostic interpretability with Partial Dependence Plots.

The analysis trains random forest regression models and explains their predictions with one-dimensional and two-dimensional PDPs:

- Bike rentals (`day.csv`), target `cnt`.
- House prices (`kc_house_data.csv`), target `price`.

The report is intentionally kept outside git until it is reviewed in Overleaf.

## Reproduce

Required R packages:

- `dplyr`
- `ggplot2`
- `patchwork`
- `ranger`
- `readr`
- `scales`
- `tidyr`
- `viridis`

```bash
Rscript src/01_pdp_random_forest.R
```

Generated figures are written to `outputs/figures/` and tabular summaries to `outputs/tables/`.

## Notes

- The bike rental model is trained with all daily records and its PDP values are computed on exactly 50 sampled observations.
- The house price model is trained and explained on a random sample of 1,500 observations, following the class indication to use 1,000-2,000 records for this part.
- The two-dimensional PDP uses `geom_tile()` and shows sampled feature distributions as marginal rugs, matching the class-slide style.
- Development was done on the branch `analysis/pdp-random-forest` and then integrated into `main`.

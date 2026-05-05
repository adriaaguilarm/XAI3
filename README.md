# XAI3 - Model-agnostic PDP

Repository for the XAI3 exercise on model-agnostic interpretability with Partial Dependence Plots.

The analysis trains random forest regression models and explains their predictions with one-dimensional and two-dimensional PDPs:

- Bike rentals (`day.csv`), target `cnt`.
- House prices (`kc_house_data.csv`), target `price`.

The report is intentionally kept outside git until it is reviewed in Overleaf.

## Reproduce

```bash
Rscript src/01_pdp_random_forest.R
```

Generated figures are written to `outputs/figures/` and tabular summaries to `outputs/tables/`.


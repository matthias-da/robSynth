# Build a column-aligned design matrix for xgboost on new data

Coerces unseen factor levels to NA (so their dummies are zero), drops
any columns not present at training time, and pads missing training
columns with zeros. Guarantees the predict-time matrix has the same
columns in the same order as the fit-time matrix.

## Usage

``` r
.align_xgb_matrix(X_new, xlev, train_cols)
```

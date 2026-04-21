# Fit robust XGBoost for synthesis

Uses XGBoost with pseudo-Huber loss (`reg:pseudohubererror`) for
continuous variables, providing bounded influence on the loss function.
For categorical variables, uses `multi:softprob` with sample weights.

## Usage

``` r
.fit_robust_xgb(
  y,
  X,
  var_type,
  obs_weights = NULL,
  huber_delta = 1,
  nrounds = 100L,
  ...
)
```

## Arguments

- y:

  numeric or factor response

- X:

  data.frame of predictors

- var_type:

  "continuous" or "categorical"

- obs_weights:

  observation weights

- huber_delta:

  Huber loss delta parameter (default 1.0). Smaller = more robust,
  larger = more efficient.

- nrounds:

  number of boosting rounds (default 100)

- ...:

  passed to xgb.train

## Value

fit object

# Fit robust random forest for synthesis

Uses ranger with case weights derived from robust Mahalanobis distances.
Observations far from the robust center receive lower weight, reducing
the influence of y-outliers on leaf predictions.

## Usage

``` r
.fit_robust_rf(y, X, var_type, obs_weights = NULL, ...)
```

## Arguments

- y:

  numeric or factor response

- X:

  data.frame of predictors

- var_type:

  "continuous" or "categorical"

- obs_weights:

  observation weights (combined survey + robustness)

- ...:

  passed to ranger

## Value

fit object

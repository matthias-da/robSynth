# Fit robust CART for synthesis

Fits an rpart tree but replaces leaf-node means with medians (robust to
y-outliers). Residual scale uses MAD instead of SD.

## Usage

``` r
.fit_robust_cart(y, X, var_type, obs_weights = NULL, ...)
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

- ...:

  passed to rpart

## Value

fit object compatible with .generate_dispatch

# Predictive mean matching draw

Finds the `donors` closest fitted values to the target prediction and
randomly selects one donor's observed value.

## Usage

``` r
pmm_draw(y_obs, yhat_obs, yhat_new, donors = 5L)
```

## Arguments

- y_obs:

  observed response values (numeric)

- yhat_obs:

  fitted values for observed data (same length as `y_obs`)

- yhat_new:

  fitted values for new / synthetic observations

- donors:

  number of candidate donors. Default: 5.

## Value

numeric vector of length `length(yhat_new)`

## Author

Matthias Templ

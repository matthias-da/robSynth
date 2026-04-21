# Weighted median

Computes the weighted median of a numeric vector using sorted cumulative
weights.

## Usage

``` r
weighted_median(x, w, na.rm = TRUE)
```

## Arguments

- x:

  numeric vector

- w:

  numeric vector of non-negative weights (same length as `x`)

- na.rm:

  logical; if `TRUE`, remove NAs. Default: TRUE.

## Value

numeric scalar

## Author

Matthias Templ

## Examples

``` r
weighted_median(1:10, w = c(rep(1, 5), rep(0.1, 5)))
#> [1] 3
```

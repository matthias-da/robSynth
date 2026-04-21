# Weighted MAD (median absolute deviation)

Computes the weighted median absolute deviation, using
[`weighted_median`](https://matthias-da.github.io/robSynth/reference/weighted_median.md)
for both the center and the deviations. Consistency factor of 1.4826 is
applied for normal data.

## Usage

``` r
weighted_mad(x, w, constant = 1.4826, na.rm = TRUE)
```

## Arguments

- x:

  numeric vector

- w:

  numeric vector of non-negative weights (same length as `x`)

- constant:

  scale factor for asymptotic consistency at the normal. Default:
  1.4826.

- na.rm:

  logical; remove NAs. Default: TRUE.

## Value

positive numeric scalar

## Author

Matthias Templ

## Examples

``` r
set.seed(1)
x <- c(rnorm(100), 10)
weighted_mad(x, w = c(rep(1, 100), 0.01))
#> [1] 0.8844379
```

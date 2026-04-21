# Robust Box-Cox lambda estimation

Estimates the Box-Cox transformation parameter by minimising a robust
skewness criterion (absolute difference of mean and median, divided by
MAD) over a grid of lambda values.

## Usage

``` r
robust_boxcox_lambda(y, lambda_grid = seq(-1, 2, by = 0.05))
```

## Arguments

- y:

  numeric vector (must be strictly positive)

- lambda_grid:

  grid of lambda values to search

## Value

scalar: estimated lambda

## References

Box G. E. P., Cox D. R. (1964). An analysis of transformations. *Journal
of the Royal Statistical Society: Series B*, 26(2), 211-252.

Marazzi A., Yohai V. J. (2006). Robust Box-Cox transformations based on
minimum residual autocorrelation. *Computational Statistics & Data
Analysis*, 50(10), 2752-2768.

## Examples

``` r
set.seed(1); x <- rlnorm(200)
robust_boxcox_lambda(x)  # should be near 0 (log)
#> [1] -0.2
```

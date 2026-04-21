# Generate AR(1) covariance matrix

Constructs a \\p \times p\\ covariance matrix with AR(1) structure
\\\Sigma\_{jk} = \rho^{\|j-k\|}\\. Useful for simulation studies.

## Usage

``` r
generate_ar1_cov(p, rho)
```

## Arguments

- p:

  integer; dimension

- rho:

  numeric; autocorrelation parameter in \\(-1, 1)\\

## Value

\\p \times p\\ positive definite matrix

## Author

Matthias Templ

## Examples

``` r
generate_ar1_cov(5, 0.7)
#>        [,1]  [,2] [,3]  [,4]   [,5]
#> [1,] 1.0000 0.700 0.49 0.343 0.2401
#> [2,] 0.7000 1.000 0.70 0.490 0.3430
#> [3,] 0.4900 0.700 1.00 0.700 0.4900
#> [4,] 0.3430 0.490 0.70 1.000 0.7000
#> [5,] 0.2401 0.343 0.49 0.700 1.0000
```

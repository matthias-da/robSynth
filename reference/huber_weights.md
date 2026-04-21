# Huber weights for standardised values

Computes Huber weights: 1 for values within the tuning constant and
`k / abs(x)` for values outside. Standardisation (centering by median,
scaling by MAD) is applied internally.

## Usage

``` r
huber_weights(x, k = 1.345)
```

## Arguments

- x:

  numeric vector (raw, unstandardised)

- k:

  tuning constant, Default: 1.345

## Value

numeric vector of weights in \\\[0, 1\]\\

## Author

Matthias Templ

## Examples

``` r
set.seed(1)
x <- c(rnorm(100), 10, -8)
w <- huber_weights(x)
plot(x, w)
```

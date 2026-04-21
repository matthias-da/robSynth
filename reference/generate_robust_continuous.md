# Generate synthetic continuous values from a robust fit

Draws from the fitted robust model with added residual noise. Supports
both direct drawing (normal residuals) and predictive mean matching
(PMM).

## Usage

``` r
generate_robust_continuous(
  fit,
  X_new,
  uncertainty = c("normal", "pmm"),
  donors = 5L
)
```

## Arguments

- fit:

  object of class `"robsynth_fit_cont"` as returned by
  [`fit_robust_continuous`](https://matthias-da.github.io/robSynth/reference/fit_robust_continuous.md)

- X_new:

  data.frame of predictors (from synthetic data)

- uncertainty:

  how to introduce synthesis noise: `"normal"` (default) adds \\N(0,
  \hat\sigma^2)\\; `"pmm"` uses predictive mean matching.

- donors:

  number of PMM donors. Default: 5.

## Value

numeric vector of synthetic values

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_robust_continuous(iris$Sepal.Width,
                             iris[, "Sepal.Length", drop = FALSE])
y_new <- generate_robust_continuous(fit, iris[, "Sepal.Length", drop = FALSE])
} # }
```

# Generate synthetic categorical values from a robust fit

Draws from predicted class probabilities of the fitted weighted
multinomial model.

## Usage

``` r
generate_robust_categorical(fit, X_new)
```

## Arguments

- fit:

  object of class `"robsynth_fit_cat"` as returned by
  [`fit_robust_categorical`](https://matthias-da.github.io/robSynth/reference/fit_robust_categorical.md)

- X_new:

  data.frame of predictors (from synthetic data)

## Value

factor vector of synthetic values

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- fit_robust_categorical(iris$Species,
                              iris[, 1:2, drop = FALSE])
y_new <- generate_robust_categorical(fit, iris[, 1:2, drop = FALSE])
table(y_new)
} # }
```

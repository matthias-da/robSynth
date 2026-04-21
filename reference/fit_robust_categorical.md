# Fit a robust model for a categorical variable

Fits a weighted multinomial logistic regression for categorical response
synthesis. Uses
[`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html) with
observation-level weights derived from cell-level contamination weights.

## Usage

``` r
fit_robust_categorical(y, X, weights = NULL, ...)
```

## Arguments

- y:

  factor or character response vector

- X:

  data.frame of predictors

- weights:

  optional cell-level weights (same structure as in
  [`fit_robust_continuous`](https://matthias-da.github.io/robSynth/reference/fit_robust_continuous.md))

- ...:

  additional arguments passed to
  [`nnet::multinom`](https://rdrr.io/pkg/nnet/man/multinom.html)

## Value

A list of class `"robsynth_fit_cat"` with components:

- model:

  the fitted multinom object

- levels:

  factor levels of the response

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
data(CrohnD, package = "robustbase")
fit <- fit_robust_categorical(CrohnD$treat,
                              CrohnD[, c("BMI", "age"), drop = FALSE])
} # }
```

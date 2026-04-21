# Compare model fit between original and synthetic data

Fits the same regression model on original and synthetic data and
compares coefficient estimates. When `m > 1`, applies Reiter (2003)
combining rules. Optionally produces a forest plot.

## Usage

``` r
compare_fit(formula, object, original, plot = TRUE, ...)
```

## Arguments

- formula:

  a model formula

- object:

  a `"robsynth"` object

- original:

  the original data.frame

- plot:

  logical: produce a coefficient comparison plot? Default TRUE.

- ...:

  additional arguments passed to `lm`

## Value

An S3 object of class `"robsynth_fit_compare"` with:

- coef_orig:

  coefficients from original data

- coef_synth:

  coefficients from synthetic data (or combined)

- se_orig:

  standard errors from original

- se_synth:

  standard errors from synthetic (or combined)

- ci_overlap:

  per-coefficient CI overlap

- std_diff:

  standardised differences

- mean_abs_std_diff:

  mean absolute standardised difference

- plot:

  ggplot object (if requested)

## Examples

``` r
if (FALSE) { # \dontrun{
data(CrohnD, package = "robustbase")
dat <- CrohnD[, -1]
res <- robsynth(dat, m = 5)
comp <- compare_fit(BMI ~ age + height + weight, res, dat)
comp$plot
} # }
```

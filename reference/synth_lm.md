# Fit a linear model on synthetic data

When `m > 1`, fits the model on each synthetic copy and combines
estimates using Reiter's (2003) partially synthetic data combining
rules.

## Usage

``` r
synth_lm(formula, object, ...)
```

## Arguments

- formula:

  a model formula

- object:

  a `"robsynth"` object

- ...:

  additional arguments passed to `lm`

## Value

A list with components:

- coefficients:

  combined point estimates

- se:

  combined standard errors

- t:

  t-statistics

- df:

  degrees of freedom

- individual_fits:

  list of `lm` objects (one per copy)

## References

Reiter J. P. (2003). Inference for partially synthetic, public use
microdata sets. *Survey Methodology*, 29(2), 181-188.

## Examples

``` r
if (FALSE) { # \dontrun{
data(CrohnD, package = "robustbase")
res <- robsynth(CrohnD[, -1], m = 5)
fit <- synth_lm(BMI ~ age + height + weight, res)
fit$coefficients
} # }
```

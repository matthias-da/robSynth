# Fit a GLM on synthetic data

Fit a GLM on synthetic data

## Usage

``` r
synth_glm(formula, object, family = stats::gaussian(), ...)
```

## Arguments

- formula:

  a model formula

- object:

  a `"robsynth"` object

- family:

  a family object (e.g.,
  [`binomial()`](https://rdrr.io/r/stats/family.html))

- ...:

  additional arguments passed to `glm`

## Value

same structure as
[`synth_lm`](https://matthias-da.github.io/robSynth/reference/synth_lm.md)

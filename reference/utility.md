# Utility assessment for synthetic data

Unified interface to compute all utility metrics comparing original and
synthetic data. Handles multiple synthetic copies (`m > 1`) by averaging
across copies.

## Usage

``` r
utility(object, original = NULL, metrics = NULL, ...)
```

## Arguments

- object:

  a `"robsynth"` object, or a synthetic data.frame

- original:

  the original data.frame (required if `object` is a data.frame,
  optional if `object` is a robsynth object with `$original` stored)

- metrics:

  character vector of metrics to compute. Options: `"pMSE"`, `"KS"`,
  `"regression"`, `"correlation"`, `"overlap"`. Default: all.

- ...:

  additional arguments passed to metric functions

## Value

An S3 object of class `"robsynth_utility"` with components for each
metric and a formatted print method.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- robsynth(iris)
utility(res, iris)
} # }
```

# Synthetic data risk metrics

Computes disclosure risk metrics for synthetic data. Risk measures how
much an attacker could learn about original records from the synthetic
data.

## Usage

``` r
synth_risk(
  orig,
  synth,
  metrics = c("k_anonymity", "distance", "membership_inference", "TCAP"),
  key_vars = NULL,
  sensitive_var = NULL
)
```

## Arguments

- orig:

  original data.frame

- synth:

  synthetic data.frame (same columns as `orig`)

- metrics:

  character vector of metrics to compute. Default: all available.
  Options: `"k_anonymity"`, `"distance"`, `"membership_inference"`,
  `"TCAP"`.

- key_vars:

  character vector of key (quasi-identifier) variable names. Default:
  all variables.

- sensitive_var:

  name of the sensitive variable (for TCAP). Default: last column.

## Value

A list of class `"synth_risk"` with one component per requested metric:

- k_anonymity:

  list with `synth_only` (k-anonymity on synthetic data alone) and
  `cross_match` (minimum equivalence class size when matching original
  to synthetic; Issue 14)

- distance:

  distribution of closest-record distances between original and
  synthetic

- membership_inference:

  AUC of member vs. non-member classification (Issue 15)

- TCAP:

  Target Correct Attribution Probability (Issue 15)

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
data(iris)
synth <- iris[sample(nrow(iris), replace = TRUE), ]
synth_risk(iris, synth)
} # }
```

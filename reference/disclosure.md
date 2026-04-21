# Disclosure risk assessment

Computes one or more disclosure-risk measures for synthetic data. When
the riskutility package is installed the following measures are
available:

- `"rapid"`:

  Risk of Attribute Prediction-Induced Disclosure (Thees, Mueller &
  Templ, 2025): trains a predictive attacker on the synthetic data and
  scores its success on the original.

- `"dcap"`:

  Differential Correct Attribution Probability — exact- or
  Gower-matching attack on the key variables with sensitive attribute
  recovery.

- `"dcr"`:

  Distance to Closest Record — per-record nearest distance with a
  holdout null.

- `"delta_presence"`:

  \\\delta\\-presence: the proportion of original records whose
  inclusion can be inferred.

- `"domias"`:

  Density-based membership inference attack: AUC of train-vs-holdout
  separation by density ratio.

- `"drisk"`:

  sdcMicro-style disclosure risk on key-vars (interval /
  robust-Mahalanobis).

The built-in `"distance"` measure is a simple robust-MAD nearest-
neighbour distance that does not require riskutility. The special value
`"all"` expands to every available measure whose dependencies are
satisfied.

## Usage

``` r
disclosure(
  object,
  original,
  key_vars,
  target_var,
  method = c("rapid", "distance"),
  ...
)
```

## Arguments

- object:

  a `"robsynth"` object or synthetic data.frame

- original:

  the original data.frame

- key_vars:

  character vector of quasi-identifier column names (required by
  `rapid`, `dcap`, `delta_presence`, `drisk`; ignored by the others)

- target_var:

  name of the sensitive attribute (required by `rapid`, `dcap`)

- method:

  character vector of risk measures to compute; see Details. Also
  accepts `"both"` (alias for `c("rapid", "distance")`) and `"all"`.

- ...:

  additional arguments forwarded to the underlying riskutility function.
  When several riskutility measures are requested, `...` arguments are
  passed to every call; use a single-method call for per-function
  argument control.

## Value

An S3 object of class `"robsynth_disclosure"` with one list component
per requested measure plus a print method.

## References

Thees M., Mueller J., Templ M. (2025). RAPID: A random-forest attack for
disclosure-risk assessment of synthetic data. arXiv:2602.09235.

## Examples

``` r
if (FALSE) { # \dontrun{
res <- robsynth(iris)
disclosure(res, iris,
  key_vars = c("Sepal.Width", "Petal.Length"),
  target_var = "Sepal.Length",
  method = c("rapid", "dcap", "domias"))
} # }
```

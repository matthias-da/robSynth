# Synthesise from estimated clean distribution

Given the contamination weights from
[`estimate_contamination`](https://matthias-da.github.io/robSynth/reference/estimate_contamination.md),
draws synthetic records from the estimated clean distribution — either a
multivariate normal fitted from weighted moments (`"parametric"`) or a
weighted resample of the observed rows (`"weighted_resample"`).

## Usage

``` r
synth_from_clean(
  data,
  clean_probs,
  method = c("parametric", "weighted_resample"),
  n_synth = nrow(data),
  seed = NULL
)
```

## Arguments

- data:

  original data.frame

- clean_probs:

  n x p matrix of posterior clean probabilities (from
  [`estimate_contamination`](https://matthias-da.github.io/robSynth/reference/estimate_contamination.md))

- method:

  synthesis method for the clean distribution: `"parametric"` (default)
  draws from the estimated normal; `"weighted_resample"` resamples with
  clean-probability weights.

- n_synth:

  number of synthetic records. Default: `nrow(data)`.

- seed:

  random seed.

## Value

a synthetic data.frame

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
data(Animals2, package = "robustbase")
X      <- log(Animals2)
contam <- estimate_contamination(X, method = "cellwise")
synth  <- synth_from_clean(X, contam$clean_probs)
} # }
```

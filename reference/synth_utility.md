# Synthetic data utility metrics

Computes utility metrics comparing original and synthetic data. Utility
measures how well the synthetic data preserves statistical properties of
the original.

## Usage

``` r
synth_utility(
  orig,
  synth,
  metrics = c("pMSE", "KS", "regression", "correlation", "overlap")
)
```

## Arguments

- orig:

  original data.frame

- synth:

  synthetic data.frame (same columns as `orig`)

- metrics:

  character vector of metrics to compute. Default: all available.
  Options: `"pMSE"`, `"KS"`, `"regression"`, `"correlation"`,
  `"overlap"`.

## Value

A list of class `"synth_utility"` with one component per requested
metric:

- pMSE:

  propensity score MSE (lower = more similar)

- KS:

  per-variable Kolmogorov-Smirnov statistic (continuous only)

- regression:

  comparison of regression coefficients (relative bias and confidence
  interval overlap)

- correlation:

  Frobenius norm distance between correlation matrices

- overlap:

  per-variable empirical distribution overlap

## Details

The **pMSE** (propensity score mean squared error) trains a logistic
regression to distinguish original from synthetic records. If the
synthetic data is good, the classifier cannot distinguish them, and pMSE
approaches \\c(1-c)/n\\ where \\c = n\_\text{synth} / (n\_\text{orig} +
n\_\text{synth})\\. The raw value is returned here; for
cross-sample-size comparison use the null-scaled pMSE of Snoke and
Slavkovic (2018), \\pMSE / \mathrm{Var}(pMSE \mid H_0)\\.

## References

Snoke J., Slavkovic A. (2018). pMSE mechanism: Differentially private
synthetic data with maximal distributional similarity. In *Privacy in
Statistical Databases*, 138-159. Springer.

Reiter J. P. (2005). Using CART to generate partially synthetic public
use microdata. *Journal of Official Statistics*, 21(3), 441-462.

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
data(iris)
synth <- iris[sample(nrow(iris), replace = TRUE), ]
synth_utility(iris, synth)
} # }
```

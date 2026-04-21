# Estimate contamination model

Estimates cellwise or rowwise contamination probabilities from the
original data. Returns posterior clean probabilities that can be used as
diagnostic weights or as observation weights in downstream fitting.

## Usage

``` r
estimate_contamination(
  data,
  method = c("cellwise", "rowwise"),
  eps_init = 0.1,
  gamma_init = 3,
  maxit = 100L,
  tol = 1e-04,
  alpha = 0.75
)
```

## Arguments

- data:

  a data.frame

- method:

  `"cellwise"` (default) estimates per-cell posterior clean
  probabilities; `"rowwise"` uses MCD-based Mahalanobis distances to
  flag entire rows.

- eps_init:

  initial contamination probability per variable. Default: 0.1. Must be
  in \\(0, 0.5)\\.

- gamma_init:

  initial scale inflation factor for the contamination distribution.
  Default: 3.

- maxit:

  maximum EM iterations. Default: 100.

- tol:

  convergence tolerance on relative parameter change. Default: 1e-4.

- alpha:

  fraction of data for MCD (rowwise only). Default: 0.75.

## Value

A list with components:

- clean_probs:

  n x p matrix of posterior clean probabilities (values in \\\[0,
  1\]\\). Categorical columns have weight 1.

- epsilon:

  named numeric vector of estimated per-variable contamination rates
  (continuous only)

- method:

  the method used

- converged:

  logical

- iterations:

  number of EM iterations performed

## Details

**Cellwise method.** For each continuous variable \\j\\, the observed
value is modeled as a two-component mixture: \$\$(1 - \varepsilon_j)
\cdot N(\mu_j, \sigma_j^2) + \varepsilon_j \cdot N(\mu_j, (\gamma_j
\sigma_j)^2)\$\$ The EM algorithm iterates between computing posterior
clean probabilities (E-step) and updating parameters (M-step).

**Important (Issue 20):** Cellwise z-scores use \\z_j = (x -
\mathrm{med}) / \mathrm{mad}\\, NOT \\\|x - \mathrm{med}\| /
\mathrm{mad}\\. The absolute value folds the distribution and makes
`dnorm(z_j)` incorrect.

**Rowwise method.** Uses robust Mahalanobis distances from
[`robustbase::covMcd`](https://rdrr.io/pkg/robustbase/man/covMcd.html)
and converts to row-level clean probabilities via a chi-squared tail
probability.

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
data(iris)
iris_c <- iris[, 1:4]
iris_c[1:5, 1] <- iris_c[1:5, 1] + 10
res <- estimate_contamination(iris_c, method = "cellwise")
image(res$clean_probs, main = "Clean probabilities")
} # }
```

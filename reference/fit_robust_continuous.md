# Fit a robust model for a continuous variable

Fits a robust regression model for sequential conditional synthesis.
MM-estimation
([`robustbase::lmrob`](https://rdrr.io/pkg/robustbase/man/lmrob.html))
is the default. When `method = "robust_cart"`, a robust CART model is
fitted via
[`partykit::ctree`](https://rdrr.io/pkg/partykit/man/ctree.html) (if
available) or [`rpart::rpart`](https://rdrr.io/pkg/rpart/man/rpart.html)
with Huber-weighted splits as fallback.

## Usage

``` r
fit_robust_continuous(
  y,
  X,
  method = c("mm", "robust_cart"),
  weights = NULL,
  ...
)
```

## Arguments

- y:

  numeric response vector

- X:

  data.frame of predictors (already synthesised variables)

- method:

  `"mm"` (default) or `"robust_cart"`

- weights:

  optional cell-level weights: a list with components `w_response`
  (length `n`) and `w_predictors` (n x p_pred matrix) coming from
  [`estimate_contamination`](https://matthias-da.github.io/robSynth/reference/estimate_contamination.md).
  Leave as `NULL` to use lmrob's built-in M-weights only.

- ...:

  additional arguments passed to `lmrob` or `rpart`/`ctree`

## Value

A list of class `"robsynth_fit_cont"` with components:

- model:

  the fitted model object

- method:

  the fitting method used

- sigma:

  robust residual scale estimate

- y_obs:

  observed response (for PMM)

- yhat_obs:

  fitted values (for PMM)

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
data(CrohnD, package = "robustbase")
fit <- fit_robust_continuous(CrohnD$BMI,
                             CrohnD[, c("age", "height"), drop = FALSE])
} # }
```

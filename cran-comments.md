# CRAN submission: robSynth 0.5.0

## Release summary

First CRAN submission of `robSynth`, an R package for robust
generation of synthetic microdata that protects downstream analyses
from contamination in the training data.  The core methodological
contribution is the replacement of ordinary least squares /
maximum-likelihood logistic regression in sequential conditional
synthesis by MM-estimators (continuous variables) and Huber-weighted
logistic regression (categorical variables), so that outliers and
coding errors in the training register do not propagate into the
synthetic output.

## Test environments

- local: macOS aarch64, R 4.5.2 release
- GitHub Actions (via the standard `check-standard` workflow):
  - macOS-latest, R release
  - windows-latest, R release
  - ubuntu-latest, R devel / release / oldrel-1

## R CMD check results

```
0 errors | 0 warnings | 0 notes
```

## Reverse dependencies

This is a first submission; no reverse dependencies yet.

## Package dependencies

Imports: `robustbase`, `nnet`, `rpart`, `stats`, `MASS`.

Suggests (all guarded via `requireNamespace()`): `partykit`, `ranger`,
`xgboost`, `riskutility`, `synthpop`, `ggplot2`, `gridExtra`,
`tinytest`, `knitr`, `rmarkdown`.

## Additional notes

- License: `GPL (>= 2)` (standard; no separate LICENSE file needed).
- Vignette (`robSynth-intro.Rmd`) builds in under 10 seconds on the
  reference platform.
- 146 `tinytest` tests; all pass.
- The companion paper describing the method is under review at
  Journal of Applied Statistics.

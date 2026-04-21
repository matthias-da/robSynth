# robSynth <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/matthias-da/robSynth/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/matthias-da/robSynth/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/matthias-da/robSynth/actions/workflows/pkgdown.yaml/badge.svg)](https://matthias-da.github.io/robSynth/)
[![License: GPL v2](https://img.shields.io/badge/License-GPL%20v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.en.html)
<!-- badges: end -->

**Robust Synthetic Data Generation** -- protecting data quality from
contaminated training data.

Real-world microdata often contain outliers, coding errors, or
measurement artifacts. Standard synthesis methods (OLS, CART) propagate
these errors into the synthetic output, degrading utility and
inflating disclosure risk. robSynth replaces the conditional models in
sequential synthesis with MM-estimators for continuous variables (50%
breakdown point, 95% asymptotic efficiency) and Huber-weighted logistic
regression for categorical variables. The result is synthetic data
whose statistical properties are close to the *clean* data-generating
process, even when the training data are contaminated.

## Installation

```r
# install.packages("remotes")
remotes::install_github("statistikat/robSynth")
```

## Quick example

```r
library(robSynth)

# Contaminate some iris records
iris_c <- iris
iris_c[1:5, 1:4] <- iris_c[1:5, 1:4] + 10

# Robust synthesis
res <- robsynth(iris_c, method = "robust_conditional", seed = 42)
res

# Utility assessment
u <- synth_utility(iris, res$synth)
u$pMSE
u$correlation

# Risk assessment
r <- synth_risk(iris, res$synth)
r$TCAP
```

## Key features

- **Robust MM synthesis** -- MM-estimation via `robustbase::lmrob` for
  continuous variables; weighted multinomial logit for categorical
  variables.
- **Contamination detection** -- optional cellwise contamination
  filtering (`method = "contam_filter"`) downweights suspicious cells
  before synthesis.
- **Utility metrics** -- propensity score MSE (pMSE), Kolmogorov-Smirnov
  statistics, regression coefficient preservation, correlation matrix
  distance, and empirical distribution overlap.
- **Risk metrics** -- k-anonymity, closest-record distance,
  membership inference AUC, and Target Correct Attribution Probability
  (TCAP).
- **Risk-utility frontier** -- `risk_utility_frontier()` compares
  multiple synthesizers on the Duncan-Lambert R-U confidentiality map
  and identifies Pareto-optimal methods.

## Comparison with synthpop

robSynth adds robustness to the sequential synthesis framework
popularized by [synthpop](https://CRAN.R-project.org/package=synthpop).
Under contamination, MM-based synthesis reduces pMSE by 44--59%
compared to non-robust methods (OLS, CART), while maintaining
comparable disclosure risk levels.

## Citation

If you use robSynth in your work, please cite:

> Templ, M. (2026). Robust synthetic data generation: protecting data
> quality from contaminated training data. *Working paper*.

## License

GPL (>= 2). See [LICENSE](LICENSE) for details.

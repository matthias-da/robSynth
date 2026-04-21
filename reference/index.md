# Package index

## Synthesis

Main entry points for generating synthetic data.

- [`robsynth()`](https://matthias-da.github.io/robSynth/reference/robsynth.md)
  : Robust synthetic data generation
- [`robsynth_hh()`](https://matthias-da.github.io/robSynth/reference/robsynth_hh.md)
  : Robust synthesis for hierarchical (household/person) data
- [`synth_lm()`](https://matthias-da.github.io/robSynth/reference/synth_lm.md)
  : Fit a linear model on synthetic data
- [`synth_glm()`](https://matthias-da.github.io/robSynth/reference/synth_glm.md)
  : Fit a GLM on synthetic data

## Robust conditional models

Low-level fit and generate functions used by the synthesiser.

- [`fit_robust_continuous()`](https://matthias-da.github.io/robSynth/reference/fit_robust_continuous.md)
  : Fit a robust model for a continuous variable
- [`fit_robust_categorical()`](https://matthias-da.github.io/robSynth/reference/fit_robust_categorical.md)
  : Fit a robust model for a categorical variable
- [`generate_robust_continuous()`](https://matthias-da.github.io/robSynth/reference/generate_robust_continuous.md)
  : Generate synthetic continuous values from a robust fit
- [`generate_robust_categorical()`](https://matthias-da.github.io/robSynth/reference/generate_robust_categorical.md)
  : Generate synthetic categorical values from a robust fit
- [`robust_boxcox_lambda()`](https://matthias-da.github.io/robSynth/reference/robust_boxcox_lambda.md)
  : Robust Box-Cox lambda estimation

## Utility diagnostics

- [`synth_utility()`](https://matthias-da.github.io/robSynth/reference/synth_utility.md)
  : Synthetic data utility metrics
- [`utility()`](https://matthias-da.github.io/robSynth/reference/utility.md)
  : Utility assessment for synthetic data
- [`compare()`](https://matthias-da.github.io/robSynth/reference/compare.md)
  : Compare original and synthetic data visually
- [`compare_fit()`](https://matthias-da.github.io/robSynth/reference/compare_fit.md)
  : Compare model fit between original and synthetic data

## Disclosure risk

- [`synth_risk()`](https://matthias-da.github.io/robSynth/reference/synth_risk.md)
  : Synthetic data risk metrics
- [`disclosure()`](https://matthias-da.github.io/robSynth/reference/disclosure.md)
  : Disclosure risk assessment
- [`risk_utility_frontier()`](https://matthias-da.github.io/robSynth/reference/risk_utility_frontier.md)
  : Risk-utility frontier analysis
- [`estimate_contamination()`](https://matthias-da.github.io/robSynth/reference/estimate_contamination.md)
  : Estimate contamination model
- [`synth_from_clean()`](https://matthias-da.github.io/robSynth/reference/synth_from_clean.md)
  : Synthesise from estimated clean distribution

## Helpers

- [`detect_var_types()`](https://matthias-da.github.io/robSynth/reference/detect_var_types.md)
  : Detect variable types in a data.frame
- [`weighted_median()`](https://matthias-da.github.io/robSynth/reference/weighted_median.md)
  : Weighted median
- [`weighted_mad()`](https://matthias-da.github.io/robSynth/reference/weighted_mad.md)
  : Weighted MAD (median absolute deviation)
- [`huber_weights()`](https://matthias-da.github.io/robSynth/reference/huber_weights.md)
  : Huber weights for standardised values
- [`strip_diagnostics()`](https://matthias-da.github.io/robSynth/reference/strip_diagnostics.md)
  : Strip diagnostics from a robsynth result
- [`generate_ar1_cov()`](https://matthias-da.github.io/robSynth/reference/generate_ar1_cov.md)
  : Generate AR(1) covariance matrix
- [`pmm_draw()`](https://matthias-da.github.io/robSynth/reference/pmm_draw.md)
  : Predictive mean matching draw

## S3 methods (print, plot, summary)

- [`print(`*`<robsynth>`*`)`](https://matthias-da.github.io/robSynth/reference/print.robsynth.md)
  : Print method for robsynth objects
- [`print(`*`<robsynth_hh>`*`)`](https://matthias-da.github.io/robSynth/reference/print.robsynth_hh.md)
  : Print method for robsynth_hh objects
- [`print(`*`<ru_frontier>`*`)`](https://matthias-da.github.io/robSynth/reference/print.ru_frontier.md)
  : Print method for ru_frontier
- [`summary(`*`<robsynth>`*`)`](https://matthias-da.github.io/robSynth/reference/summary.robsynth.md)
  : Summary method for robsynth objects
- [`plot(`*`<robsynth>`*`)`](https://matthias-da.github.io/robSynth/reference/plot.robsynth.md)
  : Plot a robsynth object

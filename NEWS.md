# robSynth 0.5.0

## Nonparametric robust methods (Phase 5)

* New methods: `"robust_cart"` (median-leaf CART), `"robust_rf"`
  (ranger with OOB-residual-based case weights), `"robust_xgboost"`
  (pseudo-Huber loss).
* `robust_rf` is a two-pass estimator: a preliminary forest produces
  OOB residuals, rows with residual z-score > 3 are downweighted, and
  the forest is refit.
* `robust_xgboost` stores `xlev` and training column names so new-data
  design matrices are aligned, preventing silent garbage predictions
  when factor levels differ between train and predict.

# robSynth 0.4.0

## Diagnostics & reporting (Phase 4)

* `utility()` S3 generic with methods for `"robsynth"` and
  `"data.frame"`: returns pMSE, KS, regression coefficient
  preservation, correlation-matrix distance, and distribution overlap.
* `compare_fit()` compares a model fitted on original data to the same
  model fitted on synthetic data, returning coefficient and standard-
  error differences plus Reiter-pooled CIs for `m > 1`.
* `disclosure()` unified disclosure-risk entry point: k-anonymity,
  closest-record distance, membership inference AUC, TCAP, and
  RAPID (via optional `riskutility`).
* `compare()` and `plot()` S3 methods for side-by-side marginal and
  correlation diagnostics.

# robSynth 0.3.0

## Survey & hierarchical support (Phase 3)

* `weights =` argument for survey-weighted synthesis (accepts a
  variable name in `data` or a numeric vector).
* `robsynth_hh()` for two-phase hierarchical synthesis: Phase 1
  resamples households with replacement; Phase 2 synthesises
  person-level variables using models fit on the **original** data.
* `n_cores` argument for parallel synthesis across the `m > 1` draws
  via `parallel::parLapply`.  Per-draw seeds make the parallel and
  sequential paths produce identical output when `seed` is set.

# robSynth 0.2.0

## synthpop parity (Phase 2)

* Per-variable method vector via `method = c(var = "mm", ...)`.
* `predictor.matrix` for user-controlled conditioning.
* `formulas` for custom formulae (including interactions and passive
  relationships).
* `m > 1` multiple synthesis with Reiter (2003) combining rules in
  `synth_lm()` and `synth_glm()`.
* `transform = list(var = "log" | "sqrt" | "boxcox")` with
  `robust_boxcox_lambda()` — a robust Box-Cox that minimises a
  mean-vs-median skewness proxy rather than the standard MLE.
* `cont.na` argument for recoding sentinel values to `NA` before
  synthesis (applied *before* variable-type detection so that scale
  estimates are not contaminated by sentinel codes).
* `"passive"` method for deterministic transformations of already-
  synthesised variables.

# robSynth 0.1.0

* Initial release.
* `robsynth()`: sequential synthesis with MM-estimation (continuous) and
  weighted multinomial logit (categorical).
* Utility metrics via `synth_utility()`: pMSE, KS, regression
  preservation, correlation matrix distance, distribution overlap.
* Risk metrics via `synth_risk()`: k-anonymity, closest-record distance,
  membership inference AUC, TCAP.
* `risk_utility_frontier()` for comparing multiple synthesisers on the
  Duncan-Lambert R-U confidentiality map.
* `estimate_contamination()` for cellwise contamination detection.
* `strip_diagnostics()` to remove sensitive diagnostics before data release.

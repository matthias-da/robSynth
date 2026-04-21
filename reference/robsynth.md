# Robust synthetic data generation

Main entry point for the robSynth package. Generates synthetic data from
a (possibly contaminated) original dataset using robust sequential
conditional synthesis.

## Usage

``` r
robsynth(
  data,
  method = NULL,
  m = 1L,
  visit.sequence = NULL,
  predictor.matrix = NULL,
  formulas = NULL,
  transform = NULL,
  weights = NULL,
  n_synth = nrow(data),
  seed = NULL,
  proper = FALSE,
  cont.na = NULL,
  n_cores = 1L,
  ...
)
```

## Arguments

- data:

  a data.frame (or data.table, which is converted internally)

- method:

  synthesis method(s). Either a single string applied to all variables,
  or a named character vector with one method per variable. Available
  methods for continuous variables: `"mm"` (MM-estimation, default
  robust), `"norm"` (OLS + normal noise), `"cart"` (CART via rpart),
  `"ctree"` (conditional inference tree), `"sample"` (marginal
  resampling). For categorical variables: `"robust_logreg"`
  (Huber-weighted logistic, default robust), `"logreg"` (standard
  logistic), `"polyreg"` (multinomial logistic via nnet), `"cart"`,
  `"ctree"`, `"sample"`. Special methods: `"passive"` (deterministic
  function of other variables, see `formulas`), `""` (skip: variable not
  synthesised, used as predictor only).

- m:

  number of synthetic copies to generate. Default 1. When `m > 1`, the
  `synth` component is a list of data frames and combining rules apply
  to model-based inference.

- visit.sequence:

  character vector specifying the order in which variables are
  synthesised. Default: column order of `data`.

- predictor.matrix:

  a \\p \times p\\ binary matrix where entry `[j, k] = 1` means variable
  `k` is used as predictor for variable `j`. Default: lower-triangular
  (each variable predicted by all prior variables in the visit
  sequence).

- formulas:

  optional named list of formulas. For variables with custom formulas,
  the right-hand side specifies predictors with optional interactions
  and transformations (e.g., `list(y = ~ x1 * x2 + I(x3^2))`). For
  `method = "passive"`, the formula specifies the deterministic function
  (e.g., `list(bmi = ~ I(weight / height^2 * 10000))`).

- transform:

  optional named list mapping variable names to transformation type:
  `"log"`, `"sqrt"`, or `"boxcox"` (robust Box-Cox with robustly
  estimated lambda). Transformations are applied before synthesis and
  back-transformed afterwards.

- weights:

  survey/design weights. Either a character string naming a column in
  `data` (which is then removed from synthesis variables), or a numeric
  vector of length `nrow(data)`. Weights are passed to all fitting
  functions (lmrob, lm, glm).

- n_synth:

  number of synthetic records. Default: `nrow(data)`.

- seed:

  random seed for reproducibility.

- proper:

  logical. Currently ignored (plug-in synthesis only). Future versions
  will support bootstrap-based proper synthesis.

- cont.na:

  optional named list of values to treat as NA for continuous variables
  (e.g., `list(income = c(-8, -9))`).

- n_cores:

  number of cores for parallel synthesis when `m > 1`. Default 1
  (sequential).

- ...:

  additional arguments passed to fitting functions.

## Value

An S3 object of class `"robsynth"` with components:

- synth:

  synthetic data.frame (or list of data.frames when `m > 1`)

- m:

  number of synthetic copies

- method:

  named character vector of methods used

- visit.sequence:

  variable ordering

- predictor.matrix:

  the predictor matrix used

- formulas:

  custom formulas (if any)

- transform:

  transformations applied (if any)

- models:

  list of fitted models (from the last synthesis)

- var_types:

  named character vector of variable types

- call:

  the matched call

## Details

Variables are synthesised sequentially. For each variable, a model is
fitted on the original data conditional on the predictors specified by
the predictor matrix. Synthetic values are then drawn from the fitted
model using the already-synthesised predictor values.

The default method for continuous variables is `"mm"` (MM-estimation via
[`robustbase::lmrob`](https://rdrr.io/pkg/robustbase/man/lmrob.html)),
which achieves 50% breakdown point and 95% Gaussian efficiency. The
default for categorical variables is `"robust_logreg"` (Huber-weighted
logistic regression).

## References

Nowok B., Raab G. M., Dibben C. (2016). synthpop: Bespoke creation of
synthetic data in R. *Journal of Statistical Software*, 74(11), 1-26.

Reiter J. P. (2003). Inference for partially synthetic, public use
microdata sets. *Survey Methodology*, 29(2), 181-188.

Templ M., Meindl B., Kowarik A., Dupriez O. (2017). Simulation of
synthetic complex data: The R package simPop. *Journal of Statistical
Software*, 79(10), 1-38.

Yohai V. J. (1987). High breakdown-point and high efficiency robust
estimates for regression. *Annals of Statistics*, 15(2), 642-656.

Box G. E. P., Cox D. R. (1964). An analysis of transformations. *Journal
of the Royal Statistical Society: Series B*, 26(2), 211-252.

## See also

[`synth_utility`](https://matthias-da.github.io/robSynth/reference/synth_utility.md),
[`synth_risk`](https://matthias-da.github.io/robSynth/reference/synth_risk.md),
[`compare.robsynth`](https://matthias-da.github.io/robSynth/reference/compare.md),
[`synth_lm`](https://matthias-da.github.io/robSynth/reference/synth_lm.md)

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
# CrohnD: 117 Crohn's-disease patients, mixed continuous + factor
# variables, known outliers in BMI and nrAdvE (Cantoni & Ronchetti,
# 2001, via robustbase).
data(CrohnD, package = "robustbase")
dat <- CrohnD[, -1]                     # drop opaque ID

# Single method (default: robust MM + Huber-weighted logistic)
res <- robsynth(dat)

# Per-variable methods (mix robust and non-robust backends)
res2 <- robsynth(dat,
  method = c(BMI = "mm", height = "norm", weight = "mm",
             age = "robust_rf", country = "robust_logreg",
             sex = "robust_logreg", treat = "polyreg"))

# Multiple synthesis with Reiter combining rules
res3 <- robsynth(dat, m = 5)
} # }
```

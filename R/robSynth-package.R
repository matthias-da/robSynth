#' robSynth: Robust Synthetic Data Generation
#'
#' Robust synthetic data generation methods that protect data quality
#' from contaminated training data.  Replaces the conditional models in
#' sequential synthesis with MM-estimators (continuous) and weighted
#' logistic regression (categorical).
#'
#' @importFrom stats as.formula binomial coef complete.cases confint
#'   cor density fitted glm ks.test lm mad median predict rnorm
#'   rbinom sd setNames var quantile pchisq dnorm pnorm approx
#' @keywords internal
"_PACKAGE"

## Silence R CMD check for ggplot2 tidy-eval pronoun
if (getRversion() >= "2.15.1") {
  utils::globalVariables(".data")
}

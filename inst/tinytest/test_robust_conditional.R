# test_robust_conditional.R — tinytest tests for fit/generate functions
# Tests: fit_robust_continuous, generate_robust_continuous,
#        fit_robust_categorical, generate_robust_categorical

library(tinytest)
library(robSynth)

# --- test data ---------------------------------------------------------------

set.seed(42)
n <- 80
X <- data.frame(x1 = rnorm(n, 5, 2), x2 = rnorm(n, 0, 1))
y_cont <- 2 + 0.5 * X$x1 - 0.3 * X$x2 + rnorm(n, sd = 0.5)
y_cat  <- factor(ifelse(X$x1 > 5, "high", "low"))

# --- fit_robust_continuous (MM) ----------------------------------------------

fit_mm <- fit_robust_continuous(y_cont, X, method = "mm")

expect_inherits(fit_mm, "robsynth_fit_cont",
                info = "fit_robust_continuous returns robsynth_fit_cont")
expect_equal(fit_mm$method, "mm")
expect_true(is.numeric(fit_mm$sigma) && fit_mm$sigma > 0,
            info = "sigma is positive numeric")
expect_equal(length(fit_mm$y_obs), n)
expect_equal(length(fit_mm$yhat_obs), n)

# --- generate_robust_continuous (normal uncertainty) -------------------------

X_new <- data.frame(x1 = rnorm(30, 5, 2), x2 = rnorm(30))
y_synth <- generate_robust_continuous(fit_mm, X_new, uncertainty = "normal")
expect_equal(length(y_synth), 30L)
expect_true(is.numeric(y_synth))
# synthetic values should be in a reasonable range
expect_true(all(is.finite(y_synth)),
            info = "no NA/NaN/Inf in generated values")

# --- generate_robust_continuous (PMM) ----------------------------------------

y_pmm <- generate_robust_continuous(fit_mm, X_new, uncertainty = "pmm",
                                    donors = 5)
expect_equal(length(y_pmm), 30L)
# PMM draws must come from observed y
expect_true(all(y_pmm %in% y_cont),
            info = "PMM draws come from observed response values")

# --- fit_robust_continuous with weights (Approach 2) -------------------------

w_list <- list(
  w_response   = runif(n, 0.5, 1),
  w_predictors = matrix(runif(n * 2, 0.5, 1), ncol = 2)
)
fit_w <- fit_robust_continuous(y_cont, X, method = "mm", weights = w_list)
expect_inherits(fit_w, "robsynth_fit_cont",
                info = "weighted fit returns robsynth_fit_cont")

# --- fit_robust_categorical --------------------------------------------------

fit_cat <- fit_robust_categorical(y_cat, X)

expect_inherits(fit_cat, "robsynth_fit_cat",
                info = "fit_robust_categorical returns robsynth_fit_cat")
expect_equal(fit_cat$levels, levels(y_cat))

# --- generate_robust_categorical ---------------------------------------------

y_cat_synth <- generate_robust_categorical(fit_cat, X_new)
expect_equal(length(y_cat_synth), 30L)
expect_inherits(y_cat_synth, "factor")
# synthetic factor levels subset of original levels
expect_true(all(levels(y_cat_synth) %in% levels(y_cat)),
            info = "synthetic factor levels match original")

# --- categorical with 3 levels (multinomial) ---------------------------------

y_cat3 <- factor(sample(c("A", "B", "C"), n, replace = TRUE))
fit_cat3 <- fit_robust_categorical(y_cat3, X)
y_cat3_synth <- generate_robust_categorical(fit_cat3, X_new)
expect_equal(length(y_cat3_synth), 30L)
expect_inherits(y_cat3_synth, "factor")
expect_equal(sort(fit_cat3$levels), c("A", "B", "C"))

# --- character response is coerced to factor ---------------------------------

y_char <- sample(c("yes", "no"), n, replace = TRUE)
fit_char <- fit_robust_categorical(y_char, X)
expect_inherits(fit_char, "robsynth_fit_cat")
expect_true(length(fit_char$levels) == 2L)

# test_evaluation.R — tinytest tests for synth_utility and synth_risk

library(tinytest)
library(robSynth)

# --- set up simple data ------------------------------------------------------

set.seed(10)
n <- 60
orig <- data.frame(
  x1 = rnorm(n, 10, 2),
  x2 = rnorm(n, 5, 1),
  x3 = rnorm(n, 0, 1),
  grp = factor(sample(c("A", "B"), n, replace = TRUE))
)

# a reasonable synthetic dataset (bootstrap resample + jitter)
set.seed(11)
idx <- sample(n, n, replace = TRUE)
synth <- orig[idx, ]
synth$x1 <- synth$x1 + rnorm(n, sd = 0.3)
synth$x2 <- synth$x2 + rnorm(n, sd = 0.2)
synth$x3 <- synth$x3 + rnorm(n, sd = 0.2)
rownames(synth) <- NULL

# --- synth_utility -----------------------------------------------------------

util <- synth_utility(orig, synth)
expect_inherits(util, "synth_utility",
                info = "synth_utility returns synth_utility class")

# pMSE should be a non-negative scalar
expect_true(is.numeric(util$pMSE) && length(util$pMSE) == 1L)
expect_true(util$pMSE >= 0,
            info = "pMSE is non-negative")

# KS statistics: one per continuous variable
expect_equal(length(util$KS), 3L,
             info = "KS has one entry per continuous variable")
expect_true(all(util$KS >= 0 & util$KS <= 1),
            info = "KS values in [0, 1]")

# correlation distance: scalar
expect_true(is.numeric(util$correlation) && length(util$correlation) == 1L)

# overlap: one per variable (4 total)
expect_equal(length(util$overlap), 4L)
expect_true(all(util$overlap >= 0 & util$overlap <= 1),
            info = "overlap in [0, 1]")

# regression: relative_bias and ci_overlap present
expect_true(!is.null(util$regression$relative_bias))
expect_true(!is.null(util$regression$ci_overlap))

# --- synth_utility with subset of metrics ------------------------------------

util_ks <- synth_utility(orig, synth, metrics = "KS")
expect_true(!is.null(util_ks$KS))
expect_true(is.null(util_ks$pMSE),
            info = "only requested metrics are computed")

# --- synth_risk --------------------------------------------------------------

risk <- synth_risk(orig, synth)
expect_inherits(risk, "synth_risk",
                info = "synth_risk returns synth_risk class")

# k_anonymity
expect_true(is.list(risk$k_anonymity))
expect_true(risk$k_anonymity$synth_only >= 1L)

# distance
expect_true(is.list(risk$distance))
expect_true(risk$distance$mean_min_dist >= 0)
expect_equal(length(risk$distance$min_distances), n)

# membership_inference: AUC in [0, 1]
expect_true(is.numeric(risk$membership_inference))

# TCAP in [0, 1]
expect_true(risk$TCAP >= 0 && risk$TCAP <= 1,
            info = "TCAP in [0, 1]")

# --- risk_utility_frontier ---------------------------------------------------

set.seed(12)
synth2 <- orig
synth2[, 1:3] <- orig[, 1:3] + matrix(rnorm(n * 3, sd = 2), ncol = 3)

ru <- risk_utility_frontier(orig,
                            list(bootstrap = synth, noisy = synth2),
                            utility_metric = "pMSE",
                            risk_metric = "distance")
expect_inherits(ru, "ru_frontier")
expect_equal(nrow(ru), 2L)
expect_true("pareto_optimal" %in% names(ru))
expect_true(is.logical(ru$pareto_optimal))

# print does not error
expect_silent(capture.output(print(ru)))

# test_robsynth.R — tinytest tests for robsynth() main entry point

library(tinytest)
library(robSynth)

# --- set up small test data --------------------------------------------------

set.seed(123)
n <- 80
dat <- data.frame(
  x1 = rnorm(n, 10, 2),
  x2 = rnorm(n, 5, 1),
  x3 = rnorm(n, 0, 3),
  grp = factor(sample(c("A", "B", "C"), n, replace = TRUE))
)

# --- basic call (robust_conditional, mm) ------------------------------------

res <- robsynth(dat, method = "robust_conditional", seed = 1)

# output class
expect_inherits(res, "robsynth",
                info = "robsynth returns object of class robsynth")

# output components
expect_true(is.data.frame(res$synth))
expect_true(!is.null(res$method))
expect_true(!is.null(res$visit.sequence))
expect_true(!is.null(res$var_types))
expect_true(is.list(res$models))

# dimensions match
expect_equal(nrow(res$synth), n)
expect_equal(ncol(res$synth), ncol(dat))
expect_equal(names(res$synth), names(dat))

# factor levels preserved
expect_equal(levels(res$synth$grp), levels(dat$grp))
expect_inherits(res$synth$grp, "factor")

# var_types correct
expect_equal(unname(res$var_types["grp"]), "categorical")
expect_equal(unname(res$var_types["x1"]), "continuous")

# --- custom n_synth ----------------------------------------------------------

res50 <- robsynth(dat, n_synth = 50, seed = 2)
expect_equal(nrow(res50$synth), 50L,
             info = "n_synth controls number of synthetic rows")

# --- visit_sequence ----------------------------------------------------------

res_vs <- robsynth(dat, visit.sequence = c("grp", "x2", "x1", "x3"), seed = 3)
expect_equal(res_vs$visit.sequence, c("grp", "x2", "x1", "x3"))
expect_equal(nrow(res_vs$synth), n)

# --- print and summary do not error -----------------------------------------

expect_silent(capture.output(print(res)))
expect_silent(capture.output(summary(res)))

# --- contWeights is NULL for robust_conditional ------------------------------

expect_true(is.null(res$contWeights),
            info = "no contWeights for robust_conditional method")

# --- strip_diagnostics -------------------------------------------------------

# on a result that has contWeights = NULL, strip_diagnostics still works
res_stripped <- strip_diagnostics(res)
expect_true(is.null(res_stripped$contWeights))
expect_inherits(res_stripped, "robsynth")

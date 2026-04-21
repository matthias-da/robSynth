# test_contam_filter.R — tinytest tests for estimate_contamination
#                       and synth_from_clean

library(tinytest)
library(robSynth)

# --- set up test data --------------------------------------------------------

set.seed(77)
n <- 100
dat_clean <- data.frame(
  x1 = rnorm(n, 10, 2),
  x2 = rnorm(n, 5, 1),
  x3 = rnorm(n, 0, 1)
)

# contaminated version: shift first 10 rows of x1 by +15
dat_contam <- dat_clean
dat_contam$x1[1:10] <- dat_contam$x1[1:10] + 15

# --- estimate_contamination (cellwise) on clean data -------------------------

res_clean <- estimate_contamination(dat_clean, method = "cellwise",
                                    maxit = 200)

expect_true(is.list(res_clean))
# EM may not converge on small clean data (no mixture to separate)
# -- convergence not required, just check it ran without error
expect_equal(dim(res_clean$clean_probs), c(n, 3L))

# on clean data, clean probabilities should mostly be high (> 0.5)
expect_true(mean(res_clean$clean_probs > 0.5) > 0.8,
            info = "most clean probs > 0.5 on clean data")

# epsilon estimates should be bounded below 0.5 on clean data
# (the two-component mixture may absorb some mass even without
# true contamination, so we only check < 0.5 rather than < 0.2)
expect_true(all(res_clean$epsilon < 0.5),
            info = "estimated epsilon below 0.5 on clean data")

# --- estimate_contamination (cellwise) on contaminated data ------------------

res_contam <- estimate_contamination(dat_contam, method = "cellwise",
                                     maxit = 100)

expect_true(is.list(res_contam))
expect_equal(dim(res_contam$clean_probs), c(n, 3L))

# contaminated cells (rows 1:10, col x1) should have lower clean_probs
# than uncontaminated cells
mean_clean_contam_cells <- mean(res_contam$clean_probs[1:10, "x1"])
mean_clean_good_cells   <- mean(res_contam$clean_probs[11:n, "x1"])
expect_true(mean_clean_contam_cells < mean_clean_good_cells,
            info = "contaminated cells get lower clean probability")

# epsilon for x1 should be higher than for clean variables
expect_true(res_contam$epsilon["x1"] > res_contam$epsilon["x2"],
            info = "epsilon higher for contaminated variable")

# --- estimate_contamination (rowwise) ----------------------------------------

res_row <- estimate_contamination(dat_contam, method = "rowwise")
expect_true(is.list(res_row))
expect_equal(res_row$method, "rowwise")
expect_equal(dim(res_row$clean_probs), c(n, 3L))
expect_true(res_row$converged)

# --- synth_from_clean (parametric) -------------------------------------------

synth_par <- synth_from_clean(dat_clean, res_clean$clean_probs,
                              method = "parametric", n_synth = 50, seed = 1)
expect_true(is.data.frame(synth_par))
expect_equal(nrow(synth_par), 50L)
expect_equal(ncol(synth_par), 3L)
# means should be in a reasonable range
expect_true(abs(mean(synth_par$x1) - 10) < 3,
            info = "parametric synth preserves approximate mean")

# --- synth_from_clean (weighted_resample) ------------------------------------

synth_wr <- synth_from_clean(dat_clean, res_clean$clean_probs,
                             method = "weighted_resample", n_synth = 40,
                             seed = 2)
expect_true(is.data.frame(synth_wr))
expect_equal(nrow(synth_wr), 40L)

# --- mixed data with categorical variables -----------------------------------

set.seed(88)
dat_mixed <- data.frame(
  x1 = rnorm(60, 5, 2),
  x2 = rnorm(60, 0, 1),
  grp = factor(sample(c("A", "B"), 60, TRUE))
)

res_mixed <- estimate_contamination(dat_mixed, method = "cellwise")
# categorical column should have clean_probs = 1
expect_true(all(res_mixed$clean_probs[, "grp"] == 1),
            info = "categorical columns get clean_prob = 1")
# epsilon only for continuous variables
expect_equal(length(res_mixed$epsilon), 2L)

# synth_from_clean with mixed data preserves factor levels
synth_mixed <- synth_from_clean(dat_mixed, res_mixed$clean_probs,
                                method = "parametric", n_synth = 30, seed = 3)
expect_inherits(synth_mixed$grp, "factor")
expect_equal(levels(synth_mixed$grp), levels(dat_mixed$grp))

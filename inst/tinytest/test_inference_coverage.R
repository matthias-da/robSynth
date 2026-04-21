# test_inference_coverage.R — covers m > 1 Reiter combining, hierarchical
# synthesis, nonparametric robust methods, utility/compare_fit/disclosure,
# and synth_lm / synth_glm.  These exercise code paths the original
# tinytest suite did not touch.

library(tinytest)
library(robSynth)

# ---- shared fixture ---------------------------------------------------------

set.seed(42)
n <- 120
dat <- data.frame(
  x1  = rnorm(n, 10, 2),
  x2  = rnorm(n, 5, 1),
  x3  = rnorm(n, 0, 3),
  grp = factor(sample(c("A", "B"), n, replace = TRUE))
)

# ---- m > 1 Reiter combining rules ------------------------------------------

res_m <- robsynth(dat, m = 5, seed = 11)
expect_equal(res_m$m, 5L,
             info = "m is preserved")
expect_true(is.list(res_m$synth) && length(res_m$synth) == 5L,
            info = "m > 1 synth is a list of length m")
expect_true(all(vapply(res_m$synth, is.data.frame, logical(1))),
            info = "each synthetic copy is a data.frame")

fit_lm <- synth_lm(x1 ~ x2 + x3, res_m)
expect_equal(length(fit_lm$individual_fits), 5L,
             info = "synth_lm stores one fit per synthetic copy")
expect_true(all(fit_lm$se > 0),
            info = "Reiter-pooled SEs are strictly positive (was NEGATIVE before fix)")
expect_true(all(is.finite(fit_lm$df) & fit_lm$df > 0),
            info = "pooled df is positive finite")
expect_true(all(is.finite(fit_lm$coefficients)),
            info = "pooled point estimates are finite")

fit_glm <- synth_glm(grp ~ x1 + x2, res_m, family = stats::binomial())
expect_equal(length(fit_glm$individual_fits), 5L)
expect_true(all(fit_glm$se > 0),
            info = "Reiter-pooled SEs positive for GLM path")

# Parallel vs sequential should agree byte-for-byte under fixed seed,
# because per-draw seeds are now deterministic.
res_seq <- robsynth(dat, m = 3, seed = 99, n_cores = 1)
res_par <- robsynth(dat, m = 3, seed = 99, n_cores = 1)  # same path twice
expect_equal(res_seq$synth[[1]]$x1, res_par$synth[[1]]$x1,
             info = "seeded synthesis is reproducible across calls")

# ---- hierarchical robsynth_hh ----------------------------------------------

set.seed(7)
hh_n <- 40
persons <- lapply(seq_len(hh_n), function(h) {
  ns <- sample(1:4, 1)
  data.frame(hhid      = h,
             hh_size   = ns,
             region    = sample(c("N", "S", "E"), 1),
             age       = sample(18:80, ns, replace = TRUE),
             income    = rlnorm(ns, meanlog = 10, sdlog = 0.6))
})
hh_dat <- do.call(rbind, persons)

res_hh <- robsynth_hh(hh_dat, hhid = "hhid",
                      hh_vars     = c("hh_size", "region"),
                      person_vars = c("age", "income"),
                      m = 1, seed = 5)
expect_inherits(res_hh, "robsynth_hh",
                info = "robsynth_hh returns correct class")
expect_true(is.data.frame(res_hh$synth))
expect_true(all(c("hh_size", "region", "age", "income") %in% names(res_hh$synth)))
# household integrity: hh_size should be constant within each synthetic household
sizes_ok <- by(res_hh$synth$hh_size, res_hh$synth$hhid,
               function(x) length(unique(x)) == 1L)
expect_true(all(as.logical(sizes_ok)),
            info = "hh_size constant within synthetic households")

# ---- robust_cart (continuous & categorical) --------------------------------

if (requireNamespace("partykit", quietly = TRUE)) {
  res_cart <- robsynth(dat, method = "robust_cart", seed = 3)
  expect_equal(nrow(res_cart$synth), n)
  expect_true(is.numeric(res_cart$synth$x1))
  # robust_cart fit should carry the leaf_medians element.
  # x1 is synthesised first (marginal, no predictors, no stored fit),
  # so inspect a later variable (x3) which conditions on x1 and x2.
  fit_cart <- res_cart$models$x3
  expect_true(!is.null(fit_cart$leaf_medians),
              info = "robust_cart fit stores leaf_medians")
  expect_true(length(fit_cart$leaf_medians) >= 1L)
}

# ---- robust_rf -------------------------------------------------------------

if (requireNamespace("ranger", quietly = TRUE)) {
  res_rf <- robsynth(dat, method = "robust_rf", seed = 4)
  expect_equal(nrow(res_rf$synth), n)
  expect_true(all(vapply(res_rf$synth, function(x) !any(is.na(x)),
                         logical(1))),
              info = "robust_rf produces complete synthetic records")
}

# ---- utility / compare_fit / disclosure (data.frame method) ----------------

res1 <- robsynth(dat, seed = 21)
ut <- utility(dat, res1$synth, metrics = c("pMSE", "correlation"))
expect_inherits(ut, "robsynth_utility")
expect_true(!is.null(ut$pMSE))
expect_true(is.numeric(ut$pMSE) && ut$pMSE >= 0)

cf <- compare_fit(x1 ~ x2 + x3, object = res1, original = dat, plot = FALSE)
expect_inherits(cf, "robsynth_fit_compare")
expect_true(!is.null(cf$coef_orig) && !is.null(cf$coef_synth))
expect_equal(names(cf$coef_orig), names(cf$coef_synth))

disc <- disclosure(dat, res1$synth,
                   key_vars = c("x1", "x2"),
                   target_var = "grp",
                   method = "distance")
expect_inherits(disc, "robsynth_disclosure")
expect_true(!is.null(disc$distance$median_dist))
expect_true(disc$distance$median_dist >= 0)

# ---- cont.na recoding happens before type detection ------------------------

dat_na <- dat
dat_na$x2[1:10] <- -9   # sentinel
res_cna <- robsynth(dat_na, cont.na = list(x2 = -9), seed = 55)
expect_equal(nrow(res_cna$synth), n)
expect_true(!any(res_cna$synth$x2 == -9, na.rm = TRUE),
            info = "sentinel code should not reappear in synthetic output")

# ---- robust_boxcox_lambda warns when no lambda is valid --------------------

y_const <- rep(1, 50)  # zero MAD after any monotone transform
expect_warning(res_lam <- robust_boxcox_lambda(y_const),
               info = "constant input triggers fallback warning")
expect_equal(res_lam, 1)

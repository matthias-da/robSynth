# test_helpers.R — tinytest tests for robSynth helper functions
# Tests: detect_var_types, huber_weights, weighted_median, weighted_mad,
#        generate_ar1_cov, pmm_draw

library(tinytest)
library(robSynth)

# --- detect_var_types --------------------------------------------------------

# numeric columns -> continuous
df1 <- data.frame(x = rnorm(20), y = rnorm(20))
vt1 <- detect_var_types(df1)
expect_true(all(vt1 == "continuous"),
            info = "numeric columns classified as continuous")

# factor and character -> categorical
df2 <- data.frame(x = rnorm(20),
                  f = factor(sample(letters[1:3], 20, TRUE)),
                  ch = sample(c("a", "b"), 20, TRUE),
                  stringsAsFactors = FALSE)
vt2 <- detect_var_types(df2)
expect_equal(unname(vt2["x"]), "continuous")
expect_equal(unname(vt2["f"]), "categorical")
expect_equal(unname(vt2["ch"]), "categorical")

# numeric with few unique values -> categorical (max_levels = 5)
df3 <- data.frame(x = sample(1:3, 30, TRUE), y = rnorm(30))
vt3 <- detect_var_types(df3)
expect_equal(unname(vt3["x"]), "categorical",
             info = "numeric with <= 5 unique values -> categorical")
expect_equal(unname(vt3["y"]), "continuous")

# logical columns -> categorical
df4 <- data.frame(flag = c(TRUE, FALSE, TRUE, TRUE))
expect_equal(unname(detect_var_types(df4)["flag"]), "categorical")

# --- huber_weights -----------------------------------------------------------

set.seed(42)
x_clean <- rnorm(100)
w_clean <- huber_weights(x_clean, k = 1.345)
expect_true(all(w_clean >= 0 & w_clean <= 1),
            info = "huber weights in [0, 1]")

# extreme outlier should get weight < 1
x_out <- c(rnorm(50), 100)
w_out <- huber_weights(x_out)
expect_true(w_out[51] < 0.1,
            info = "large outlier gets downweighted")

# constant input: no crash, all weights = 1
w_const <- huber_weights(rep(5, 20))
expect_true(all(w_const == 1),
            info = "constant input gives weights = 1")

# --- weighted_median ---------------------------------------------------------

# equal weights -> ordinary median
expect_equal(weighted_median(1:5, w = rep(1, 5)), median(1:5))

# heavy weight on one value pulls median
wm <- weighted_median(c(1, 2, 3), w = c(100, 1, 1))
expect_equal(wm, 1,
             info = "large weight on 1 pulls weighted median to 1")

# NA handling
wm_na <- weighted_median(c(NA, 2, 3), w = c(1, 1, 1), na.rm = TRUE)
expect_true(!is.na(wm_na))

# --- weighted_mad ------------------------------------------------------------

set.seed(7)
x_norm <- rnorm(200)
w_eq   <- rep(1, 200)
wmad   <- weighted_mad(x_norm, w_eq)
# should be close to 1 (consistency factor for normal)
expect_true(abs(wmad - 1) < 0.5,
            info = "weighted_mad with equal weights near MAD")

# downweighting outlier reduces MAD
x_big <- c(rnorm(100), 50)
w_big <- c(rep(1, 100), 0.001)
wmad_down <- weighted_mad(x_big, w_big)
wmad_full <- weighted_mad(x_big, rep(1, 101))
expect_true(wmad_down <= wmad_full,
            info = "downweighting outlier reduces weighted MAD")

# --- generate_ar1_cov -------------------------------------------------------

S <- generate_ar1_cov(4, 0.7)
expect_equal(nrow(S), 4L)
expect_equal(ncol(S), 4L)

# diagonal should be 1
expect_equal(diag(S), rep(1, 4), tolerance = 1e-12)

# off-diagonal: S[1,2] = 0.7, S[1,3] = 0.49
expect_equal(S[1, 2], 0.7, tolerance = 1e-12)
expect_equal(S[1, 3], 0.7^2, tolerance = 1e-12)

# positive definiteness
eig <- eigen(S, symmetric = TRUE, only.values = TRUE)$values
expect_true(all(eig > 0), info = "AR(1) covariance is positive definite")

# p = 1 case
S1 <- generate_ar1_cov(1, 0.5)
expect_equal(S1, matrix(1, 1, 1))

# --- pmm_draw ----------------------------------------------------------------

set.seed(99)
y_obs    <- 1:20
yhat_obs <- y_obs + rnorm(20, sd = 0.1)
yhat_new <- c(5, 10, 15)
drawn    <- robSynth:::pmm_draw(y_obs, yhat_obs, yhat_new, donors = 3)

expect_equal(length(drawn), 3L)
# drawn values must be from observed pool
expect_true(all(drawn %in% y_obs),
            info = "PMM draws come from observed values")

## Method registry
##
## Each key in `.METHODS` maps a synthesis-method name to a triple
## (types, fit, gen):
##
##   - types:  character vector with "continuous", "categorical", or both
##   - fit:    function(y, X, df, fml, obs_weights, var_type,
##                      lvls, n_levels, n, ...) -> robsynth_fit_*
##   - gen:    function(fit, X_new, n, var_type, lvls, ...) -> vector
##
## The dispatchers `.fit_dispatch` / `.generate_dispatch` are thin
## lookups into this table.  Adding a method is a single edit instead
## of three (valid-methods list, fit switch, gen switch).
##
## `passive` and `""` are handled in `robsynth()` directly and are NOT
## in the registry.

# ---- result constructors --------------------------------------------------

.cont_fit <- function(model, method, sigma, y, yhat = stats::fitted(model),
                     extra = list()) {
  base <- list(model = model, method = method, sigma = sigma,
               y_obs = y, yhat_obs = as.numeric(yhat))
  structure(utils::modifyList(base, extra),
            class = "robsynth_fit_cont")
}

.cat_fit <- function(model, method, lvls, y_obs = NULL, extra = list()) {
  base <- list(model = model, method = method, levels = lvls,
               n_levels = length(lvls), y_obs = y_obs)
  structure(utils::modifyList(base, extra),
            class = "robsynth_fit_cat")
}

# ---- fit helpers ----------------------------------------------------------

.fit_mm <- function(y, X, df, fml, obs_weights, var_type, lvls, n_levels, n, ...) {
  fit <- tryCatch(
    suppressWarnings(robustbase::lmrob(fml, data = df, weights = obs_weights,
                                       setting = "KS2014", ...)),
    error = function(e) stats::lm(fml, data = df, weights = obs_weights))
  sigma <- if (inherits(fit, "lmrob")) fit$scale else summary(fit)$sigma
  .cont_fit(fit, "mm", sigma, y)
}

.fit_norm <- function(y, X, df, fml, obs_weights, var_type, lvls, n_levels, n, ...) {
  fit <- stats::lm(fml, data = df, weights = obs_weights)
  .cont_fit(fit, "norm", summary(fit)$sigma, y)
}

.fit_cart <- function(y, X, df, fml, obs_weights, var_type, lvls, n_levels, n, ...) {
  if (var_type == "continuous") {
    fit <- rpart::rpart(fml, data = df, ...)
    yhat <- stats::predict(fit, newdata = df)
    sig <- stats::mad(y - yhat)
    if (sig < .Machine$double.eps) sig <- stats::sd(y - yhat)
    .cont_fit(fit, "cart", sig, y, yhat = yhat)
  } else {
    fit <- rpart::rpart(fml, data = df, method = "class", ...)
    .cat_fit(fit, "cart", lvls)
  }
}

.fit_ctree <- function(y, X, df, fml, obs_weights, var_type, lvls, n_levels, n, ...) {
  if (!requireNamespace("partykit", quietly = TRUE))
    stop("Package 'partykit' required for method 'ctree'")
  fit <- partykit::ctree(fml, data = df, ...)
  if (var_type == "continuous") {
    yhat <- stats::predict(fit, newdata = df)
    sig <- stats::mad(y - yhat)
    if (sig < .Machine$double.eps) sig <- stats::sd(y - yhat)
    .cont_fit(fit, "ctree", sig, y, yhat = yhat)
  } else {
    .cat_fit(fit, "ctree", lvls)
  }
}

.fit_sample <- function(y, X, df, fml, obs_weights, var_type, lvls, n_levels, n, ...) {
  if (var_type == "continuous")
    .cont_fit(NULL, "sample", 0, y, yhat = y)
  else
    .cat_fit(NULL, "sample", lvls, y_obs = y)
}

.fit_logreg <- function(y, X, df, fml, obs_weights, var_type, lvls, n_levels, n, ...) {
  if (n_levels > 2L) {
    fit <- tryCatch(
      nnet::multinom(fml, data = df, weights = obs_weights, trace = FALSE, ...),
      error = function(e) NULL)
  } else {
    fit <- tryCatch(
      stats::glm(fml, data = df, family = stats::binomial(),
                 weights = obs_weights),
      error = function(e) NULL)
  }
  .cat_fit(fit, "logreg", lvls)
}

.fit_robust_logreg <- function(y, X, df, fml, obs_weights, var_type,
                               lvls, n_levels, n, ...) {
  if (n_levels > 2L) {
    fit <- tryCatch({
      fit0 <- nnet::multinom(fml, data = df, weights = obs_weights, trace = FALSE)
      prior <- as.numeric(prop.table(table(y)))
      p_hat <- fit0$fitted.values[cbind(seq_len(n), as.integer(y))]
      sc <- log(pmax(p_hat, 1e-10)) - log(prior[as.integer(y)])
      z  <- (sc - stats::median(sc)) / pmax(stats::mad(sc), 1e-10)
      rob_wts <- obs_weights * ifelse(z < -3, 0.1, 1.0)
      nnet::multinom(fml, data = df, weights = rob_wts, trace = FALSE)
    }, error = function(e) NULL)
  } else {
    fit <- tryCatch({
      fit0 <- stats::glm(fml, data = df, family = stats::binomial(),
                         weights = obs_weights)
      rs_named <- stats::rstandard(fit0, type = "pearson")
      rs <- rep(0, n); names(rs) <- rownames(df)
      rs[names(rs_named)] <- rs_named
      wts <- obs_weights * ifelse(abs(rs) > 3, 0.1, 1.0)
      stats::glm(fml, data = df, family = stats::binomial(), weights = wts)
    }, error = function(e) NULL)
  }
  .cat_fit(fit, "robust_logreg", lvls)
}

.fit_polyreg <- function(y, X, df, fml, obs_weights, var_type,
                         lvls, n_levels, n, ...) {
  fit <- tryCatch(
    nnet::multinom(fml, data = df, trace = FALSE, ...),
    error = function(e) NULL)
  .cat_fit(fit, "polyreg", lvls)
}

.fit_robust_cart_wrap <- function(y, X, df, fml, obs_weights, var_type,
                                   lvls, n_levels, n, ...) {
  .fit_robust_cart(y, X, var_type, obs_weights, ...)
}

.fit_robust_rf_wrap <- function(y, X, df, fml, obs_weights, var_type,
                                 lvls, n_levels, n, ...) {
  .fit_robust_rf(y, X, var_type, obs_weights, ...)
}

.fit_robust_xgb_wrap <- function(y, X, df, fml, obs_weights, var_type,
                                  lvls, n_levels, n, ...) {
  .fit_robust_xgb(y, X, var_type, obs_weights, ...)
}

# ---- gen helpers ----------------------------------------------------------

.gen_predict_plus_noise <- function(fit, X_new, n, var_type, lvls, ...) {
  yhat <- stats::predict(fit$model, newdata = X_new)
  as.numeric(yhat) + rnorm(n, 0, fit$sigma)
}

.gen_sample <- function(fit, X_new, n, var_type, lvls, ...) {
  if (var_type == "continuous")
    sample(fit$y_obs, n, replace = TRUE)
  else
    factor(sample(fit$y_obs, n, replace = TRUE), levels = lvls)
}

.gen_cat_dispatch <- function(fit, X_new, n, var_type, lvls, ...) {
  ## Shared categorical generator used by logreg / robust_logreg /
  ## polyreg / cart / ctree.  Uses the fit's class to pick the right
  ## prediction path.
  if (is.null(fit$model)) {
    return(factor(sample(lvls, n, replace = TRUE), levels = lvls))
  }
  if (fit$n_levels == 2L && inherits(fit$model, "glm")) {
    probs <- stats::predict(fit$model, newdata = X_new, type = "response")
    probs <- pmin(pmax(probs, 0.001), 0.999)
    draws <- rbinom(n, 1, probs)
    return(factor(lvls[draws + 1L], levels = lvls))
  }
  if (inherits(fit$model, "multinom")) {
    probs <- stats::predict(fit$model, newdata = X_new, type = "probs")
    if (is.null(dim(probs))) probs <- cbind(1 - probs, probs)
    drawn <- apply(probs, 1, function(p) {
      p <- pmax(p, 0); p <- p / sum(p); sample(lvls, 1, prob = p)
    })
    return(factor(drawn, levels = lvls))
  }
  # CART / ctree
  pred <- stats::predict(fit$model, newdata = X_new,
                          type = if (inherits(fit$model, "rpart")) "class"
                                 else "response")
  factor(as.character(pred), levels = lvls)
}

.gen_tree <- function(fit, X_new, n, var_type, lvls, ...) {
  if (var_type == "continuous") .gen_predict_plus_noise(fit, X_new, n, var_type, lvls)
  else .gen_cat_dispatch(fit, X_new, n, var_type, lvls)
}

.gen_robust_cart_wrap <- function(fit, X_new, n, var_type, lvls, ...) {
  .generate_robust_cart(fit, X_new, n)
}

.gen_robust_rf_wrap <- function(fit, X_new, n, var_type, lvls, ...) {
  .generate_robust_rf(fit, X_new, n)
}

.gen_robust_xgb_wrap <- function(fit, X_new, n, var_type, lvls, ...) {
  .generate_robust_xgb(fit, X_new, n)
}

# ---- the registry ---------------------------------------------------------

.METHODS <- list(
  mm             = list(types = "continuous",
                        fit   = .fit_mm,
                        gen   = .gen_predict_plus_noise),
  norm           = list(types = "continuous",
                        fit   = .fit_norm,
                        gen   = .gen_predict_plus_noise),
  cart           = list(types = c("continuous", "categorical"),
                        fit   = .fit_cart,
                        gen   = .gen_tree),
  ctree          = list(types = c("continuous", "categorical"),
                        fit   = .fit_ctree,
                        gen   = .gen_tree),
  sample         = list(types = c("continuous", "categorical"),
                        fit   = .fit_sample,
                        gen   = .gen_sample),
  robust_cart    = list(types = c("continuous", "categorical"),
                        fit   = .fit_robust_cart_wrap,
                        gen   = .gen_robust_cart_wrap),
  robust_rf      = list(types = c("continuous", "categorical"),
                        fit   = .fit_robust_rf_wrap,
                        gen   = .gen_robust_rf_wrap),
  robust_xgboost = list(types = c("continuous", "categorical"),
                        fit   = .fit_robust_xgb_wrap,
                        gen   = .gen_robust_xgb_wrap),
  robust_logreg  = list(types = "categorical",
                        fit   = .fit_robust_logreg,
                        gen   = .gen_cat_dispatch),
  logreg         = list(types = "categorical",
                        fit   = .fit_logreg,
                        gen   = .gen_cat_dispatch),
  polyreg        = list(types = "categorical",
                        fit   = .fit_polyreg,
                        gen   = .gen_cat_dispatch)
)

#' Known synthesis method names (including `"passive"` and `""`, which are
#' handled outside the registry in `robsynth()`).
#' @keywords internal
.valid_method_names <- function() c(names(.METHODS), "passive", "")

#' Method names that support a given variable type.
#' @keywords internal
.methods_for_type <- function(type) {
  keep <- vapply(.METHODS, function(m) type %in% m$types, logical(1))
  names(.METHODS)[keep]
}

#' Fit a robust model for a continuous variable
#'
#' Fits a robust regression model for sequential conditional synthesis.
#' MM-estimation (\code{robustbase::lmrob}) is the default. When
#' \code{method = "robust_cart"}, a robust CART model is fitted via
#' \code{partykit::ctree} (if available) or \code{rpart::rpart}
#' with Huber-weighted splits as fallback.
#'
#' @param y numeric response vector
#' @param X data.frame of predictors (already synthesised variables)
#' @param method \code{"mm"} (default) or \code{"robust_cart"}
#' @param weights optional cell-level weights: a list with components
#'   \code{w_response} (length \code{n}) and \code{w_predictors}
#'   (n x p_pred matrix) coming from \code{\link{estimate_contamination}}.
#'   Leave as \code{NULL} to use lmrob's built-in M-weights only.
#' @param ... additional arguments passed to \code{lmrob} or
#'   \code{rpart}/\code{ctree}
#'
#' @return A list of class \code{"robsynth_fit_cont"} with components:
#'   \item{model}{the fitted model object}
#'   \item{method}{the fitting method used}
#'   \item{sigma}{robust residual scale estimate}
#'   \item{y_obs}{observed response (for PMM)}
#'   \item{yhat_obs}{fitted values (for PMM)}
#'
#' @author Matthias Templ
#' @export
#' @importFrom robustbase lmrob
#' @examples
#' \dontrun{
#' fit <- fit_robust_continuous(iris$Sepal.Width,
#'                              iris[, "Sepal.Length", drop = FALSE])
#' }
fit_robust_continuous <- function(y, X,
                                  method = c("mm", "robust_cart"),
                                  weights = NULL, ...) {
  method <- match.arg(method)
  n <- length(y)
  stopifnot(nrow(X) == n)

  ## Combine cell-level weights into observation-level weights.
  ##
  ## We avoid the old geometric-mean rule ( exp(mean(log(pmax(w, 1e-10)))) ),
  ## which penalises a row with one clean-zero predictor weight roughly
  ## twenty-fold and thus double-counts with lmrob's own psi-weighting.
  ## Instead, a row is kept at full weight when the MAJORITY of its
  ## predictor cells are clean; if not, its weight is the mean of the
  ## clean-cell weights (so a mostly-contaminated row is attenuated but
  ## not crushed).
  obs_weights <- rep(1, n)
  if (!is.null(weights)) {
    w_resp <- weights$w_response
    w_pred <- as.matrix(weights$w_predictors)
    if (ncol(w_pred) > 0L) {
      frac_clean <- rowMeans(w_pred > 0.5)
      row_w <- ifelse(frac_clean > 0.5, 1, rowMeans(w_pred))
      obs_weights <- w_resp * row_w
    } else {
      obs_weights <- w_resp
    }
  }

  if (method == "mm") {
    fit <- .fit_mm(y, X, obs_weights, ...)
  } else if (method == "robust_cart") {
    fit <- .fit_robust_cart(y, X, "continuous", obs_weights, ...)
  } else {
    stop("Unknown method for fit_robust_continuous: ", method)
  }
  fit
}

#' Generate synthetic continuous values from a robust fit
#'
#' Draws from the fitted robust model with added residual noise.
#' Supports both direct drawing (normal residuals) and predictive
#' mean matching (PMM).
#'
#' @param fit object of class \code{"robsynth_fit_cont"} as returned
#'   by \code{\link{fit_robust_continuous}}
#' @param X_new data.frame of predictors (from synthetic data)
#' @param uncertainty how to introduce synthesis noise:
#'   \code{"normal"} (default) adds \eqn{N(0, \hat\sigma^2)};
#'   \code{"pmm"} uses predictive mean matching.
#' @param donors number of PMM donors. Default: 5.
#'
#' @return numeric vector of synthetic values
#' @author Matthias Templ
#' @export
#' @examples
#' \dontrun{
#' fit <- fit_robust_continuous(iris$Sepal.Width,
#'                              iris[, "Sepal.Length", drop = FALSE])
#' y_new <- generate_robust_continuous(fit, iris[, "Sepal.Length", drop = FALSE])
#' }
generate_robust_continuous <- function(fit, X_new,
                                       uncertainty = c("normal", "pmm"),
                                       donors = 5L) {
  uncertainty <- match.arg(uncertainty)
  n_new <- nrow(X_new)

  yhat_new <- stats::predict(fit$model, newdata = X_new)

  if (uncertainty == "pmm") {
    return(pmm_draw(fit$y_obs, fit$yhat_obs, yhat_new, donors = donors))
  }

  ## normal residual noise
  yhat_new + rnorm(n_new, mean = 0, sd = fit$sigma)
}

#' Fit a robust model for a categorical variable
#'
#' Fits a weighted multinomial logistic regression for categorical
#' response synthesis. Uses \code{nnet::multinom} with observation-level
#' weights derived from cell-level contamination weights.
#'
#' @param y factor or character response vector
#' @param X data.frame of predictors
#' @param weights optional cell-level weights (same structure as in
#'   \code{\link{fit_robust_continuous}})
#' @param ... additional arguments passed to \code{nnet::multinom}
#'
#' @return A list of class \code{"robsynth_fit_cat"} with components:
#'   \item{model}{the fitted multinom object}
#'   \item{levels}{factor levels of the response}
#'
#' @author Matthias Templ
#' @export
#' @importFrom nnet multinom
#' @examples
#' \dontrun{
#' fit <- fit_robust_categorical(iris$Species,
#'                               iris[, 1:2, drop = FALSE])
#' }
fit_robust_categorical <- function(y, X, weights = NULL, ...) {
  n <- length(y)
  stopifnot(nrow(X) == n)

  if (!is.factor(y)) y <- as.factor(y)
  lvls <- levels(y)

  ## Same combination rule as fit_robust_continuous — majority-clean
  ## survives at full weight, otherwise the mean of clean-cell weights.
  obs_weights <- rep(1, n)
  if (!is.null(weights)) {
    w_resp <- weights$w_response
    w_pred <- as.matrix(weights$w_predictors)
    if (ncol(w_pred) > 0L) {
      frac_clean <- rowMeans(w_pred > 0.5)
      row_w <- ifelse(frac_clean > 0.5, 1, rowMeans(w_pred))
      obs_weights <- w_resp * row_w
    } else {
      obs_weights <- w_resp
    }
  }

  df <- cbind(data.frame(.y = y), X)
  fit <- nnet::multinom(.y ~ ., data = df, weights = obs_weights,
                        trace = FALSE, ...)

  structure(
    list(model = fit, levels = lvls),
    class = "robsynth_fit_cat"
  )
}

#' Generate synthetic categorical values from a robust fit
#'
#' Draws from predicted class probabilities of the fitted weighted
#' multinomial model.
#'
#' @param fit object of class \code{"robsynth_fit_cat"} as returned
#'   by \code{\link{fit_robust_categorical}}
#' @param X_new data.frame of predictors (from synthetic data)
#'
#' @return factor vector of synthetic values
#' @author Matthias Templ
#' @export
#' @examples
#' \dontrun{
#' fit <- fit_robust_categorical(iris$Species,
#'                               iris[, 1:2, drop = FALSE])
#' y_new <- generate_robust_categorical(fit, iris[, 1:2, drop = FALSE])
#' table(y_new)
#' }
generate_robust_categorical <- function(fit, X_new) {
  probs <- stats::predict(fit$model, newdata = X_new, type = "probs")

  ## nnet::multinom returns a vector (not matrix) for 2-class problems
  if (is.null(dim(probs))) {
    probs <- cbind(1 - probs, probs)
    colnames(probs) <- fit$levels
  }

  n_new <- nrow(probs)
  drawn <- character(n_new)
  for (i in seq_len(n_new)) {
    p <- probs[i, ]
    p <- pmax(p, 0)
    p <- p / sum(p)
    drawn[i] <- sample(fit$levels, 1L, prob = p)
  }
  factor(drawn, levels = fit$levels)
}


# ---- internal fitting functions -------------------------------------------

#' @keywords internal
.fit_mm <- function(y, X, obs_weights, ...) {
  df <- cbind(data.frame(.y = y), X)

  ## robustbase::lmrob with MM-estimator
  fit <- robustbase::lmrob(.y ~ ., data = df, weights = obs_weights,
                           setting = "KS2014", ...)

  sigma_hat <- fit$scale
  yhat_obs  <- stats::fitted(fit)

  structure(
    list(
      model    = fit,
      method   = "mm",
      sigma    = sigma_hat,
      y_obs    = y,
      yhat_obs = yhat_obs
    ),
    class = "robsynth_fit_cont"
  )
}

# .fit_robust_cart removed — use version in robust_nonparametric.R

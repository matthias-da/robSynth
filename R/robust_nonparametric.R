#' Robust nonparametric synthesis methods
#'
#' Provides robust CART, random forest, and XGBoost synthesis that
#' addresses the y-outlier vulnerability of standard tree-based methods.
#'
#' @name robust_nonparametric
#' @keywords internal
NULL


# ==========================================================================
# ROBUST CART: median leaves + MAD scale
# ==========================================================================

#' Fit robust CART for synthesis
#'
#' Fits an rpart tree but replaces leaf-node means with medians
#' (robust to y-outliers). Residual scale uses MAD instead of SD.
#'
#' @param y numeric or factor response
#' @param X data.frame of predictors
#' @param var_type "continuous" or "categorical"
#' @param obs_weights observation weights
#' @param ... passed to rpart
#' @return fit object compatible with .generate_dispatch
#' @keywords internal
.fit_robust_cart <- function(y, X, var_type, obs_weights = NULL, ...) {
  df <- cbind(data.frame(.y = y), X)
  n <- nrow(df)
  if (is.null(obs_weights)) obs_weights <- rep(1, n)

  if (var_type == "continuous") {
    if (!requireNamespace("partykit", quietly = TRUE))
      stop("Package 'partykit' required for method 'robust_cart'; ",
           "install.packages('partykit').")
    tree <- rpart::rpart(.y ~ ., data = df, weights = obs_weights, ...)
    # Use partykit node IDs for leaves so the keys match at predict time.
    pt <- partykit::as.party(tree)
    leaf_ids <- as.integer(stats::predict(pt, newdata = df, type = "node"))
    unique_leaves <- unique(leaf_ids)
    leaf_medians <- vapply(unique_leaves, function(lid) {
      idx <- which(leaf_ids == lid)
      weighted_median(y[idx], obs_weights[idx])
    }, numeric(1))
    names(leaf_medians) <- as.character(unique_leaves)

    yhat_obs <- leaf_medians[as.character(leaf_ids)]
    sigma <- mad(y - yhat_obs)
    if (sigma < .Machine$double.eps) sigma <- sd(y - yhat_obs)

    structure(list(
      model = tree,
      method = "robust_cart",
      var_type = "continuous",
      leaf_medians = leaf_medians,
      sigma = sigma,
      y_obs = y,
      yhat_obs = as.numeric(yhat_obs)
    ), class = "robsynth_fit_cont")

  } else {
    # Categorical: standard CART but with weighted modal probabilities
    tree <- rpart::rpart(.y ~ ., data = df, method = "class",
                         weights = obs_weights, ...)
    lvls <- if (is.factor(y)) levels(y) else sort(unique(y))

    structure(list(
      model = tree,
      method = "robust_cart",
      var_type = "categorical",
      levels = lvls,
      n_levels = length(lvls)
    ), class = "robsynth_fit_cat")
  }
}

#' Generate from robust CART
#'
#' Predicts the leaf node of each new observation and returns the stored
#' leaf median (plus Gaussian noise with MAD scale).  Leaf-node assignment
#' uses \code{partykit::as.party} when available and falls back to an
#' rpart \code{$frame} lookup otherwise, so the median-leaf logic is
#' preserved rather than round-tripped through rpart's mean predictions.
#'
#' @keywords internal
.generate_robust_cart <- function(fit, X_new, n) {
  if (fit$var_type == "continuous") {
    df_new <- cbind(data.frame(.y = rep(0, n)), X_new)
    leaf_ids <- .predict_rpart_nodes(fit$model, df_new)
    yhat <- fit$leaf_medians[as.character(leaf_ids)]
    if (anyNA(yhat)) {
      # Safety net: an unseen leaf id (should not occur, but handle it)
      fallback <- stats::predict(fit$model, newdata = df_new)
      yhat[is.na(yhat)] <- fallback[is.na(yhat)]
    }
    return(as.numeric(yhat) + rnorm(n, 0, fit$sigma))
  } else {
    df_new <- cbind(data.frame(.y = factor(fit$levels[1],
                                            levels = fit$levels)), X_new)
    pred <- stats::predict(fit$model, newdata = df_new, type = "class")
    return(factor(as.character(pred), levels = fit$levels))
  }
}

#' Predict leaf node IDs for an rpart fit on new data
#'
#' Returns integer leaf IDs for each row of \code{newdata}, aligned to
#' the names used to store \code{leaf_medians} at fit time.  Requires
#' \pkg{partykit} (already in Suggests for \code{ctree}); errors if
#' absent with an informative message.
#'
#' @keywords internal
.predict_rpart_nodes <- function(tree, newdata) {
  if (!requireNamespace("partykit", quietly = TRUE))
    stop("Package 'partykit' required for method 'robust_cart' prediction; ",
         "install.packages('partykit').")
  pt <- partykit::as.party(tree)
  as.integer(stats::predict(pt, newdata = newdata, type = "node"))
}


# ==========================================================================
# ROBUST RANDOM FOREST: weighted leaf sampling
# ==========================================================================

#' Fit robust random forest for synthesis
#'
#' Uses ranger with case weights derived from robust Mahalanobis distances.
#' Observations far from the robust center receive lower weight, reducing
#' the influence of y-outliers on leaf predictions.
#'
#' @param y numeric or factor response
#' @param X data.frame of predictors
#' @param var_type "continuous" or "categorical"
#' @param obs_weights observation weights (combined survey + robustness)
#' @param ... passed to ranger
#' @return fit object
#' @keywords internal
.fit_robust_rf <- function(y, X, var_type, obs_weights = NULL, ...) {
  if (!requireNamespace("ranger", quietly = TRUE))
    stop("Package 'ranger' required for method 'robust_rf'")

  df <- cbind(data.frame(.y = y), X)
  n <- nrow(df)
  if (is.null(obs_weights)) obs_weights <- rep(1, n)

  if (var_type == "continuous") {
    # Two-pass: fit initial forest, compute OOB-residual robustness weights,
    # then refit.  This downweights rows that the model itself cannot
    # predict well — i.e. true y-outliers conditional on X — instead of
    # flagging marginal y extremes that are legitimate signal.
    fit0 <- ranger::ranger(.y ~ ., data = df,
                           case.weights = obs_weights,
                           keep.inbag = TRUE, ...)
    resid_oob <- y - fit0$predictions
    s_resid <- mad(resid_oob)
    if (s_resid < .Machine$double.eps) s_resid <- sd(resid_oob)
    if (s_resid > 0) {
      z_resid <- abs(resid_oob) / s_resid
      rob_weights <- ifelse(z_resid > 3, 0.1, 1.0)
      combined_weights <- obs_weights * rob_weights
    } else {
      combined_weights <- obs_weights
    }

    fit <- ranger::ranger(.y ~ ., data = df,
                          case.weights = combined_weights,
                          keep.inbag = TRUE, ...)
    yhat_obs <- fit$predictions
    sigma <- mad(y - yhat_obs)
    if (sigma < .Machine$double.eps) sigma <- sd(y - yhat_obs)

    structure(list(
      model = fit,
      method = "robust_rf",
      var_type = "continuous",
      sigma = sigma,
      y_obs = y,
      yhat_obs = yhat_obs
    ), class = "robsynth_fit_cont")

  } else {
    if (!is.factor(y)) y <- factor(y)
    df$.y <- y
    fit <- ranger::ranger(.y ~ ., data = df,
                          case.weights = obs_weights,
                          probability = TRUE, ...)
    lvls <- levels(y)

    structure(list(
      model = fit,
      method = "robust_rf",
      var_type = "categorical",
      levels = lvls,
      n_levels = length(lvls)
    ), class = "robsynth_fit_cat")
  }
}

#' Generate from robust RF
#' @keywords internal
.generate_robust_rf <- function(fit, X_new, n) {
  if (fit$var_type == "continuous") {
    pred <- stats::predict(fit$model, data = X_new)$predictions
    # Add noise (PMM-style from observed residuals would be better,
    # but normal noise is simpler and safer for disclosure)
    return(pred + rnorm(n, 0, fit$sigma))
  } else {
    probs <- stats::predict(fit$model, data = X_new)$predictions
    drawn <- apply(probs, 1, function(p) {
      p <- pmax(p, 0); p <- p / sum(p)
      sample(fit$levels, 1, prob = p)
    })
    return(factor(drawn, levels = fit$levels))
  }
}


# ==========================================================================
# ROBUST XGBOOST: Huber loss
# ==========================================================================

#' Fit robust XGBoost for synthesis
#'
#' Uses XGBoost with pseudo-Huber loss (\code{reg:pseudohubererror})
#' for continuous variables, providing bounded influence on the loss
#' function.  For categorical variables, uses \code{multi:softprob}
#' with sample weights.
#'
#' @param y numeric or factor response
#' @param X data.frame of predictors
#' @param var_type "continuous" or "categorical"
#' @param obs_weights observation weights
#' @param huber_delta Huber loss delta parameter (default 1.0).
#'   Smaller = more robust, larger = more efficient.
#' @param nrounds number of boosting rounds (default 100)
#' @param ... passed to xgb.train
#' @return fit object
#' @keywords internal
.fit_robust_xgb <- function(y, X, var_type, obs_weights = NULL,
                            huber_delta = 1.0, nrounds = 100L, ...) {
  if (!requireNamespace("xgboost", quietly = TRUE))
    stop("Package 'xgboost' required for method 'robust_xgboost'")

  n <- length(y)
  if (is.null(obs_weights)) obs_weights <- rep(1, n)

  # Training design matrix; record xlev and column names so new-data
  # predictions use the same columns even when a factor level is absent
  # or a new level appears in X_new.
  xlev <- .capture_xlev(X)
  mf_train <- stats::model.frame(~ ., data = X, xlev = xlev)
  X_mat <- stats::model.matrix(~ . - 1, data = mf_train)
  train_cols <- colnames(X_mat)

  if (var_type == "continuous") {
    dtrain <- xgboost::xgb.DMatrix(X_mat, label = y, weight = obs_weights)
    params <- list(
      objective = "reg:pseudohubererror",
      huber_slope = huber_delta,
      eta = 0.1,
      max_depth = 6,
      subsample = 0.8
    )
    fit <- xgboost::xgb.train(params, dtrain, nrounds = nrounds,
                               verbose = 0, ...)
    yhat_obs <- stats::predict(fit, dtrain)
    sigma <- mad(y - yhat_obs)
    if (sigma < .Machine$double.eps) sigma <- sd(y - yhat_obs)

    structure(list(
      model = fit,
      method = "robust_xgboost",
      var_type = "continuous",
      sigma = sigma,
      y_obs = y,
      yhat_obs = yhat_obs,
      xlev = xlev,
      train_cols = train_cols
    ), class = "robsynth_fit_cont")

  } else {
    if (!is.factor(y)) y <- factor(y)
    lvls <- levels(y)
    y_num <- as.integer(y) - 1L  # 0-indexed for xgboost
    n_class <- length(lvls)

    dtrain <- xgboost::xgb.DMatrix(X_mat, label = y_num, weight = obs_weights)
    params <- list(
      objective = "multi:softprob",
      num_class = n_class,
      eta = 0.1,
      max_depth = 6,
      subsample = 0.8
    )
    fit <- xgboost::xgb.train(params, dtrain, nrounds = nrounds,
                               verbose = 0, ...)

    structure(list(
      model = fit,
      method = "robust_xgboost",
      var_type = "categorical",
      levels = lvls,
      n_levels = n_class,
      xlev = xlev,
      train_cols = train_cols
    ), class = "robsynth_fit_cat")
  }
}

#' Capture factor levels from a predictor frame for xlev re-use
#'
#' @keywords internal
.capture_xlev <- function(X) {
  is_fac <- vapply(X, is.factor, logical(1))
  lapply(X[, is_fac, drop = FALSE], levels)
}

#' Build a column-aligned design matrix for xgboost on new data
#'
#' Coerces unseen factor levels to NA (so their dummies are zero), drops
#' any columns not present at training time, and pads missing training
#' columns with zeros.  Guarantees the predict-time matrix has the same
#' columns in the same order as the fit-time matrix.
#'
#' @keywords internal
.align_xgb_matrix <- function(X_new, xlev, train_cols) {
  for (nm in names(xlev)) {
    if (nm %in% names(X_new)) {
      X_new[[nm]] <- factor(X_new[[nm]], levels = xlev[[nm]])
    }
  }
  mf <- stats::model.frame(~ ., data = X_new, xlev = xlev,
                            na.action = stats::na.pass)
  X_new_mat <- stats::model.matrix(~ . - 1, data = mf)
  out <- matrix(0, nrow = nrow(X_new_mat), ncol = length(train_cols),
                dimnames = list(NULL, train_cols))
  shared <- intersect(colnames(X_new_mat), train_cols)
  if (length(shared) > 0) out[, shared] <- X_new_mat[, shared, drop = FALSE]
  out
}

#' Generate from robust XGBoost
#' @keywords internal
.generate_robust_xgb <- function(fit, X_new, n) {
  X_mat <- .align_xgb_matrix(X_new, fit$xlev, fit$train_cols)

  if (fit$var_type == "continuous") {
    dnew <- xgboost::xgb.DMatrix(X_mat)
    pred <- stats::predict(fit$model, dnew)
    return(pred + rnorm(n, 0, fit$sigma))
  } else {
    dnew <- xgboost::xgb.DMatrix(X_mat)
    probs_raw <- stats::predict(fit$model, dnew)
    probs <- matrix(probs_raw, ncol = fit$n_levels, byrow = TRUE)
    drawn <- apply(probs, 1, function(p) {
      p <- pmax(p, 0); p <- p / sum(p)
      sample(fit$levels, 1, prob = p)
    })
    return(factor(drawn, levels = fit$levels))
  }
}

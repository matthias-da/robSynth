#' Utility assessment for synthetic data
#'
#' Unified interface to compute all utility metrics comparing original
#' and synthetic data.  Handles multiple synthetic copies (\code{m > 1})
#' by averaging across copies.
#'
#' @param object a \code{"robsynth"} object, or a synthetic data.frame
#' @param original the original data.frame (required if \code{object}
#'   is a data.frame, optional if \code{object} is a robsynth object
#'   with \code{$original} stored)
#' @param metrics character vector of metrics to compute.  Options:
#'   \code{"pMSE"}, \code{"KS"}, \code{"regression"}, \code{"correlation"},
#'   \code{"overlap"}.  Default: all.
#' @param ... additional arguments passed to metric functions
#'
#' @return An S3 object of class \code{"robsynth_utility"} with
#'   components for each metric and a formatted print method.
#'
#' @export
#' @examples
#' \dontrun{
#' res <- robsynth(iris)
#' utility(res, iris)
#' }
utility <- function(object, original = NULL, metrics = NULL, ...) {
  UseMethod("utility")
}

#' @export
utility.robsynth <- function(object, original = NULL,
                             metrics = NULL, ...) {
  if (is.null(original)) {
    original <- object$original
    if (is.null(original))
      stop("'original' data must be provided (or stored in the robsynth object)")
  }

  if (is.null(metrics)) {
    metrics <- c("pMSE", "KS", "regression", "correlation")
  }

  synth_data <- if (object$m == 1L) list(object$synth) else object$synth

  ## compute metrics for each copy
  all_results <- lapply(synth_data, function(s) {
    synth_utility(original, s, metrics = metrics, ...)
  })

  ## average across copies
  result <- all_results[[1L]]
  if (length(all_results) > 1L) {
    # Average scalar metrics
    for (nm in c("pMSE", "correlation")) {
      if (!is.null(result[[nm]])) {
        vals <- vapply(all_results, function(r) r[[nm]], numeric(1))
        result[[nm]] <- mean(vals, na.rm = TRUE)
        attr(result[[nm]], "sd") <- sd(vals, na.rm = TRUE)
      }
    }
  }

  result$m <- object$m
  result$method <- object$method
  class(result) <- "robsynth_utility"
  result
}

#' @export
utility.data.frame <- function(object, original = NULL,
                               metrics = NULL, ...) {
  if (is.null(original)) stop("'original' must be provided")
  if (is.null(metrics)) metrics <- c("pMSE", "KS", "regression", "correlation")
  result <- synth_utility(original, object, metrics = metrics, ...)
  result$m <- 1L
  class(result) <- "robsynth_utility"
  result
}

#' @export
print.robsynth_utility <- function(x, ...) {
  cat("robSynth Utility Assessment\n")
  cat("===========================\n\n")

  if (!is.null(x$pMSE)) {
    sd_str <- if (!is.null(attr(x$pMSE, "sd")))
      sprintf(" (SD: %.6f)", attr(x$pMSE, "sd")) else ""
    cat(sprintf("  pMSE:         %.6f%s\n", x$pMSE, sd_str))
  }

  if (!is.null(x$KS)) {
    if (is.numeric(x$KS) && length(x$KS) > 1L) {
      cat(sprintf("  Mean KS:      %.4f\n", mean(x$KS)))
      cat("  Per-variable KS:\n")
      for (i in seq_along(x$KS)) {
        cat(sprintf("    %-12s %.4f\n", names(x$KS)[i], x$KS[i]))
      }
    } else if (is.numeric(x$KS)) {
      cat(sprintf("  KS:           %.4f\n", x$KS))
    }
  }

  if (!is.null(x$correlation)) {
    sd_str <- if (!is.null(attr(x$correlation, "sd")))
      sprintf(" (SD: %.4f)", attr(x$correlation, "sd")) else ""
    cat(sprintf("  Correlation:  %.4f%s  (Frobenius norm, lower=better)\n",
                x$correlation, sd_str))
  }

  if (!is.null(x$regression)) {
    cat(sprintf("  Regression:\n"))
    cat(sprintf("    Relative bias: %.4f\n", x$regression$relative_bias))
    cat(sprintf("    CI overlap:    %.3f\n", x$regression$ci_overlap))
  }

  if (x$m > 1L) cat(sprintf("\n  (averaged over m = %d copies)\n", x$m))
  invisible(x)
}


#' Compare model fit between original and synthetic data
#'
#' Fits the same regression model on original and synthetic data and
#' compares coefficient estimates.  When \code{m > 1}, applies Reiter
#' (2003) combining rules.  Optionally produces a forest plot.
#'
#' @param formula a model formula
#' @param object a \code{"robsynth"} object
#' @param original the original data.frame
#' @param plot logical: produce a coefficient comparison plot?  Default TRUE.
#' @param ... additional arguments passed to \code{lm}
#'
#' @return An S3 object of class \code{"robsynth_fit_compare"} with:
#'   \item{coef_orig}{coefficients from original data}
#'   \item{coef_synth}{coefficients from synthetic data (or combined)}
#'   \item{se_orig}{standard errors from original}
#'   \item{se_synth}{standard errors from synthetic (or combined)}
#'   \item{ci_overlap}{per-coefficient CI overlap}
#'   \item{std_diff}{standardised differences}
#'   \item{mean_abs_std_diff}{mean absolute standardised difference}
#'   \item{plot}{ggplot object (if requested)}
#'
#' @export
#' @examples
#' \dontrun{
#' res <- robsynth(iris, m = 5)
#' comp <- compare_fit(Sepal.Length ~ Sepal.Width + Petal.Length, res, iris)
#' comp$plot
#' }
compare_fit <- function(formula, object, original, plot = TRUE, ...) {
  stopifnot(inherits(object, "robsynth"))

  ## fit on original
  fit_orig <- stats::lm(formula, data = original, ...)
  b_orig <- stats::coef(fit_orig)
  se_orig <- summary(fit_orig)$coefficients[, 2]
  ci_orig <- stats::confint(fit_orig)

  ## fit on synthetic (combine if m > 1)
  synth_data <- if (object$m == 1L) list(object$synth) else object$synth
  fits_synth <- lapply(synth_data, function(s)
    stats::lm(formula, data = s, ...))

  if (object$m == 1L) {
    b_synth <- stats::coef(fits_synth[[1L]])
    se_synth <- summary(fits_synth[[1L]])$coefficients[, 2]
    ci_synth <- stats::confint(fits_synth[[1L]])
  } else {
    ## Reiter combining rules
    combined <- .combine_fits(fits_synth, object$m, nrow(original))
    b_synth <- combined$coefficients
    se_synth <- combined$se
    ci_synth <- cbind(b_synth - 1.96 * se_synth,
                      b_synth + 1.96 * se_synth)
    rownames(ci_synth) <- names(b_synth)
  }

  ## align names
  common <- intersect(names(b_orig), names(b_synth))
  b_orig <- b_orig[common]; se_orig <- se_orig[common]
  b_synth <- b_synth[common]; se_synth <- se_synth[common]
  ci_orig <- ci_orig[common, , drop = FALSE]
  ci_synth <- ci_synth[common, , drop = FALSE]

  ## CI overlap
  ci_ov <- vapply(common, function(nm) {
    lo <- max(ci_orig[nm, 1], ci_synth[nm, 1])
    hi <- min(ci_orig[nm, 2], ci_synth[nm, 2])
    avg_w <- ((ci_orig[nm, 2] - ci_orig[nm, 1]) +
              (ci_synth[nm, 2] - ci_synth[nm, 1])) / 2
    if (avg_w < 1e-10) return(NA_real_)
    max(0, hi - lo) / avg_w
  }, numeric(1))

  ## standardised differences
  std_diff <- (b_synth - b_orig) / sqrt(se_orig^2 + se_synth^2)

  ## plot
  p <- NULL
  if (plot && requireNamespace("ggplot2", quietly = TRUE)) {
    ci_data <- rbind(
      data.frame(term = common, estimate = b_orig,
                 lower = ci_orig[, 1], upper = ci_orig[, 2],
                 source = "Original", stringsAsFactors = FALSE),
      data.frame(term = common, estimate = b_synth,
                 lower = ci_synth[, 1], upper = ci_synth[, 2],
                 source = "Synthetic", stringsAsFactors = FALSE))
    ci_data <- ci_data[ci_data$term != "(Intercept)", ]
    ci_data$source <- factor(ci_data$source, levels = c("Original", "Synthetic"))

    p <- ggplot2::ggplot(ci_data,
           ggplot2::aes(x = .data$estimate, y = .data$source,
                        colour = .data$source)) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::geom_errorbarh(
        ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
        height = 0.3, linewidth = 0.7) +
      ggplot2::facet_wrap(~ term, scales = "free_x") +
      ggplot2::scale_colour_manual(
        values = c("Original" = "#000000", "Synthetic" = "#E69F00")) +
      ggplot2::labs(x = "Coefficient (95% CI)", y = NULL, colour = NULL) +
      ggplot2::theme_bw(base_size = 11) +
      ggplot2::theme(legend.position = "bottom")
  }

  structure(
    list(
      coef_orig = b_orig,
      coef_synth = b_synth,
      se_orig = se_orig,
      se_synth = se_synth,
      ci_overlap = ci_ov,
      std_diff = std_diff,
      mean_abs_std_diff = mean(abs(std_diff[common != "(Intercept)"]),
                               na.rm = TRUE),
      m = object$m,
      plot = p
    ),
    class = "robsynth_fit_compare"
  )
}

#' @export
print.robsynth_fit_compare <- function(x, ...) {
  cat("robSynth Model Comparison\n")
  cat("=========================\n\n")
  cat("Coefficient comparison (original vs synthetic):\n\n")

  tab <- data.frame(
    Original = round(x$coef_orig, 4),
    Synthetic = round(x$coef_synth, 4),
    CI_overlap = round(x$ci_overlap, 3),
    Std_diff = round(x$std_diff, 3)
  )
  print(tab)

  cat(sprintf("\nMean |standardised difference|: %.3f\n",
              x$mean_abs_std_diff))
  cat(sprintf("Mean CI overlap: %.3f\n",
              mean(x$ci_overlap, na.rm = TRUE)))
  if (x$m > 1L) cat(sprintf("(synthetic estimates combined over m = %d)\n", x$m))
  invisible(x)
}


#' Disclosure risk assessment
#'
#' Computes one or more disclosure-risk measures for synthetic data.
#' When the \pkg{riskutility} package is installed the following
#' measures are available:
#' \describe{
#'   \item{\code{"rapid"}}{Risk of Attribute Prediction-Induced Disclosure
#'     (Thees, Mueller & Templ, 2025): trains a predictive attacker on
#'     the synthetic data and scores its success on the original.}
#'   \item{\code{"dcap"}}{Differential Correct Attribution Probability —
#'     exact- or Gower-matching attack on the key variables with sensitive
#'     attribute recovery.}
#'   \item{\code{"dcr"}}{Distance to Closest Record — per-record nearest
#'     distance with a holdout null.}
#'   \item{\code{"delta_presence"}}{\eqn{\delta}-presence: the proportion
#'     of original records whose inclusion can be inferred.}
#'   \item{\code{"domias"}}{Density-based membership inference attack:
#'     AUC of train-vs-holdout separation by density ratio.}
#'   \item{\code{"drisk"}}{\pkg{sdcMicro}-style disclosure risk on
#'     key-vars (interval / robust-Mahalanobis).}
#' }
#' The built-in \code{"distance"} measure is a simple robust-MAD nearest-
#' neighbour distance that does not require \pkg{riskutility}.  The
#' special value \code{"all"} expands to every available measure whose
#' dependencies are satisfied.
#'
#' @param object a \code{"robsynth"} object or synthetic data.frame
#' @param original the original data.frame
#' @param key_vars character vector of quasi-identifier column names
#'   (required by \code{rapid}, \code{dcap}, \code{delta_presence},
#'   \code{drisk}; ignored by the others)
#' @param target_var name of the sensitive attribute (required by
#'   \code{rapid}, \code{dcap})
#' @param method character vector of risk measures to compute; see
#'   Details.  Also accepts \code{"both"} (alias for
#'   \code{c("rapid", "distance")}) and \code{"all"}.
#' @param ... additional arguments forwarded to the underlying
#'   \pkg{riskutility} function.  When several riskutility measures are
#'   requested, \code{...} arguments are passed to every call; use a
#'   single-method call for per-function argument control.
#'
#' @return An S3 object of class \code{"robsynth_disclosure"} with one
#'   list component per requested measure plus a print method.
#'
#' @references
#' Thees M., Mueller J., Templ M. (2025).  RAPID: A random-forest attack
#'   for disclosure-risk assessment of synthetic data.  arXiv:2602.09235.
#'
#' @export
#' @examples
#' \dontrun{
#' res <- robsynth(iris)
#' disclosure(res, iris,
#'   key_vars = c("Sepal.Width", "Petal.Length"),
#'   target_var = "Sepal.Length",
#'   method = c("rapid", "dcap", "domias"))
#' }
disclosure <- function(object, original, key_vars, target_var,
                       method = c("rapid", "distance"), ...) {
  valid <- c("rapid", "distance", "dcap", "dcr",
             "delta_presence", "domias", "drisk",
             "both", "all")
  method <- unique(method)
  bad <- setdiff(method, valid)
  if (length(bad) > 0L)
    stop("Unknown disclosure method(s): ", paste(bad, collapse = ", "),
         call. = FALSE)
  if ("both" %in% method) method <- union(setdiff(method, "both"),
                                          c("rapid", "distance"))
  if ("all"  %in% method) method <- setdiff(valid, c("both", "all"))

  synth <- if (inherits(object, "robsynth")) {
    if (object$m == 1L) object$synth else object$synth[[1L]]
  } else {
    object
  }

  ru_ok <- requireNamespace("riskutility", quietly = TRUE)
  result <- list()
  dots   <- list(...)

  ## ---- RAPID ----
  if ("rapid" %in% method) {
    if (ru_ok) {
      rap <- do.call(riskutility::rapid,
                     c(list(X = original, Y = synth,
                            key_vars = key_vars, target_var = target_var,
                            model_type = "rf", verbose = FALSE), dots))
      result$rapid <- list(score = rap$rapid,
                           n_at_risk = rap$n_at_risk,
                           n_total   = rap$n_total)
    } else {
      warning("Package 'riskutility' not available; skipping RAPID.")
    }
  }

  ## ---- dcap ----
  if ("dcap" %in% method) {
    if (ru_ok) {
      dc <- tryCatch(
        do.call(riskutility::dcap,
                c(list(X = original, Y = synth,
                       key_vars = key_vars, target_var = target_var),
                  dots)),
        error = function(e) { warning("dcap failed: ", conditionMessage(e));
                              NULL })
      if (!is.null(dc)) result$dcap <- dc
    } else warning("Package 'riskutility' not available; skipping dcap.")
  }

  ## ---- dcr ----
  if ("dcr" %in% method) {
    if (ru_ok) {
      dc <- tryCatch(
        do.call(riskutility::dcr,
                c(list(X = original, Y = synth, progress = FALSE), dots)),
        error = function(e) { warning("dcr failed: ", conditionMessage(e));
                              NULL })
      if (!is.null(dc)) result$dcr <- dc
    } else warning("Package 'riskutility' not available; skipping dcr.")
  }

  ## ---- delta_presence ----
  if ("delta_presence" %in% method) {
    if (ru_ok) {
      dp <- tryCatch(
        do.call(riskutility::delta_presence,
                c(list(X = original, Y = synth, key_vars = key_vars),
                  dots)),
        error = function(e) { warning("delta_presence failed: ",
                                       conditionMessage(e)); NULL })
      if (!is.null(dp)) result$delta_presence <- dp
    } else warning("Package 'riskutility' not available; skipping delta_presence.")
  }

  ## ---- domias ----
  if ("domias" %in% method) {
    if (ru_ok) {
      dm <- tryCatch(
        do.call(riskutility::domias,
                c(list(X = original, Y = synth), dots)),
        error = function(e) { warning("domias failed: ",
                                       conditionMessage(e)); NULL })
      if (!is.null(dm)) result$domias <- dm
    } else warning("Package 'riskutility' not available; skipping domias.")
  }

  ## ---- drisk ----
  if ("drisk" %in% method) {
    if (ru_ok) {
      dr <- tryCatch(
        do.call(riskutility::drisk,
                c(list(X = original, Y = synth, vars = key_vars), dots)),
        error = function(e) { warning("drisk failed: ",
                                       conditionMessage(e)); NULL })
      if (!is.null(dr)) result$drisk <- dr
    } else warning("Package 'riskutility' not available; skipping drisk.")
  }

  ## ---- built-in distance-based risk (numeric keys only) ----
  if ("distance" %in% method) {
    cont_vars <- names(original)[vapply(original, is.numeric, logical(1))]
    cont_vars <- intersect(cont_vars, c(key_vars, target_var))
    if (length(cont_vars) >= 2) {
      orig_mat  <- as.matrix(original[, cont_vars, drop = FALSE])
      synth_mat <- as.matrix(synth[, cont_vars, drop = FALSE])
      sc <- .mad_scales(orig_mat)
      orig_sc   <- scale(orig_mat,  center = FALSE, scale = sc)
      synth_sc  <- scale(synth_mat, center = FALSE, scale = sc)
      min_dists <- .nearest_distances(orig_sc, synth_sc)
      result$distance <- list(
        median_dist          = stats::median(min_dists),
        q05_dist             = stats::quantile(min_dists, 0.05),
        pct_below_threshold  = mean(min_dists < 0.5))
    }
  }

  result$method     <- method
  result$key_vars   <- key_vars
  result$target_var <- target_var
  class(result) <- "robsynth_disclosure"
  result
}

#' @export
print.robsynth_disclosure <- function(x, ...) {
  cat("robSynth Disclosure Risk Assessment\n")
  cat("====================================\n\n")

  if (!is.null(x$rapid)) {
    r <- x$rapid
    level <- as.character(cut(r$score,
                              breaks = c(-Inf, 0.05, 0.15, Inf),
                              labels = c("LOW", "MEDIUM", "HIGH")))
    cat(sprintf("  RAPID:           %.3f  (%s)  %d/%d records at risk\n",
                r$score, level, r$n_at_risk, r$n_total))
  }
  if (!is.null(x$dcap)) {
    cat("  DCAP:            "); print(x$dcap)
  }
  if (!is.null(x$dcr)) {
    cat("  DCR:             "); print(x$dcr)
  }
  if (!is.null(x$delta_presence)) {
    cat("  delta-presence:  "); print(x$delta_presence)
  }
  if (!is.null(x$domias)) {
    cat("  DOMIAS:          "); print(x$domias)
  }
  if (!is.null(x$drisk)) {
    cat("  drisk:           "); print(x$drisk)
  }
  if (!is.null(x$distance)) {
    d <- x$distance
    cat("  Distance-based (numeric keys only):\n")
    cat(sprintf("    median nearest: %.3f    q05: %.3f    %% < 0.5: %.1f%%\n",
                d$median_dist, d$q05_dist, 100 * d$pct_below_threshold))
  }

  cat(sprintf("\n  Key vars: %s\n", paste(x$key_vars, collapse = ", ")))
  cat(sprintf("  Target:   %s\n", x$target_var))
  invisible(x)
}

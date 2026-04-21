#' Synthetic data utility metrics
#'
#' Computes utility metrics comparing original and synthetic data.
#' Utility measures how well the synthetic data preserves statistical
#' properties of the original.
#'
#' @param orig original data.frame
#' @param synth synthetic data.frame (same columns as \code{orig})
#' @param metrics character vector of metrics to compute. Default:
#'   all available. Options: \code{"pMSE"}, \code{"KS"},
#'   \code{"regression"}, \code{"correlation"}, \code{"overlap"}.
#'
#' @return A list of class \code{"synth_utility"} with one component
#'   per requested metric:
#'   \item{pMSE}{propensity score MSE (lower = more similar)}
#'   \item{KS}{per-variable Kolmogorov-Smirnov statistic (continuous
#'     only)}
#'   \item{regression}{comparison of regression coefficients (relative
#'     bias and confidence interval overlap)}
#'   \item{correlation}{Frobenius norm distance between correlation
#'     matrices}
#'   \item{overlap}{per-variable empirical distribution overlap}
#'
#' @details
#' The \strong{pMSE} (propensity score mean squared error) trains a
#' logistic regression to distinguish original from synthetic records.
#' If the synthetic data is good, the classifier cannot distinguish
#' them, and pMSE approaches \eqn{c(1-c)/n} where
#' \eqn{c = n_\text{synth} / (n_\text{orig} + n_\text{synth})}.
#' The raw value is returned here; for cross-sample-size comparison
#' use the null-scaled pMSE of Snoke and Slavkovic (2018),
#' \eqn{pMSE / \mathrm{Var}(pMSE \mid H_0)}.
#'
#' @references
#' Snoke J., Slavkovic A. (2018).  pMSE mechanism: Differentially
#'   private synthetic data with maximal distributional similarity.
#'   In \emph{Privacy in Statistical Databases}, 138-159.  Springer.
#'
#' Reiter J. P. (2005).  Using CART to generate partially synthetic
#'   public use microdata.  \emph{Journal of Official Statistics},
#'   21(3), 441-462.
#'
#' @author Matthias Templ
#' @export
#' @examples
#' \dontrun{
#' data(CrohnD, package = "robustbase")
#' dat   <- CrohnD[, -1]
#' synth <- dat[sample(nrow(dat), replace = TRUE), ]
#' synth_utility(dat, synth)
#' }
synth_utility <- function(orig, synth,
                          metrics = c("pMSE", "KS", "regression",
                                      "correlation", "overlap")) {
  metrics <- match.arg(metrics, several.ok = TRUE)
  stopifnot(is.data.frame(orig), is.data.frame(synth))
  stopifnot(ncol(orig) == ncol(synth))
  stopifnot(all(names(orig) == names(synth)))

  var_types <- detect_var_types(orig)
  results <- list()

  ## --- pMSE: propensity score MSE ---
  if ("pMSE" %in% metrics) {
    results$pMSE <- .compute_pMSE(orig, synth)
  }

  ## --- KS: Kolmogorov-Smirnov ---
  if ("KS" %in% metrics) {
    cont_vars <- names(var_types)[var_types == "continuous"]
    ks_stats <- stats::setNames(numeric(length(cont_vars)), cont_vars)
    for (v in cont_vars) {
      ks_stats[v] <- suppressWarnings(
        ks.test(orig[[v]], synth[[v]])$statistic
      )
    }
    results$KS <- ks_stats
  }

  ## --- regression coefficient comparison ---
  if ("regression" %in% metrics) {
    results$regression <- .compare_regression(orig, synth, var_types)
  }

  ## --- correlation matrix distance ---
  if ("correlation" %in% metrics) {
    cont_vars <- names(var_types)[var_types == "continuous"]
    if (length(cont_vars) >= 2L) {
      cor_orig  <- cor(orig[, cont_vars, drop = FALSE], use = "pairwise")
      cor_synth <- cor(synth[, cont_vars, drop = FALSE], use = "pairwise")
      results$correlation <- sqrt(sum((cor_orig - cor_synth)^2)) /
                             length(cont_vars)
    } else {
      results$correlation <- NA_real_
    }
  }

  ## --- empirical distribution overlap ---
  if ("overlap" %in% metrics) {
    results$overlap <- .compute_overlap(orig, synth, var_types)
  }

  structure(results, class = "synth_utility")
}

#' Synthetic data risk metrics
#'
#' Computes disclosure risk metrics for synthetic data. Risk measures
#' how much an attacker could learn about original records from the
#' synthetic data.
#'
#' @param orig original data.frame
#' @param synth synthetic data.frame (same columns as \code{orig})
#' @param metrics character vector of metrics to compute. Default:
#'   all available. Options: \code{"k_anonymity"},
#'   \code{"distance"}, \code{"membership_inference"}, \code{"TCAP"}.
#' @param key_vars character vector of key (quasi-identifier) variable
#'   names. Default: all variables.
#' @param sensitive_var name of the sensitive variable (for TCAP).
#'   Default: last column.
#'
#' @return A list of class \code{"synth_risk"} with one component
#'   per requested metric:
#'   \item{k_anonymity}{list with \code{synth_only} (k-anonymity
#'     on synthetic data alone) and \code{cross_match} (minimum
#'     equivalence class size when matching original to synthetic;
#'     Issue 14)}
#'   \item{distance}{distribution of closest-record distances
#'     between original and synthetic}
#'   \item{membership_inference}{AUC of member vs. non-member
#'     classification (Issue 15)}
#'   \item{TCAP}{Target Correct Attribution Probability (Issue 15)}
#'
#' @author Matthias Templ
#' @export
#' @examples
#' \dontrun{
#' data(CrohnD, package = "robustbase")
#' dat   <- CrohnD[, -1]
#' synth <- dat[sample(nrow(dat), replace = TRUE), ]
#' synth_risk(dat, synth)
#' }
synth_risk <- function(orig, synth,
                       metrics = c("k_anonymity", "distance",
                                   "membership_inference", "TCAP"),
                       key_vars = NULL,
                       sensitive_var = NULL) {
  metrics <- match.arg(metrics, several.ok = TRUE)
  stopifnot(is.data.frame(orig), is.data.frame(synth))

  if (is.null(key_vars)) key_vars <- names(orig)
  if (is.null(sensitive_var)) sensitive_var <- names(orig)[ncol(orig)]

  var_types <- detect_var_types(orig)
  results <- list()

  ## --- k-anonymity ---
  if ("k_anonymity" %in% metrics) {
    results$k_anonymity <- .compute_k_anonymity(orig, synth, key_vars)
  }

  ## --- distance-based disclosure ---
  if ("distance" %in% metrics) {
    results$distance <- .compute_distance_risk(orig, synth, var_types)
  }

  ## --- membership inference (Issue 15) ---
  if ("membership_inference" %in% metrics) {
    results$membership_inference <- .compute_membership_auc(
      orig, synth, var_types
    )
  }

  ## --- TCAP (Issue 15) ---
  if ("TCAP" %in% metrics) {
    results$TCAP <- .compute_tcap(orig, synth, key_vars, sensitive_var)
  }

  structure(results, class = "synth_risk")
}

#' Risk-utility frontier analysis
#'
#' Compares multiple synthetic datasets on the risk-utility frontier
#' (Duncan-Lambert R-U confidentiality map). Each synthesizer produces
#' a point in (risk, utility) space; the frontier shows which methods
#' are Pareto-optimal.
#'
#' @param orig original data.frame
#' @param synth_list named list of synthetic data.frames to compare
#' @param utility_metric which utility metric to use as scalar.
#'   Default: \code{"pMSE"}.
#' @param risk_metric which risk metric to use as scalar.
#'   Default: \code{"distance"}.
#' @param key_vars passed to \code{\link{synth_risk}}
#' @param sensitive_var passed to \code{\link{synth_risk}}
#'
#' @return A data.frame of class \code{"ru_frontier"} with columns:
#'   \code{method}, \code{utility}, \code{risk}, \code{pareto_optimal}.
#'
#' @author Matthias Templ
#' @export
#' @examples
#' \dontrun{
#' data(CrohnD, package = "robustbase")
#' dat  <- CrohnD[, -1]
#' cont <- c("BMI", "height", "age", "weight")
#' s1   <- dat[sample(nrow(dat), replace = TRUE), ]
#' s2   <- dat
#' s2[, cont] <- as.matrix(s2[, cont]) +
#'              matrix(rnorm(nrow(dat) * length(cont), sd = 0.5),
#'                     ncol = length(cont))
#' res <- risk_utility_frontier(dat, list(bootstrap = s1, noisy = s2))
#' plot(res$utility, res$risk, pch = 19)
#' text(res$utility, res$risk, res$method, pos = 3)
#' }
risk_utility_frontier <- function(orig, synth_list,
                                  utility_metric = "pMSE",
                                  risk_metric = "distance",
                                  key_vars = NULL,
                                  sensitive_var = NULL) {
  stopifnot(is.list(synth_list), length(synth_list) > 0L)
  if (is.null(names(synth_list))) {
    names(synth_list) <- paste0("synth_", seq_along(synth_list))
  }

  n_methods <- length(synth_list)
  ru <- data.frame(
    method  = names(synth_list),
    utility = numeric(n_methods),
    risk    = numeric(n_methods),
    stringsAsFactors = FALSE
  )

  for (i in seq_len(n_methods)) {
    s <- synth_list[[i]]
    u <- synth_utility(orig, s, metrics = utility_metric)
    r <- synth_risk(orig, s, metrics = risk_metric,
                    key_vars = key_vars, sensitive_var = sensitive_var)

    ru$utility[i] <- .extract_utility_scalar(u, utility_metric)
    ru$risk[i]    <- .extract_risk_scalar(r, risk_metric)
  }

  ## identify Pareto-optimal points (lower utility loss + lower risk)
  ru$pareto_optimal <- .pareto_front(ru$utility, ru$risk)

  structure(ru, class = c("ru_frontier", "data.frame"))
}

#' Print method for ru_frontier
#' @param x a \code{"ru_frontier"} object
#' @param ... ignored
#' @export
print.ru_frontier <- function(x, ...) {
  cat("Risk-Utility Frontier\n")
  cat("  Methods compared:", nrow(x), "\n")
  cat("  Pareto-optimal:", sum(x$pareto_optimal), "\n\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}


# ---- internal utility functions -------------------------------------------

#' @keywords internal
.compute_pMSE <- function(orig, synth) {
  n_orig  <- nrow(orig)
  n_synth <- nrow(synth)
  combined <- rbind(
    cbind(orig, .synth_indicator = 0),
    cbind(synth, .synth_indicator = 1)
  )
  ## propensity score model
  fit <- tryCatch(
    stats::glm(.synth_indicator ~ ., data = combined, family = stats::binomial()),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NA_real_)

  phat <- stats::fitted(fit)
  c_expected <- n_synth / (n_orig + n_synth)
  mean((phat - c_expected)^2)
}

#' @keywords internal
.compare_regression <- function(orig, synth, var_types) {
  cont_vars <- names(var_types)[var_types == "continuous"]
  if (length(cont_vars) < 2L) {
    return(list(relative_bias = NA_real_, ci_overlap = NA_real_))
  }
  ## use first continuous variable as response
  resp <- cont_vars[1L]
  preds <- cont_vars[-1L]
  fml <- stats::as.formula(paste(resp, "~", paste(preds, collapse = " + ")))

  fit_orig  <- stats::lm(fml, data = orig)
  fit_synth <- stats::lm(fml, data = synth)

  coef_orig  <- stats::coef(fit_orig)
  coef_synth <- stats::coef(fit_synth)

  ## relative bias of non-intercept coefficients
  idx <- intersect(names(coef_orig), names(coef_synth))
  idx <- setdiff(idx, "(Intercept)")
  if (length(idx) == 0L) {
    return(list(relative_bias = NA_real_, ci_overlap = NA_real_))
  }

  rel_bias <- mean(abs(coef_synth[idx] - coef_orig[idx]) /
                    (abs(coef_orig[idx]) + 1e-10))

  ## confidence interval overlap
  ci_orig  <- stats::confint(fit_orig)[idx, , drop = FALSE]
  ci_synth <- stats::confint(fit_synth)[idx, , drop = FALSE]
  overlap  <- .ci_overlap(ci_orig, ci_synth)

  list(relative_bias = rel_bias, ci_overlap = overlap)
}

#' @keywords internal
.ci_overlap <- function(ci1, ci2) {
  ## average overlap of confidence intervals
  lo <- pmax(ci1[, 1], ci2[, 1])
  hi <- pmin(ci1[, 2], ci2[, 2])
  widths1 <- ci1[, 2] - ci1[, 1]
  widths2 <- ci2[, 2] - ci2[, 1]
  overlap <- pmax(hi - lo, 0)
  mean(2 * overlap / (widths1 + widths2 + 1e-10))
}

#' @keywords internal
.compute_overlap <- function(orig, synth, var_types) {
  overlaps <- numeric(ncol(orig))
  names(overlaps) <- names(orig)

  for (v in names(orig)) {
    if (var_types[v] == "continuous") {
      ## kernel density overlap for continuous
      range_v <- range(c(orig[[v]], synth[[v]]), na.rm = TRUE)
      grid    <- seq(range_v[1], range_v[2], length.out = 512)
      d_orig  <- tryCatch(
        stats::approx(stats::density(orig[[v]], na.rm = TRUE), xout = grid)$y,
        error = function(e) rep(0, 512)
      )
      d_synth <- tryCatch(
        stats::approx(stats::density(synth[[v]], na.rm = TRUE), xout = grid)$y,
        error = function(e) rep(0, 512)
      )
      d_orig[is.na(d_orig)]   <- 0
      d_synth[is.na(d_synth)] <- 0
      dx <- diff(grid[1:2])
      overlaps[v] <- sum(pmin(d_orig, d_synth)) * dx
    } else {
      ## categorical: overlap of proportions
      tab_orig  <- prop.table(table(orig[[v]]))
      tab_synth <- prop.table(table(synth[[v]]))
      all_lvls  <- union(names(tab_orig), names(tab_synth))
      p1 <- stats::setNames(rep(0, length(all_lvls)), all_lvls)
      p2 <- p1
      p1[names(tab_orig)]  <- tab_orig
      p2[names(tab_synth)] <- tab_synth
      overlaps[v] <- sum(pmin(p1, p2))
    }
  }
  overlaps
}

#' @keywords internal
.compute_k_anonymity <- function(orig, synth, key_vars) {
  ## k-anonymity on synthetic data alone
  synth_keys <- do.call(paste, c(synth[, key_vars, drop = FALSE],
                                 sep = "|"))
  tab_synth  <- table(synth_keys)
  k_synth    <- min(tab_synth)

  ## cross-dataset matching (Issue 14)
  orig_keys <- do.call(paste, c(orig[, key_vars, drop = FALSE],
                                sep = "|"))
  cross_tab <- table(orig_keys)[orig_keys %in% names(tab_synth)]
  k_cross   <- if (length(cross_tab) > 0L) min(cross_tab) else Inf

  list(synth_only = k_synth, cross_match = k_cross)
}

#' @keywords internal
.compute_distance_risk <- function(orig, synth, var_types) {
  ## Gower-like distance between each original and nearest synthetic
  cont_vars <- names(var_types)[var_types == "continuous"]
  if (length(cont_vars) == 0L) {
    return(list(min_distances = NA_real_, mean_min_dist = NA_real_))
  }
  X_orig  <- as.matrix(orig[, cont_vars, drop = FALSE])
  X_synth <- as.matrix(synth[, cont_vars, drop = FALSE])
  scales  <- .mad_scales(X_orig)
  X_orig_s  <- sweep(X_orig, 2, scales, "/")
  X_synth_s <- sweep(X_synth, 2, scales, "/")

  min_dists <- .nearest_distances(X_orig_s, X_synth_s)
  list(min_distances = min_dists, mean_min_dist = mean(min_dists))
}

#' @keywords internal
.compute_membership_auc <- function(orig, synth, var_types) {
  ## Membership inference attack (Issue 15):
  ## For each record (member = in orig, non-member = random),
  ## compute distance to nearest synthetic record; AUC measures

  ## how well these distances separate members from non-members.
  cont_vars <- names(var_types)[var_types == "continuous"]
  if (length(cont_vars) < 1L) return(NA_real_)

  X_synth <- as.matrix(synth[, cont_vars, drop = FALSE])
  X_orig  <- as.matrix(orig[, cont_vars, drop = FALSE])
  scales  <- .mad_scales(X_orig)
  X_synth_s <- sweep(X_synth, 2, scales, "/")
  X_orig_s  <- sweep(X_orig, 2, scales, "/")

  ## generate non-members by permuting each column independently
  n <- nrow(X_orig)
  X_nonmember <- X_orig
  for (j in seq_len(ncol(X_nonmember))) {
    X_nonmember[, j] <- sample(X_nonmember[, j])
  }
  X_nonmember_s <- sweep(X_nonmember, 2, scales, "/")

  ## distances to nearest synthetic record
  dist_member    <- .nearest_distances(X_orig_s, X_synth_s)
  dist_nonmember <- .nearest_distances(X_nonmember_s, X_synth_s)

  ## AUC: members should have smaller distances
  labels  <- c(rep(1L, n), rep(0L, n))
  scores  <- -c(dist_member, dist_nonmember)  # negate: closer = higher score
  .compute_auc(labels, scores)
}

#' @keywords internal
.compute_tcap <- function(orig, synth, key_vars, sensitive_var) {
  ## TCAP: for each original record, find nearest synthetic record
  ## on key variables and check if sensitive attribute matches.
  if (!sensitive_var %in% names(orig)) return(NA_real_)
  key_vars_use <- setdiff(key_vars, sensitive_var)
  if (length(key_vars_use) == 0L) return(NA_real_)

  var_types <- detect_var_types(orig[, key_vars_use, drop = FALSE])
  cont_keys <- names(var_types)[var_types == "continuous"]

  n <- nrow(orig)
  correct <- 0L

  if (length(cont_keys) > 0L) {
    X_orig_key  <- as.matrix(orig[, cont_keys, drop = FALSE])
    X_synth_key <- as.matrix(synth[, cont_keys, drop = FALSE])
    scales <- .mad_scales(X_orig_key)
    X_orig_s  <- sweep(X_orig_key, 2, scales, "/")
    X_synth_s <- sweep(X_synth_key, 2, scales, "/")

    for (i in seq_len(n)) {
      diffs <- sweep(X_synth_s, 2, X_orig_s[i, ], "-")
      dists <- sqrt(rowSums(diffs^2))
      nearest <- which.min(dists)
      if (as.character(orig[[sensitive_var]][i]) ==
          as.character(synth[[sensitive_var]][nearest])) {
        correct <- correct + 1L
      }
    }
  } else {
    ## all key vars categorical: exact matching
    orig_keys  <- do.call(paste, c(orig[, key_vars_use, drop = FALSE],
                                   sep = "|"))
    synth_keys <- do.call(paste, c(synth[, key_vars_use, drop = FALSE],
                                   sep = "|"))
    for (i in seq_len(n)) {
      matches <- which(synth_keys == orig_keys[i])
      if (length(matches) > 0L) {
        nearest <- matches[1L]
        if (as.character(orig[[sensitive_var]][i]) ==
            as.character(synth[[sensitive_var]][nearest])) {
          correct <- correct + 1L
        }
      }
    }
  }

  correct / n
}


# ---- small internal helpers -----------------------------------------------

#' Per-column MAD scale, floored at 1 when MAD is effectively zero.
#'
#' @keywords internal
.mad_scales <- function(X) {
  s <- apply(X, 2, stats::mad, na.rm = TRUE)
  s[s < .Machine$double.eps] <- 1
  s
}

#' Distance from each row of \code{X_query} to its nearest row in
#' \code{X_ref}, in Euclidean metric.  Expects both inputs on the same
#' scale.
#'
#' @keywords internal
.nearest_distances <- function(X_query, X_ref) {
  n <- nrow(X_query)
  dists <- numeric(n)
  for (i in seq_len(n)) {
    diffs <- sweep(X_ref, 2, X_query[i, ], "-")
    dists[i] <- min(sqrt(rowSums(diffs^2)))
  }
  dists
}

#' @keywords internal
.compute_auc <- function(labels, scores) {
  ## Wilcoxon-Mann-Whitney AUC
  n_pos <- sum(labels == 1L)
  n_neg <- sum(labels == 0L)
  if (n_pos == 0L || n_neg == 0L) return(NA_real_)

  ord <- order(scores, decreasing = TRUE)
  labels_sorted <- labels[ord]
  tp <- cumsum(labels_sorted == 1L)
  fp <- cumsum(labels_sorted == 0L)
  tpr <- tp / n_pos
  fpr <- fp / n_neg
  ## trapezoidal AUC
  sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2)
}

#' @keywords internal
.extract_utility_scalar <- function(u, metric) {
  switch(metric,
    pMSE        = u$pMSE,
    KS          = mean(u$KS, na.rm = TRUE),
    regression  = u$regression$relative_bias,
    correlation = u$correlation,
    overlap     = 1 - mean(u$overlap, na.rm = TRUE),
    NA_real_
  )
}

#' @keywords internal
.extract_risk_scalar <- function(r, metric) {
  switch(metric,
    k_anonymity          = 1 / max(r$k_anonymity$synth_only, 1),
    distance             = r$distance$mean_min_dist,
    membership_inference = r$membership_inference,
    TCAP                 = r$TCAP,
    NA_real_
  )
}

#' @keywords internal
.pareto_front <- function(utility_loss, risk) {
  ## lower utility_loss and lower risk is better
  ## a point is Pareto-optimal if no other point dominates it
  n <- length(utility_loss)
  optimal <- logical(n)
  for (i in seq_len(n)) {
    dominated <- FALSE
    for (j in seq_len(n)) {
      if (j == i) next
      if (utility_loss[j] <= utility_loss[i] &&
          risk[j] <= risk[i] &&
          (utility_loss[j] < utility_loss[i] || risk[j] < risk[i])) {
        dominated <- TRUE
        break
      }
    }
    optimal[i] <- !dominated
  }
  optimal
}

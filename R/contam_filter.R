#' Estimate contamination model
#'
#' Estimates cellwise or rowwise contamination probabilities from the
#' original data.  Returns posterior clean probabilities that can be
#' used as diagnostic weights or as observation weights in downstream
#' fitting.
#'
#' @param data a data.frame
#' @param method \code{"cellwise"} (default) estimates per-cell
#'   posterior clean probabilities; \code{"rowwise"} uses MCD-based
#'   Mahalanobis distances to flag entire rows.
#' @param eps_init initial contamination probability per variable.
#'   Default: 0.1. Must be in \eqn{(0, 0.5)}.
#' @param gamma_init initial scale inflation factor for the
#'   contamination distribution. Default: 3.
#' @param maxit maximum EM iterations. Default: 100.
#' @param tol convergence tolerance on relative parameter change.
#'   Default: 1e-4.
#' @param alpha fraction of data for MCD (rowwise only). Default: 0.75.
#'
#' @return A list with components:
#'   \item{clean_probs}{n x p matrix of posterior clean probabilities
#'     (values in \eqn{[0, 1]}). Categorical columns have weight 1.}
#'   \item{epsilon}{named numeric vector of estimated per-variable
#'     contamination rates (continuous only)}
#'   \item{method}{the method used}
#'   \item{converged}{logical}
#'   \item{iterations}{number of EM iterations performed}
#'
#' @details
#' \strong{Cellwise method.}
#' For each continuous variable \eqn{j}, the observed value is modeled
#' as a two-component mixture:
#' \deqn{(1 - \varepsilon_j) \cdot N(\mu_j, \sigma_j^2) +
#'   \varepsilon_j \cdot N(\mu_j, (\gamma_j \sigma_j)^2)}
#' The EM algorithm iterates between computing posterior clean
#' probabilities (E-step) and updating parameters (M-step).
#'
#' \strong{Important (Issue 20):} Cellwise z-scores use
#' \eqn{z_j = (x - \mathrm{med}) / \mathrm{mad}}, NOT
#' \eqn{|x - \mathrm{med}| / \mathrm{mad}}. The absolute value folds
#' the distribution and makes \code{dnorm(z_j)} incorrect.
#'
#' \strong{Rowwise method.}
#' Uses robust Mahalanobis distances from \code{robustbase::covMcd}
#' and converts to row-level clean probabilities via a chi-squared
#' tail probability.
#'
#' @author Matthias Templ
#' @export
#' @importFrom robustbase covMcd
#' @examples
#' \dontrun{
#' data(iris)
#' iris_c <- iris[, 1:4]
#' iris_c[1:5, 1] <- iris_c[1:5, 1] + 10
#' res <- estimate_contamination(iris_c, method = "cellwise")
#' image(res$clean_probs, main = "Clean probabilities")
#' }
estimate_contamination <- function(data,
                                   method = c("cellwise", "rowwise"),
                                   eps_init = 0.1,
                                   gamma_init = 3,
                                   maxit = 100L,
                                   tol = 1e-4,
                                   alpha = 0.75) {
  method <- match.arg(method)
  if (inherits(data, "data.table")) data <- as.data.frame(data)
  stopifnot(is.data.frame(data))

  var_types <- detect_var_types(data)
  cont_idx  <- which(var_types == "continuous")
  n <- nrow(data)
  p <- ncol(data)

  if (method == "rowwise") {
    return(.estimate_rowwise(data, cont_idx, alpha))
  }

  ## --- cellwise EM ---
  clean_probs <- matrix(1, nrow = n, ncol = p,
                        dimnames = list(NULL, names(data)))
  epsilon <- setNames(rep(eps_init, length(cont_idx)),
                      names(data)[cont_idx])
  gamma   <- setNames(rep(gamma_init, length(cont_idx)),
                      names(data)[cont_idx])

  ## robust initial estimates
  mu    <- vapply(cont_idx, function(j) median(data[[j]], na.rm = TRUE),
                  numeric(1L))
  sigma <- vapply(cont_idx, function(j) {
    s <- mad(data[[j]], na.rm = TRUE)
    if (s < .Machine$double.eps) s <- sd(data[[j]], na.rm = TRUE)
    if (s < .Machine$double.eps) s <- 1
    s
  }, numeric(1L))
  names(mu) <- names(sigma) <- names(data)[cont_idx]

  converged <- FALSE
  iter <- 0L

  for (it in seq_len(maxit)) {
    iter <- it
    mu_old    <- mu
    sigma_old <- sigma

    ## --- E-step: compute posterior clean probabilities per cell ---
    for (k in seq_along(cont_idx)) {
      j    <- cont_idx[k]
      vn   <- names(data)[j]
      x    <- data[[j]]
      obs  <- !is.na(x)

      ## z-scores: (x - mu) / sigma, NOT abs(x - mu) / sigma (Issue 20)
      z <- (x[obs] - mu[vn]) / sigma[vn]

      ## clean component density
      d_clean  <- dnorm(z)
      ## contamination component: inflated scale
      d_contam <- dnorm(z / gamma[vn]) / gamma[vn]

      ## posterior clean probability
      eps_j <- epsilon[vn]
      numer <- (1 - eps_j) * d_clean
      denom <- numer + eps_j * d_contam
      denom[denom < .Machine$double.eps] <- .Machine$double.eps
      w <- numer / denom

      clean_probs[obs, j] <- w
      ## missing cells stay at weight 1
    }

    ## --- M-step: update parameters ---
    for (k in seq_along(cont_idx)) {
      j    <- cont_idx[k]
      vn   <- names(data)[j]
      x    <- data[[j]]
      obs  <- !is.na(x)
      w    <- clean_probs[obs, j]
      xo   <- x[obs]

      ## weighted location
      mu[vn] <- sum(w * xo) / sum(w)

      ## weighted scale
      resid2 <- (xo - mu[vn])^2
      sigma[vn] <- sqrt(sum(w * resid2) / sum(w))
      if (sigma[vn] < .Machine$double.eps) sigma[vn] <- 1

      ## update contamination rate
      epsilon[vn] <- 1 - mean(w)
      epsilon[vn] <- max(min(epsilon[vn], 0.499), 1e-6)

      ## update contamination scale
      w_contam <- 1 - w
      if (sum(w_contam) > 1e-6) {
        gamma[vn] <- sqrt(sum(w_contam * resid2) /
                          (sum(w_contam) * sigma[vn]^2))
        gamma[vn] <- max(gamma[vn], 1.01)
      }
    }

    ## convergence check
    rel_change_mu <- max(abs(mu - mu_old) / (abs(mu_old) + 1e-10))
    rel_change_sigma <- max(abs(sigma - sigma_old) /
                            (abs(sigma_old) + 1e-10))
    if (max(rel_change_mu, rel_change_sigma) < tol) {
      converged <- TRUE
      break
    }
  }

  list(
    clean_probs = clean_probs,
    epsilon     = epsilon,
    gamma       = gamma,
    mu          = mu,
    sigma       = sigma,
    method      = "cellwise",
    converged   = converged,
    iterations  = iter
  )
}

#' Synthesise from estimated clean distribution
#'
#' Given the contamination weights from \code{\link{estimate_contamination}},
#' draws synthetic records from the estimated clean distribution —
#' either a multivariate normal fitted from weighted moments
#' (\code{"parametric"}) or a weighted resample of the observed rows
#' (\code{"weighted_resample"}).
#'
#' @param data original data.frame
#' @param clean_probs n x p matrix of posterior clean probabilities
#'   (from \code{\link{estimate_contamination}})
#' @param method synthesis method for the clean distribution:
#'   \code{"parametric"} (default) draws from the estimated normal;
#'   \code{"weighted_resample"} resamples with clean-probability weights.
#' @param n_synth number of synthetic records. Default: \code{nrow(data)}.
#' @param seed random seed.
#'
#' @return a synthetic data.frame
#' @author Matthias Templ
#' @export
#' @examples
#' \dontrun{
#' data(iris)
#' iris_c <- iris[, 1:4]
#' iris_c[1:5, 1] <- iris_c[1:5, 1] + 10
#' contam <- estimate_contamination(iris_c, method = "cellwise")
#' synth  <- synth_from_clean(iris_c, contam$clean_probs)
#' }
synth_from_clean <- function(data, clean_probs,
                             method = c("parametric", "weighted_resample"),
                             n_synth = nrow(data),
                             seed = NULL) {
  method <- match.arg(method)
  if (!is.null(seed)) set.seed(seed)
  if (inherits(data, "data.table")) data <- as.data.frame(data)

  var_types <- detect_var_types(data)
  cont_idx  <- which(var_types == "continuous")
  cat_idx   <- which(var_types == "categorical")
  n <- nrow(data)

  if (method == "parametric") {
    ## estimate clean parameters using weights
    synth <- data.frame(matrix(NA, nrow = n_synth, ncol = ncol(data)))
    names(synth) <- names(data)

    ## continuous: weighted mean and covariance -> multivariate normal
    if (length(cont_idx) > 0L) {
      X_cont <- as.matrix(data[, cont_idx, drop = FALSE])
      W_cont <- clean_probs[, cont_idx, drop = FALSE]

      ## weighted means
      mu <- colSums(W_cont * X_cont) / colSums(W_cont)

      ## pairwise weighted covariance (Issue 1: w_ij * w_ik)
      p_cont <- length(cont_idx)
      Sigma  <- matrix(0, p_cont, p_cont)
      for (j in seq_len(p_cont)) {
        for (k in j:p_cont) {
          wjk <- W_cont[, j] * W_cont[, k]
          cov_jk <- sum(wjk * (X_cont[, j] - mu[j]) *
                        (X_cont[, k] - mu[k])) / sum(wjk)
          Sigma[j, k] <- Sigma[k, j] <- cov_jk
        }
      }

      ## ensure positive definiteness
      eig <- eigen(Sigma, symmetric = TRUE)
      eig$values <- pmax(eig$values, 1e-8)
      Sigma <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)

      synth_cont <- MASS::mvrnorm(n_synth, mu = mu, Sigma = Sigma)
      for (j in seq_along(cont_idx)) {
        synth[[cont_idx[j]]] <- synth_cont[, j]
      }
    }

    ## categorical: weighted multinomial draw
    for (j in cat_idx) {
      y <- data[[j]]
      if (!is.factor(y)) y <- as.factor(y)
      lvls <- levels(y)
      w <- clean_probs[, j]
      ## compute weighted class proportions
      probs <- tapply(w, y, sum)
      probs <- probs / sum(probs)
      synth[[j]] <- factor(
        sample(lvls, n_synth, replace = TRUE, prob = probs),
        levels = lvls
      )
    }
    return(synth)
  }

  ## --- weighted resampling ---
  ## row-level weight = geometric mean of cell weights
  row_w <- exp(rowMeans(log(pmax(clean_probs, 1e-10))))
  row_w <- row_w / sum(row_w)
  idx   <- sample(n, n_synth, replace = TRUE, prob = row_w)
  data[idx, , drop = FALSE]
}


# ---- internal: rowwise estimation ----------------------------------------

#' @keywords internal
#' @importFrom robustbase covMcd
.estimate_rowwise <- function(data, cont_idx, alpha) {
  n <- nrow(data)
  p <- ncol(data)

  clean_probs <- matrix(1, nrow = n, ncol = p,
                        dimnames = list(NULL, names(data)))

  if (length(cont_idx) > 0L) {
    X <- as.matrix(data[, cont_idx, drop = FALSE])
    X_complete <- X[stats::complete.cases(X), , drop = FALSE]

    if (nrow(X_complete) > ncol(X_complete) + 1L) {
      mcd <- robustbase::covMcd(X_complete, alpha = alpha)

      ## Mahalanobis distances for all observations
      mu_hat    <- mcd$center
      sigma_hat <- mcd$cov

      ## regularise if needed
      eig <- eigen(sigma_hat, symmetric = TRUE)
      eig$values <- pmax(eig$values, 1e-8)
      sigma_inv <- eig$vectors %*% diag(1 / eig$values) %*%
                   t(eig$vectors)

      for (i in seq_len(n)) {
        xi <- X[i, ]
        if (any(is.na(xi))) next
        d2 <- as.numeric(t(xi - mu_hat) %*% sigma_inv %*% (xi - mu_hat))
        ## clean probability from chi-squared survival function
        p_clean <- stats::pchisq(d2, df = length(cont_idx))
        ## high distance -> low clean probability
        w <- 1 - p_clean
        clean_probs[i, cont_idx] <- w
      }
    }
  }

  epsilon <- 1 - colMeans(clean_probs[, cont_idx, drop = FALSE])

  list(
    clean_probs = clean_probs,
    epsilon     = epsilon,
    method      = "rowwise",
    converged   = TRUE,
    iterations  = 1L
  )
}

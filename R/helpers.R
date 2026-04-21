#' Detect variable types in a data.frame
#'
#' Classifies each column as \code{"continuous"} or \code{"categorical"}
#' based on class and number of unique values.
#'
#' @param data a data.frame
#' @param max_levels integer; numeric columns with at most this many
#'   unique values are treated as categorical. Default: 5.
#' @return named character vector with entries \code{"continuous"} or
#'   \code{"categorical"}, one per column
#' @author Matthias Templ
#' @export
#' @examples
#' detect_var_types(iris)
detect_var_types <- function(data, max_levels = 5L) {
  stopifnot(is.data.frame(data))
  types <- vapply(data, function(x) {
    if (is.factor(x) || is.character(x) || is.logical(x)) {
      return("categorical")
    }
    if (is.numeric(x) && length(unique(x[!is.na(x)])) <= max_levels) {
      return("categorical")
    }
    "continuous"
  }, character(1L))
  types
}

#' Huber weights for standardised values
#'
#' Computes Huber weights: 1 for values within the tuning constant
#' and \code{k / abs(x)} for values outside. Standardisation
#' (centering by median, scaling by MAD) is applied internally.
#'
#' @param x numeric vector (raw, unstandardised)
#' @param k tuning constant, Default: 1.345
#' @return numeric vector of weights in \eqn{[0, 1]}
#' @author Matthias Templ
#' @export
#' @examples
#' set.seed(1)
#' x <- c(rnorm(100), 10, -8)
#' w <- huber_weights(x)
#' plot(x, w)
huber_weights <- function(x, k = 1.345) {
  med <- median(x, na.rm = TRUE)
  s   <- mad(x, na.rm = TRUE)
  if (s < .Machine$double.eps) s <- 1
  u <- (x - med) / s
  w <- ifelse(abs(u) <= k, 1, k / abs(u))
  w[is.nan(w) | is.na(w)] <- 1
  w
}

#' Weighted median
#'
#' Computes the weighted median of a numeric vector using sorted
#' cumulative weights.
#'
#' @param x numeric vector
#' @param w numeric vector of non-negative weights (same length as \code{x})
#' @param na.rm logical; if \code{TRUE}, remove NAs. Default: TRUE.
#' @return numeric scalar
#' @author Matthias Templ
#' @export
#' @examples
#' weighted_median(1:10, w = c(rep(1, 5), rep(0.1, 5)))
weighted_median <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]
    w <- w[keep]
  }
  if (length(x) == 0L) return(NA_real_)
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cum_w <- cumsum(w)
  half  <- sum(w) / 2
  idx   <- which(cum_w >= half)[1L]
  x[idx]
}

#' Weighted MAD (median absolute deviation)
#'
#' Computes the weighted median absolute deviation, using
#' \code{\link{weighted_median}} for both the center and the
#' deviations. Consistency factor of 1.4826 is applied for
#' normal data.
#'
#' @param x numeric vector
#' @param w numeric vector of non-negative weights (same length as \code{x})
#' @param constant scale factor for asymptotic consistency at the
#'   normal. Default: 1.4826.
#' @param na.rm logical; remove NAs. Default: TRUE.
#' @return positive numeric scalar
#' @author Matthias Templ
#' @export
#' @examples
#' set.seed(1)
#' x <- c(rnorm(100), 10)
#' weighted_mad(x, w = c(rep(1, 100), 0.01))
weighted_mad <- function(x, w, constant = 1.4826, na.rm = TRUE) {
  if (na.rm) {
    keep <- !is.na(x) & !is.na(w)
    x <- x[keep]
    w <- w[keep]
  }
  center <- weighted_median(x, w, na.rm = FALSE)
  dev    <- abs(x - center)
  constant * weighted_median(dev, w, na.rm = FALSE)
}

#' Generate AR(1) covariance matrix
#'
#' Constructs a \eqn{p \times p} covariance matrix with AR(1) structure
#' \eqn{\Sigma_{jk} = \rho^{|j-k|}}. Useful for simulation studies.
#'
#' @param p integer; dimension
#' @param rho numeric; autocorrelation parameter in \eqn{(-1, 1)}
#' @return \eqn{p \times p} positive definite matrix
#' @author Matthias Templ
#' @export
#' @examples
#' generate_ar1_cov(5, 0.7)
generate_ar1_cov <- function(p, rho) {
  stopifnot(length(p) == 1L, p >= 1L)
  stopifnot(length(rho) == 1L, abs(rho) < 1)
  idx <- seq_len(p)
  rho^abs(outer(idx, idx, "-"))
}

#' Predictive mean matching draw
#'
#' Finds the \code{donors} closest fitted values to the target
#' prediction and randomly selects one donor's observed value.
#'
#' @param y_obs observed response values (numeric)
#' @param yhat_obs fitted values for observed data (same length as
#'   \code{y_obs})
#' @param yhat_new fitted values for new / synthetic observations
#' @param donors number of candidate donors. Default: 5.
#' @return numeric vector of length \code{length(yhat_new)}
#' @author Matthias Templ
#' @keywords internal
pmm_draw <- function(y_obs, yhat_obs, yhat_new, donors = 5L) {
  n_new <- length(yhat_new)
  out   <- numeric(n_new)
  for (i in seq_len(n_new)) {
    dists <- abs(yhat_obs - yhat_new[i])
    idx   <- order(dists)[seq_len(min(donors, length(y_obs)))]
    out[i] <- y_obs[sample(idx, 1L)]
  }
  out
}

#' Strip diagnostics from a robsynth result
#'
#' Removes the \code{contWeights} component from a robsynth result
#' to prevent disclosure of contamination flags. This should be
#' called before releasing synthetic data.
#'
#' @param x an object of class \code{"robsynth"}
#' @return the same object with \code{contWeights} set to \code{NULL}
#' @author Matthias Templ
#' @export
strip_diagnostics <- function(x) {
  stopifnot(inherits(x, "robsynth"))
  x$contWeights <- NULL
  x
}

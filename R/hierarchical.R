#' Robust synthesis for hierarchical (household/person) data
#'
#' Synthesises data with household-person structure in two phases:
#' (1)~resample complete households and synthesise household-level
#' variables, (2)~synthesise person-level variables within each
#' household conditional on household-level variables.
#'
#' @param data a data.frame with household and person identifiers
#' @param hhid character: name of the household identifier column
#' @param pid character: name of the person identifier column
#'   (optional; if NULL, row number within household is used)
#' @param hh_vars character vector of household-level variable names
#'   (constant within household)
#' @param person_vars character vector of person-level variable names
#'   to synthesise
#' @param method synthesis method(s), as in \code{\link{robsynth}}
#' @param m number of synthetic copies
#' @param weights survey weight column name (optional)
#' @param seed random seed
#' @param ... additional arguments passed to internal \code{robsynth}
#'   calls
#'
#' @return An S3 object of class \code{"robsynth_hh"} with components:
#'   \item{synth}{synthetic data.frame (or list when m > 1)}
#'   \item{m}{number of copies}
#'   \item{hhid}{household ID column name}
#'   \item{n_hh}{number of households}
#'   \item{n_persons}{number of persons}
#'   \item{call}{the matched call}
#'
#' @details
#' Phase 1 (Structure): Households are resampled with replacement from
#' the original data.  Household-level variables (specified via
#' \code{hh_vars}) are carried over from the resampled households.
#' Household sizes are preserved.
#'
#' Phase 2 (Person-level): For each person-level variable, a robust
#' model is fitted on the full original data using all previously
#' synthesised variables (including household-level) as predictors.
#' Synthetic values are drawn conditional on the household context.
#'
#' @author Matthias Templ
#' @export
#' @examples
#' \dontrun{
#' # Create simple household data
#' hh_data <- data.frame(
#'   hhid = rep(1:100, each = 3),
#'   hh_income = rep(rnorm(100, 50000, 10000), each = 3),
#'   hh_region = rep(sample(c("A","B","C"), 100, TRUE), each = 3),
#'   age = rnorm(300, 40, 15),
#'   sex = factor(sample(c("M","F"), 300, TRUE)),
#'   employed = factor(sample(0:1, 300, TRUE))
#' )
#'
#' res <- robsynth_hh(hh_data,
#'   hhid = "hhid",
#'   hh_vars = c("hh_income", "hh_region"),
#'   person_vars = c("age", "sex", "employed"))
#' }
robsynth_hh <- function(data,
                        hhid,
                        pid = NULL,
                        hh_vars = NULL,
                        person_vars = NULL,
                        method = NULL,
                        m = 1L,
                        weights = NULL,
                        seed = NULL,
                        ...) {
  cl <- match.call()

  if (inherits(data, "data.table")) data <- as.data.frame(data)
  stopifnot(is.data.frame(data), hhid %in% names(data))

  if (!is.null(seed)) set.seed(seed)

  ## identify household structure
  hh_ids <- unique(data[[hhid]])
  n_hh <- length(hh_ids)
  hh_sizes <- table(data[[hhid]])

  ## auto-detect hh_vars if not specified: constant within household

  if (is.null(hh_vars)) {
    hh_vars <- character(0)
    for (v in setdiff(names(data), c(hhid, pid))) {
      is_const <- all(vapply(split(data[[v]], data[[hhid]]),
                              function(x) length(unique(x)) == 1L,
                              logical(1)))
      if (is_const) hh_vars <- c(hh_vars, v)
    }
  }

  if (is.null(person_vars)) {
    person_vars <- setdiff(names(data), c(hhid, pid, hh_vars))
  }

  message(sprintf("Hierarchical synthesis: %d households, %d persons",
              n_hh, nrow(data)))
  message(sprintf("  HH-level vars: %s", paste(hh_vars, collapse = ", ")))
  message(sprintf("  Person-level vars: %s", paste(person_vars, collapse = ", ")))

  ## --- Phase 1: Resample households ---
  synth_list <- vector("list", m)

  rows_by_hh <- split(seq_len(nrow(data)), data[[hhid]])

  for (mi in seq_len(m)) {
    ## resample household IDs with replacement
    sampled_hh <- sample(hh_ids, n_hh, replace = TRUE)
    picked <- rows_by_hh[as.character(sampled_hh)]
    synth <- data[unlist(picked, use.names = FALSE), , drop = FALSE]
    synth[[hhid]] <- rep(seq_along(picked), lengths(picked))
    rownames(synth) <- NULL

    ## --- Phase 2: Synthesise person-level variables ---
    ## Use all hh_vars + previously synthesised person_vars as predictors
    all_preds <- c(hh_vars)

    for (pv in person_vars) {
      y_orig <- data[[pv]]  # fit on original, full data
      var_type <- if (is.factor(y_orig) || is.character(y_orig))
                    "categorical" else "continuous"

      if (length(all_preds) == 0L) {
        ## marginal synthesis
        synth[[pv]] <- .synth_marginal(y_orig, var_type, n = nrow(synth))
      } else {
        ## Fit on the ORIGINAL data (so the model sees the full empirical
        ## distribution of the response), then predict on the resampled
        ## household structure.
        X_orig <- data[, all_preds, drop = FALSE]
        X_new  <- synth[, all_preds, drop = FALSE]

        meth <- if (is.null(method)) {
          if (var_type == "continuous") "mm" else "robust_logreg"
        } else if (length(method) == 1L) {
          method
        } else {
          method[pv]
        }

        fit <- .fit_dispatch(y_orig, X_orig, meth, var_type, vname = pv, ...)
        synth[[pv]] <- .generate_dispatch(fit, X_new, meth, var_type,
                                           nrow(synth))
      }
      all_preds <- c(all_preds, pv)
    }

    ## preserve factor levels
    for (v in names(data)) {
      if (is.factor(data[[v]]) && v %in% names(synth)) {
        synth[[v]] <- factor(synth[[v]], levels = levels(data[[v]]))
      }
    }

    synth_list[[mi]] <- synth
  }

  structure(
    list(
      synth     = if (m == 1L) synth_list[[1L]] else synth_list,
      m         = m,
      hhid      = hhid,
      hh_vars   = hh_vars,
      person_vars = person_vars,
      n_hh      = n_hh,
      n_persons = nrow(data),
      call      = cl
    ),
    class = "robsynth_hh"
  )
}

#' Print method for robsynth_hh objects
#' @param x a \code{"robsynth_hh"} object
#' @param ... ignored
#' @export
print.robsynth_hh <- function(x, ...) {
  cat("robSynth hierarchical result\n")
  cat("  Households:", x$n_hh, "\n")
  cat("  Persons:", x$n_persons, "\n")
  cat("  HH vars:", paste(x$hh_vars, collapse = ", "), "\n")
  cat("  Person vars:", paste(x$person_vars, collapse = ", "), "\n")
  if (x$m > 1L) cat("  Synthetic copies:", x$m, "\n")
  invisible(x)
}

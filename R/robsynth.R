#' Robust synthetic data generation
#'
#' Main entry point for the \pkg{robSynth} package.  Generates synthetic
#' data from a (possibly contaminated) original dataset using robust
#' sequential conditional synthesis.
#'
#' @param data a data.frame (or data.table, which is converted internally)
#' @param method synthesis method(s).  Either a single string applied to all
#'   variables, or a named character vector with one method per variable.
#'   Available methods for continuous variables:
#'   \code{"mm"} (MM-estimation, default robust),
#'   \code{"norm"} (OLS + normal noise),
#'   \code{"cart"} (CART via rpart),
#'   \code{"ctree"} (conditional inference tree),
#'   \code{"sample"} (marginal resampling).
#'   For categorical variables:
#'   \code{"robust_logreg"} (Huber-weighted logistic, default robust),
#'   \code{"logreg"} (standard logistic),
#'   \code{"polyreg"} (multinomial logistic via nnet),
#'   \code{"cart"}, \code{"ctree"}, \code{"sample"}.
#'   Special methods:
#'   \code{"passive"} (deterministic function of other variables, see
#'     \code{formulas}),
#'   \code{""} (skip: variable not synthesised, used as predictor only).
#' @param m number of synthetic copies to generate.  Default 1.
#'   When \code{m > 1}, the \code{synth} component is a list of
#'   data frames and combining rules apply to model-based inference.
#' @param visit.sequence character vector specifying the order in which
#'   variables are synthesised.  Default: column order of \code{data}.
#' @param predictor.matrix a \eqn{p \times p} binary matrix where entry
#'   \code{[j, k] = 1} means variable \code{k} is used as predictor
#'   for variable \code{j}.  Default: lower-triangular (each variable
#'   predicted by all prior variables in the visit sequence).
#' @param formulas optional named list of formulas.  For variables with
#'   custom formulas, the right-hand side specifies predictors with
#'   optional interactions and transformations (e.g.,
#'   \code{list(y = ~ x1 * x2 + I(x3^2))}).  For \code{method = "passive"},
#'   the formula specifies the deterministic function (e.g.,
#'   \code{list(bmi = ~ I(weight / height^2 * 10000))}).
#' @param transform optional named list mapping variable names to
#'   transformation type: \code{"log"}, \code{"sqrt"}, or \code{"boxcox"}
#'   (robust Box-Cox with robustly estimated lambda).  Transformations
#'   are applied before synthesis and back-transformed afterwards.
#' @param weights survey/design weights.  Either a character string
#'   naming a column in \code{data} (which is then removed from
#'   synthesis variables), or a numeric vector of length \code{nrow(data)}.
#'   Weights are passed to all fitting functions (lmrob, lm, glm).
#' @param n_synth number of synthetic records.  Default: \code{nrow(data)}.
#' @param seed random seed for reproducibility.
#' @param proper logical.  Currently ignored (plug-in synthesis only).
#'   Future versions will support bootstrap-based proper synthesis.
#' @param cont.na optional named list of values to treat as NA for
#'   continuous variables (e.g., \code{list(income = c(-8, -9))}).
#' @param n_cores number of cores for parallel synthesis when
#'   \code{m > 1}.  Default 1 (sequential).
#' @param ... additional arguments passed to fitting functions.
#'
#' @return An S3 object of class \code{"robsynth"} with components:
#'   \item{synth}{synthetic data.frame (or list of data.frames when
#'     \code{m > 1})}
#'   \item{m}{number of synthetic copies}
#'   \item{method}{named character vector of methods used}
#'   \item{visit.sequence}{variable ordering}
#'   \item{predictor.matrix}{the predictor matrix used}
#'   \item{formulas}{custom formulas (if any)}
#'   \item{transform}{transformations applied (if any)}
#'   \item{models}{list of fitted models (from the last synthesis)}
#'   \item{var_types}{named character vector of variable types}
#'   \item{call}{the matched call}
#'
#' @details
#' Variables are synthesised sequentially.  For each variable, a model is
#' fitted on the original data conditional on the predictors specified by
#' the predictor matrix.  Synthetic values are then drawn from the fitted
#' model using the already-synthesised predictor values.
#'
#' The default method for continuous variables is \code{"mm"}
#' (MM-estimation via \code{robustbase::lmrob}), which achieves 50\%
#' breakdown point and 95\% Gaussian efficiency.  The default for
#' categorical variables is \code{"robust_logreg"} (Huber-weighted
#' logistic regression).
#'
#' @author Matthias Templ
#' @references
#' Nowok B., Raab G. M., Dibben C. (2016).  synthpop: Bespoke creation
#'   of synthetic data in R.  \emph{Journal of Statistical Software},
#'   74(11), 1-26.
#'
#' Reiter J. P. (2003).  Inference for partially synthetic, public use
#'   microdata sets.  \emph{Survey Methodology}, 29(2), 181-188.
#'
#' Templ M., Meindl B., Kowarik A., Dupriez O. (2017).  Simulation of
#'   synthetic complex data: The R package simPop.  \emph{Journal of
#'   Statistical Software}, 79(10), 1-38.
#'
#' Yohai V. J. (1987).  High breakdown-point and high efficiency robust
#'   estimates for regression.  \emph{Annals of Statistics}, 15(2),
#'   642-656.
#'
#' Box G. E. P., Cox D. R. (1964).  An analysis of transformations.
#'   \emph{Journal of the Royal Statistical Society: Series B}, 26(2),
#'   211-252.
#'
#' @export
#' @seealso \code{\link{synth_utility}}, \code{\link{synth_risk}},
#'   \code{\link{compare.robsynth}}, \code{\link{synth_lm}}
#' @examples
#' \dontrun{
#' data(iris)
#' iris_c <- iris
#' iris_c[1:5, 1] <- iris_c[1:5, 1] + 10
#'
#' # Single method (default: robust)
#' res <- robsynth(iris_c)
#'
#' # Per-variable methods
#' res2 <- robsynth(iris_c,
#'   method = c(Sepal.Length = "mm", Sepal.Width = "norm",
#'              Petal.Length = "mm", Petal.Width = "cart",
#'              Species = "polyreg"))
#'
#' # Multiple synthesis
#' res3 <- robsynth(iris_c, m = 5)
#' }
robsynth <- function(data,
                     method = NULL,
                     m = 1L,
                     visit.sequence = NULL,
                     predictor.matrix = NULL,
                     formulas = NULL,
                     transform = NULL,
                     weights = NULL,
                     n_synth = nrow(data),
                     seed = NULL,
                     proper = FALSE,
                     cont.na = NULL,
                     n_cores = 1L,
                     ...) {
  cl <- match.call()

  ## --- input validation ---
  if (inherits(data, "data.table")) data <- as.data.frame(data)
  stopifnot(is.data.frame(data), nrow(data) > 0L, ncol(data) > 0L)

  if (!is.null(seed)) set.seed(seed)

  ## --- survey weights ---
  if (is.character(weights) && length(weights) == 1L) {
    stopifnot(weights %in% names(data))
    wt_vec <- data[[weights]]
    data[[weights]] <- NULL  # remove weight column from synthesis vars
  } else if (is.numeric(weights)) {
    stopifnot(length(weights) == nrow(data))
    wt_vec <- weights
  } else {
    wt_vec <- NULL
  }

  var_names <- names(data)
  p <- length(var_names)

  ## --- handle cont.na (recode sentinel codes to NA BEFORE type detection) ---
  ## If sentinel codes are present, MAD/SD of "dirty" columns would be
  ## inflated and mislead downstream type classification and scale estimates.
  if (!is.null(cont.na)) {
    for (v in names(cont.na)) {
      if (v %in% var_names && is.numeric(data[[v]])) {
        data[[v]][data[[v]] %in% cont.na[[v]]] <- NA
      }
    }
  }

  ## --- detect variable types ---
  var_types <- detect_var_types(data)

  ## --- visit sequence ---
  if (is.null(visit.sequence)) {
    visit.sequence <- var_names
  } else {
    stopifnot(is.character(visit.sequence),
              all(visit.sequence %in% var_names))
  }

  ## --- method vector ---
  default_cont <- "mm"
  default_cat  <- "robust_logreg"

  if (is.null(method)) {
    # Default: robust methods
    method_vec <- ifelse(var_types == "continuous", default_cont, default_cat)
    names(method_vec) <- var_names
  } else if (length(method) == 1L && is.null(names(method))) {
    # Single string: backward compatible
    if (method == "robust_conditional") {
      method_vec <- ifelse(var_types == "continuous", "mm", "robust_logreg")
    } else if (method == "parametric") {
      method_vec <- ifelse(var_types == "continuous", "norm", "logreg")
    } else {
      cont_methods <- .methods_for_type("continuous")
      cat_methods  <- .methods_for_type("categorical")
      if (method %in% cont_methods && !(method %in% cat_methods)) {
        method_vec <- ifelse(var_types == "continuous", method, default_cat)
      } else if (method %in% cat_methods && !(method %in% cont_methods)) {
        method_vec <- ifelse(var_types == "continuous", default_cont, method)
      } else {
        method_vec <- rep(method, p)  # works for either type
      }
    }
    names(method_vec) <- var_names
  } else {
    # Named vector: per-variable
    stopifnot(all(names(method) %in% var_names))
    method_vec <- ifelse(var_types == "continuous", default_cont, default_cat)
    names(method_vec) <- var_names
    method_vec[names(method)] <- method[names(method)]
  }

  ## --- validate method names ---
  valid_methods <- .valid_method_names()
  bad <- setdiff(unique(method_vec), valid_methods)
  if (length(bad) > 0L) {
    stop("Unknown method(s): ", paste(bad, collapse = ", "),
         "\n  Valid methods: ", paste(valid_methods, collapse = ", "),
         call. = FALSE)
  }

  ## --- predictor matrix ---
  if (is.null(predictor.matrix)) {
    # Default: lower-triangular based on visit sequence
    predictor.matrix <- matrix(0L, p, p,
                               dimnames = list(var_names, var_names))
    vs_order <- match(visit.sequence, var_names)
    for (i in seq_along(vs_order)) {
      if (i > 1L) {
        prior <- vs_order[seq_len(i - 1L)]
        predictor.matrix[vs_order[i], prior] <- 1L
      }
    }
  } else {
    stopifnot(is.matrix(predictor.matrix),
              nrow(predictor.matrix) == p,
              ncol(predictor.matrix) == p)
    if (is.null(rownames(predictor.matrix))) {
      rownames(predictor.matrix) <- var_names
      colnames(predictor.matrix) <- var_names
    }
  }

  ## --- transformations ---
  bc_info <- list()  # stores lambda, shift for each transformed variable
  data_t <- data     # transformed copy for fitting

  if (!is.null(transform)) {
    for (v in names(transform)) {
      if (!(v %in% var_names) || var_types[v] != "continuous") next
      vals <- data[[v]]
      vals_nona <- vals[!is.na(vals)]
      tr_type <- transform[[v]]

      ## Shift so that values are strictly positive for Box-Cox.
      ## For all-positive data: no shift.  For zero-containing positive
      ## data: half the smallest non-zero value (keeps magnitudes close).
      ## For negative data: |min| + half the smallest non-zero spacing.
      shift <- 0
      if (any(vals_nona <= 0, na.rm = TRUE)) {
        pos <- vals_nona[vals_nona > 0]
        floor_shift <- if (length(pos) > 0L) min(pos) / 2 else 1
        if (any(vals_nona < 0, na.rm = TRUE)) {
          shift <- abs(min(vals_nona, na.rm = TRUE)) + floor_shift
        } else {
          shift <- floor_shift  # zero-containing positive data
        }
      }

      if (tr_type == "log") {
        lam <- 0
      } else if (tr_type == "sqrt") {
        lam <- 0.5
      } else if (tr_type == "boxcox") {
        lam <- robust_boxcox_lambda(vals_nona + shift)
      } else {
        next
      }

      bc_info[[v]] <- list(lambda = lam, shift = shift, type = tr_type)
      data_t[[v]] <- .bc_transform(vals + shift, lam)
    }
  }

  ## --- synthesis loop (repeated m times, optionally parallel) ---
  ## Per-draw seeds so the parallel and sequential paths produce the
  ## same output.  Workers get an explicit seed to restore reproducibility
  ## (set.seed in the main session is not propagated to parLapply workers).
  seed_vec <- if (!is.null(seed)) as.integer(seed) + seq_len(m) else
              sample.int(.Machine$integer.max, m)

  .run_one_synthesis <- function(mi) {
    set.seed(seed_vec[mi])
    synth <- data.frame(matrix(NA_real_, nrow = n_synth, ncol = p))
    names(synth) <- var_names

    # Preserve factor structure
    for (v in var_names) {
      if (is.factor(data[[v]])) {
        synth[[v]] <- factor(rep(NA, n_synth), levels = levels(data[[v]]))
      }
    }

    models <- vector("list", p)
    names(models) <- var_names

    for (idx in seq_along(visit.sequence)) {
      vname <- visit.sequence[idx]
      meth  <- method_vec[vname]

      ## skip variables with method = ""
      if (meth == "") {
        synth[[vname]] <- data[[vname]][seq_len(n_synth)]
        next
      }

      ## passive synthesis
      if (meth == "passive") {
        if (!is.null(formulas) && vname %in% names(formulas)) {
          fml <- formulas[[vname]]
          synth[[vname]] <- eval(fml[[2L]], envir = synth)
        }
        next
      }

      ## response variable (use transformed version if applicable)
      y <- if (vname %in% names(bc_info)) data_t[[vname]] else data[[vname]]

      ## custom formula?
      fml <- NULL
      if (!is.null(formulas) && vname %in% names(formulas)) {
        fml <- formulas[[vname]]
      }

      ## determine predictors
      if (!is.null(fml)) {
        # Extract predictor names from the formula
        fml_vars <- all.vars(fml)
        # Use all available variables referenced in the formula
        pred_names <- intersect(fml_vars, var_names)
        pred_names <- setdiff(pred_names, vname)  # exclude response
      } else {
        pred_idx <- which(predictor.matrix[vname, ] == 1)
        pred_names <- var_names[pred_idx]
      }

      if (length(pred_names) == 0L || idx == 1L) {
        y_raw <- data[[vname]]
        synth[[vname]] <- .synth_marginal(y_raw, var_types[vname],
                                          n = n_synth, w = wt_vec)
        next
      }

      X_orig <- data_t[, pred_names, drop = FALSE]
      X_new  <- synth[, pred_names, drop = FALSE]

      ## fit and generate (pass survey weights if available)
      fit <- .fit_dispatch(y, X_orig, meth, var_types[vname],
                           formula = fml, vname = vname,
                           obs_weights = wt_vec, ...)
      synth[[vname]] <- .generate_dispatch(fit, X_new, meth,
                                            var_types[vname], n_synth)
      models[[vname]] <- fit
    }

    ## back-transform
    for (v in names(bc_info)) {
      info <- bc_info[[v]]
      synth[[v]] <- .bc_inverse(synth[[v]], info$lambda) - info$shift
      synth[[v]] <- pmax(synth[[v]], min(data[[v]], na.rm = TRUE))
    }

    ## preserve factor levels
    for (v in var_names) {
      if (is.factor(data[[v]])) {
        synth[[v]] <- factor(synth[[v]], levels = levels(data[[v]]))
      }
    }

    list(synth = synth, models = models)
  }

  ## dispatch: parallel or sequential
  if (m > 1L && n_cores > 1L) {
    cl_par <- parallel::makeCluster(min(n_cores, m))
    on.exit(parallel::stopCluster(cl_par), add = TRUE)
    # Export only necessary objects (avoid serializing everything)
    export_vars <- c("data", "data_t", "var_names", "var_types",
                     "method_vec", "visit.sequence", "predictor.matrix",
                     "formulas", "bc_info", "n_synth", "wt_vec",
                     "p", "seed_vec", ".run_one_synthesis")
    export_vars <- intersect(export_vars, ls(environment()))
    parallel::clusterExport(cl_par, varlist = export_vars,
                            envir = environment())
    parallel::clusterEvalQ(cl_par, {
      suppressPackageStartupMessages(library(robSynth))
    })
    res_list <- parallel::parLapply(cl_par, seq_len(m), .run_one_synthesis)
  } else {
    res_list <- lapply(seq_len(m), .run_one_synthesis)
  }

  synth_list <- lapply(res_list, `[[`, "synth")
  models_last <- res_list[[m]]$models

  ## output
  out <- structure(
    list(
      synth            = if (m == 1L) synth_list[[1L]] else synth_list,
      original         = data,
      m                = m,
      method           = method_vec,
      visit.sequence   = visit.sequence,
      predictor.matrix = predictor.matrix,
      formulas         = formulas,
      transform        = transform,
      transform_info   = bc_info,
      models           = models_last,
      var_types        = var_types,
      n                = nrow(data),
      call             = cl
    ),
    class = "robsynth"
  )
  out
}


# ---- Method dispatcher ----------------------------------------------------

#' @keywords internal
.fit_dispatch <- function(y, X, method, var_type, formula = NULL,
                          vname = "y", obs_weights = NULL, ...) {
  entry <- .METHODS[[method]]
  if (is.null(entry))
    stop("Unknown method: ", method, call. = FALSE)
  if (!(var_type %in% entry$types))
    stop(sprintf("Method '%s' does not support var_type '%s'",
                 method, var_type), call. = FALSE)

  df <- cbind(data.frame(.y = y), X)
  n <- nrow(df)
  if (is.null(obs_weights)) obs_weights <- rep(1, n)

  if (!is.null(formula)) {
    fml <- stats::as.formula(paste(".y", paste(deparse(formula), collapse = "")))
  } else {
    fml <- stats::as.formula(paste(".y ~",
                                    paste(names(X), collapse = " + ")))
  }

  lvls <- if (var_type == "categorical") {
    if (is.factor(y)) levels(y) else sort(unique(y))
  } else character(0)
  n_levels <- length(lvls)

  entry$fit(y = y, X = X, df = df, fml = fml, obs_weights = obs_weights,
            var_type = var_type, lvls = lvls, n_levels = n_levels, n = n,
            ...)
}

#' @keywords internal
.generate_dispatch <- function(fit, X_new, method, var_type, n) {
  entry <- .METHODS[[method]]
  if (is.null(entry))
    stop("Unknown method: ", method, call. = FALSE)
  lvls <- if (var_type == "categorical") fit$levels else character(0)
  entry$gen(fit = fit, X_new = X_new, n = n, var_type = var_type, lvls = lvls)
}


# ---- Box-Cox helpers -------------------------------------------------------

#' Robust Box-Cox lambda estimation
#'
#' Estimates the Box-Cox transformation parameter by minimising a robust
#' skewness criterion (absolute difference of mean and median, divided by
#' MAD) over a grid of lambda values.
#'
#' @param y numeric vector (must be strictly positive)
#' @param lambda_grid grid of lambda values to search
#' @return scalar: estimated lambda
#' @references
#' Box G. E. P., Cox D. R. (1964).  An analysis of transformations.
#'   \emph{Journal of the Royal Statistical Society: Series B}, 26(2),
#'   211-252.
#'
#' Marazzi A., Yohai V. J. (2006).  Robust Box-Cox transformations
#'   based on minimum residual autocorrelation.  \emph{Computational
#'   Statistics & Data Analysis}, 50(10), 2752-2768.
#' @export
#' @examples
#' set.seed(1); x <- rlnorm(200)
#' robust_boxcox_lambda(x)  # should be near 0 (log)
robust_boxcox_lambda <- function(y, lambda_grid = seq(-1, 2, by = 0.05)) {
  stopifnot(is.numeric(y), all(y > 0, na.rm = TRUE))
  y <- y[!is.na(y)]
  best_lambda <- 1
  best_score  <- Inf
  n_valid <- 0L
  for (lam in lambda_grid) {
    yt <- .bc_transform(y, lam)
    m <- mad(yt)
    if (m < 1e-10) next
    score <- abs(mean(yt) - median(yt)) / m
    n_valid <- n_valid + 1L
    if (score < best_score) {
      best_score  <- score
      best_lambda <- lam
    }
  }
  if (n_valid == 0L) {
    warning("robust_boxcox_lambda: no grid value yielded a valid score ",
            "(MAD of transformed y < 1e-10 for all lambda); returning 1.",
            call. = FALSE)
  }
  best_lambda
}

#' @keywords internal
.bc_transform <- function(y, lambda) {
  if (abs(lambda) < 0.01) log(y) else (y^lambda - 1) / lambda
}

#' @keywords internal
.bc_inverse <- function(yt, lambda) {
  if (abs(lambda) < 0.01) {
    exp(yt)
  } else {
    # Guard against NaN: clamp lambda*yt+1 to be positive
    inner <- pmax(lambda * yt + 1, 1e-10)
    inner^(1 / lambda)
  }
}


# ---- Print / Summary -------------------------------------------------------

#' Print method for robsynth objects
#' @param x a \code{"robsynth"} object
#' @param ... ignored
#' @export
print.robsynth <- function(x, ...) {
  cat("robSynth result\n")
  cat("  Variables: ", length(x$var_types),
      "(", sum(x$var_types == "continuous"), "continuous,",
      sum(x$var_types == "categorical"), "categorical )\n")
  if (x$m > 1L) {
    cat("  Synthetic copies (m):", x$m, "\n")
  }
  cat("  Synthetic n:", if (x$m == 1L) nrow(x$synth) else nrow(x$synth[[1L]]),
      "\n")

  meths <- unique(x$method)
  cat("  Methods:", paste(meths, collapse = ", "), "\n")

  if (length(x$transform_info) > 0L) {
    cat("  Transformations:",
        paste(names(x$transform_info),
              sapply(x$transform_info, function(ti)
                sprintf("(lambda=%.2f)", ti$lambda)),
              collapse = ", "), "\n")
  }
  invisible(x)
}

#' Summary method for robsynth objects
#' @param object a \code{"robsynth"} object
#' @param ... ignored
#' @export
summary.robsynth <- function(object, ...) {
  cat("robSynth summary\n")
  cat("  Call: "); print(object$call); cat("\n")
  cat("  Visit sequence:", paste(object$visit.sequence, collapse = " -> "), "\n")
  cat("  Methods per variable:\n")
  print(object$method)
  if (object$m > 1L) {
    cat("  Synthetic copies:", object$m, "\n")
  }
  invisible(object)
}


# ---- Marginal synthesis ----------------------------------------------------

#' Marginal synthesis for a single variable
#'
#' @param y vector of original values
#' @param type \code{"continuous"} or \code{"categorical"}
#' @param n number of synthetic values
#' @return vector of length \code{n}
#' @keywords internal
.synth_marginal <- function(y, type, n, w = NULL) {
  keep <- !is.na(y)
  y <- y[keep]
  if (!is.null(w)) w <- w[keep]

  if (type == "continuous") {
    med <- median(y)
    s   <- mad(y)
    if (s < .Machine$double.eps) s <- sd(y)
    if (s < .Machine$double.eps) s <- 1
    bw <- 1.06 * s * length(y)^(-1 / 5)
    # Weighted resampling if weights provided
    prob <- if (!is.null(w)) w / sum(w) else NULL
    idx <- sample(length(y), n, replace = TRUE, prob = prob)
    return(y[idx] + rnorm(n, mean = 0, sd = bw))
  } else {
    if (!is.null(w)) {
      # Weighted proportions
      lvls <- if (is.factor(y)) levels(y) else sort(unique(y))
      probs <- vapply(lvls, function(l) sum(w[y == l]), numeric(1))
      probs <- probs / sum(probs)
    } else {
      tab <- table(y)
      probs <- tab / sum(tab)
      lvls <- names(probs)
    }
    drawn <- sample(lvls, size = n, replace = TRUE, prob = probs)
    if (is.factor(y)) return(factor(drawn, levels = levels(y)))
    return(drawn)
  }
}


# ---- Multiple synthesis inference ------------------------------------------

#' Fit a linear model on synthetic data
#'
#' When \code{m > 1}, fits the model on each synthetic copy and combines
#' estimates using Reiter's (2003) partially synthetic data combining rules.
#'
#' @param formula a model formula
#' @param object a \code{"robsynth"} object
#' @param ... additional arguments passed to \code{lm}
#'
#' @return A list with components:
#'   \item{coefficients}{combined point estimates}
#'   \item{se}{combined standard errors}
#'   \item{t}{t-statistics}
#'   \item{df}{degrees of freedom}
#'   \item{individual_fits}{list of \code{lm} objects (one per copy)}
#'
#' @references
#' Reiter J. P. (2003).  Inference for partially synthetic, public use
#'   microdata sets.  \emph{Survey Methodology}, 29(2), 181-188.
#'
#' @export
#' @examples
#' \dontrun{
#' res <- robsynth(iris, m = 5)
#' fit <- synth_lm(Sepal.Length ~ Sepal.Width + Petal.Length, res)
#' fit$coefficients
#' }
synth_lm <- function(formula, object, ...) {
  stopifnot(inherits(object, "robsynth"))
  synth_data <- if (object$m == 1L) list(object$synth) else object$synth

  fits <- lapply(synth_data, function(s) stats::lm(formula, data = s, ...))
  .combine_fits(fits, object$m, object$n)
}

#' Fit a GLM on synthetic data
#'
#' @param formula a model formula
#' @param object a \code{"robsynth"} object
#' @param family a family object (e.g., \code{binomial()})
#' @param ... additional arguments passed to \code{glm}
#' @return same structure as \code{\link{synth_lm}}
#' @export
synth_glm <- function(formula, object, family = stats::gaussian(), ...) {
  stopifnot(inherits(object, "robsynth"))
  synth_data <- if (object$m == 1L) list(object$synth) else object$synth

  fits <- lapply(synth_data, function(s)
    stats::glm(formula, data = s, family = family, ...))
  .combine_fits(fits, object$m, object$n)
}

#' @keywords internal
.combine_fits <- function(fits, m, n_orig) {
  coefs <- lapply(fits, stats::coef)
  vcovs <- lapply(fits, stats::vcov)

  nms <- names(coefs[[1L]])
  q <- length(nms)

  ## Reiter (2003) combining rules for PARTIALLY synthetic data.
  ## Q_bar = mean of point estimates
  Q_bar <- Reduce("+", coefs) / m

  ## U_bar = mean of within-imputation variances
  U_bar <- Reduce("+", vcovs) / m

  ## B = between-imputation variance
  if (m > 1L) {
    B_mat <- matrix(0, q, q)
    for (i in seq_len(m)) {
      diff <- coefs[[i]] - Q_bar
      B_mat <- B_mat + outer(diff, diff)
    }
    B_mat <- B_mat / (m - 1L)
  } else {
    B_mat <- matrix(0, q, q)
  }

  ## Total variance for partially synthetic data (Reiter 2003):
  ##   T_p = U_bar + B / m
  T_diag <- diag(U_bar) + diag(B_mat) / m
  T_diag <- pmax(T_diag, 0)

  se <- sqrt(T_diag)
  t_stat <- Q_bar / se

  ## Degrees of freedom for partially synthetic data (Reiter 2003):
  ##   df = (m - 1) * (1 + m * U_bar / B)^2
  if (m > 1L) {
    r <- diag(B_mat) / pmax(diag(U_bar), 1e-10) / m   # B / (m * U_bar)
    df <- (m - 1) * (1 + 1 / pmax(r, 1e-10))^2
  } else {
    df <- rep(Inf, q)
  }

  list(
    coefficients    = Q_bar,
    se              = se,
    t               = t_stat,
    df              = df,
    individual_fits = fits,
    m               = m
  )
}

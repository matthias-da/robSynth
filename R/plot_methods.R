#' Compare original and synthetic data visually
#'
#' Creates side-by-side visual comparisons of original versus synthetic
#' data for each variable. Continuous variables are shown as overlaid
#' density plots; categorical variables as grouped bar charts. Each
#' continuous facet label includes the Kolmogorov-Smirnov statistic.
#'
#' @param object an object of class \code{"robsynth"}.
#' @param original a data.frame containing the original data. Required
#'   because the \code{robsynth} object does not store the original data
#'   internally.
#' @param vars character vector of variable names to include. Default:
#'   all variables in the synthetic data.
#' @param ... additional arguments (currently unused).
#'
#' @return A \code{ggplot} object (invisibly).
#'
#' @details
#' Colours follow the Okabe-Ito palette for colourblind safety:
#' original data in black (\code{"#000000"}), synthetic data in orange
#' (\code{"#E69F00"}).
#'
#' The Kolmogorov-Smirnov statistic shown in the facet labels for
#' continuous variables is computed via \code{\link[stats]{ks.test}}
#' and measures the maximum absolute difference between the empirical
#' CDFs. Lower values indicate better distributional preservation.
#'
#' @author Matthias Templ
#' @export
#' @seealso \code{\link{plot.robsynth}}, \code{\link{synth_utility}}
#' @examples
#' \dontrun{
#' data(iris)
#' res <- robsynth(iris, method = "robust_conditional")
#' compare(res, original = iris)
#' compare(res, original = iris, vars = c("Sepal.Length", "Species"))
#' }
compare <- function(object, ...) {
  UseMethod("compare")
}

#' @rdname compare
#' @export
compare.robsynth <- function(object, original, vars = NULL, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for compare(). ",
         "Install it with install.packages(\"ggplot2\").",
         call. = FALSE)
  }

  stopifnot(is.data.frame(original))
  # Handle m > 1: use first synthetic copy
  synth <- if (is.data.frame(object$synth)) object$synth else object$synth[[1L]]
  var_types <- object$var_types

  ## subset variables if requested

  if (is.null(vars)) {
    vars <- names(synth)
  } else {
    stopifnot(all(vars %in% names(synth)))
  }

  ## Okabe-Ito colours
  col_orig  <- "#000000"
  col_synth <- "#E69F00"

  ## separate continuous and categorical variables
  cont_vars <- vars[var_types[vars] == "continuous"]
  cat_vars  <- vars[var_types[vars] == "categorical"]

  plot_list <- list()

  ## --- continuous variables: overlay density plots ---
  if (length(cont_vars) > 0L) {
    ## compute KS statistics for facet labels
    ks_stats <- vapply(cont_vars, function(v) {
      suppressWarnings(
        stats::ks.test(original[[v]], synth[[v]])$statistic
      )
    }, numeric(1L))

    facet_labels <- stats::setNames(
      paste0(cont_vars, "  (KS = ", formatC(ks_stats, format = "f",
                                              digits = 3), ")"),
      cont_vars
    )

    ## build long-form data for ggplot
    df_cont <- do.call(rbind, lapply(cont_vars, function(v) {
      data.frame(
        variable = facet_labels[v],
        value    = c(original[[v]], synth[[v]]),
        source   = rep(c("Original", "Synthetic"),
                       c(nrow(original), nrow(synth))),
        stringsAsFactors = FALSE
      )
    }))

    df_cont$source <- factor(df_cont$source,
                             levels = c("Original", "Synthetic"))

    p_cont <- ggplot2::ggplot(
      df_cont,
      ggplot2::aes(x = .data$value, colour = .data$source,
                   fill = .data$source)
    ) +
      ggplot2::geom_density(alpha = 0.15, linewidth = 0.8) +
      ggplot2::facet_wrap(~ variable, scales = "free") +
      ggplot2::scale_colour_manual(
        values = c("Original" = col_orig, "Synthetic" = col_synth)
      ) +
      ggplot2::scale_fill_manual(
        values = c("Original" = col_orig, "Synthetic" = col_synth)
      ) +
      ggplot2::labs(x = NULL, y = "Density", colour = NULL, fill = NULL) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "bottom")

    plot_list$continuous <- p_cont
  }

  ## --- categorical variables: grouped bar charts ---
  if (length(cat_vars) > 0L) {
    df_cat <- do.call(rbind, lapply(cat_vars, function(v) {
      tab_orig  <- prop.table(table(original[[v]]))
      tab_synth <- prop.table(table(synth[[v]]))
      all_lvls  <- union(names(tab_orig), names(tab_synth))
      data.frame(
        variable = v,
        level    = rep(all_lvls, 2),
        proportion = c(
          as.numeric(tab_orig[all_lvls]),
          as.numeric(tab_synth[all_lvls])
        ),
        source = rep(c("Original", "Synthetic"), each = length(all_lvls)),
        stringsAsFactors = FALSE
      )
    }))

    ## replace NAs from missing levels with 0
    df_cat$proportion[is.na(df_cat$proportion)] <- 0

    df_cat$source <- factor(df_cat$source,
                            levels = c("Original", "Synthetic"))

    p_cat <- ggplot2::ggplot(
      df_cat,
      ggplot2::aes(x = .data$level, y = .data$proportion,
                   fill = .data$source)
    ) +
      ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8),
                        width = 0.7) +
      ggplot2::facet_wrap(~ variable, scales = "free") +
      ggplot2::scale_fill_manual(
        values = c("Original" = col_orig, "Synthetic" = col_synth)
      ) +
      ggplot2::labs(x = NULL, y = "Proportion", fill = NULL) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "bottom",
                     axis.text.x = ggplot2::element_text(
                       angle = 45, hjust = 1))

    plot_list$categorical <- p_cat
  }

  ## --- combine and return ---
  if (length(plot_list) == 1L) {
    p <- plot_list[[1L]]
  } else if (length(plot_list) == 2L) {
    ## stack continuous on top, categorical below
    p <- .stack_plots(plot_list$continuous, plot_list$categorical)
  } else {
    message("No plottable variables found.")
    return(invisible(NULL))
  }

  ## gtable objects (returned by gridExtra::arrangeGrob for the mixed-type
  ## case) require grid::grid.draw to render in both interactive and knitr
  ## contexts; plain print() on a gtable is a no-op.
  if (inherits(p, "gtable")) {
    grid::grid.newpage()
    grid::grid.draw(p)
  } else {
    print(p)
  }
  invisible(p)
}


#' Plot a robsynth object
#'
#' Flexible plotting for synthetic data produced by \code{\link{robsynth}}.
#' Supports density comparisons, correlation heatmaps, and pairwise
#' scatter plots.
#'
#' @param x an object of class \code{"robsynth"}.
#' @param type character; type of plot. One of:
#'   \describe{
#'     \item{\code{"density"}}{Overlay density plots of original vs
#'       synthetic (requires \code{original}).}
#'     \item{\code{"correlation"}}{Side-by-side correlation heatmaps
#'       (original vs synthetic, requires \code{original}).}
#'     \item{\code{"scatter"}}{Pairwise scatter plots of the first 2--3
#'       continuous variables (requires \code{original}).}
#'   }
#' @param original a data.frame containing the original data. Required
#'   for all plot types.
#' @param vars character vector of variable names to include. For
#'   \code{type = "scatter"}, at most the first 3 continuous variables
#'   are used. Default: all variables.
#' @param layout for \code{type = "scatter"}: either \code{"overlay"}
#'   (default) to plot original and synthetic on the same axes with
#'   transparency, or \code{"facet"} to draw them in two side-by-side
#'   panels.  Ignored for other plot types.
#' @param alpha point transparency for scatter plots.  Defaults to
#'   \code{0.4} for \code{layout = "overlay"} and \code{0.6} for
#'   \code{layout = "facet"}.
#' @param cor_method for \code{type = "correlation"}: \code{"pearson"}
#'   (default, classical) or \code{"robust"} (OGK-based; resistant to
#'   cellwise contamination).  Under contamination, the classical
#'   correlation of the original data can be heavily distorted and
#'   will not match a successful robust synthesiser's output even when
#'   the synthesis recovered the clean structure.
#' @param include_robust_original logical; when \code{TRUE} and
#'   \code{cor_method = "pearson"}, the correlation plot shows three
#'   panels side-by-side: the classical correlation of the original,
#'   the robust correlation of the original (what the clean truth
#'   "would" look like), and the classical correlation of the
#'   synthetic data.  Useful for visualising the cellwise-correlation
#'   paradox.
#' @param ... additional arguments (currently unused).
#'
#' @return A \code{ggplot} object (invisibly).
#'
#' @details
#' Colours follow the Okabe-Ito palette: original = \code{"#000000"}
#' (black), synthetic = \code{"#E69F00"} (orange).
#'
#' For \code{type = "density"}, this method produces the same output
#' as \code{\link{compare.robsynth}} but with slightly different default
#' styling (darker fill, thinner lines).
#'
#' For \code{type = "correlation"}, only continuous variables are used.
#' The heatmaps show Pearson correlations with a diverging blue--white--red
#' colour scale.
#'
#' For \code{type = "scatter"} the \code{layout} argument controls how
#' the two sources are juxtaposed.  \code{"overlay"} (default) plots
#' them on a shared axis, colour-coded, with point transparency
#' controlled by \code{alpha}; differences show up as orange clouds
#' that do or do not track the grey bulk.  \code{"facet"} draws each
#' source in its own panel side-by-side, which makes shape-of-cloud
#' differences (tails, holes) easier to read when the two clouds
#' overlap heavily.
#'
#' @author Matthias Templ
#' @export
#' @seealso \code{\link{compare.robsynth}}, \code{\link{synth_utility}}
#' @examples
#' \dontrun{
#' data(iris)
#' res <- robsynth(iris, method = "robust_conditional")
#' plot(res, type = "density", original = iris)
#' plot(res, type = "correlation", original = iris)
#' plot(res, type = "scatter", original = iris)                       # overlay
#' plot(res, type = "scatter", original = iris, layout = "facet")     # panels
#' }
plot.robsynth <- function(x, type = c("density", "correlation", "scatter"),
                          original = NULL, vars = NULL,
                          layout = c("overlay", "facet"),
                          alpha = NULL,
                          cor_method = c("pearson", "robust"),
                          include_robust_original = FALSE, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for plot.robsynth(). ",
         "Install it with install.packages(\"ggplot2\").",
         call. = FALSE)
  }

  type       <- match.arg(type)
  layout     <- match.arg(layout)
  cor_method <- match.arg(cor_method)
  if (is.null(alpha)) alpha <- if (layout == "facet") 0.6 else 0.4

  if (is.null(original)) {
    stop("Argument 'original' is required. The robsynth object does not ",
         "store the original data; please pass it explicitly.",
         call. = FALSE)
  }
  stopifnot(is.data.frame(original))

  # Handle m > 1: use first synthetic copy
  synth     <- if (is.data.frame(x$synth)) x$synth else x$synth[[1L]]
  var_types <- x$var_types

  if (is.null(vars)) {
    vars <- names(synth)
  } else {
    stopifnot(all(vars %in% names(synth)))
  }

  p <- switch(type,
    density     = .plot_density(original, synth, var_types, vars),
    correlation = .plot_correlation(original, synth, var_types, vars,
                                     cor_method = cor_method,
                                     include_robust_original =
                                       include_robust_original),
    scatter     = .plot_scatter(original, synth, var_types, vars,
                                 layout = layout, alpha = alpha)
  )

  ## gtable objects (from .arrange_scatter_panels / .stack_plots) require
  ## grid::grid.draw to render; print() dumps their text structure.
  if (inherits(p, "gtable")) {
    grid::grid.newpage()
    grid::grid.draw(p)
  } else {
    print(p)
  }
  invisible(p)
}


# ---- internal plotting helpers ----------------------------------------------

#' Overlay density plot (used by both plot.robsynth and compare)
#' @keywords internal
.plot_density <- function(original, synth, var_types, vars) {
  col_orig  <- "#000000"
  col_synth <- "#E69F00"

  cont_vars <- vars[var_types[vars] == "continuous"]
  if (length(cont_vars) == 0L) {
    stop("No continuous variables found for density plot.", call. = FALSE)
  }

  ## KS statistics for facet labels
  ks_stats <- vapply(cont_vars, function(v) {
    suppressWarnings(
      stats::ks.test(original[[v]], synth[[v]])$statistic
    )
  }, numeric(1L))

  facet_labels <- stats::setNames(
    paste0(cont_vars, "  (KS = ", formatC(ks_stats, format = "f",
                                            digits = 3), ")"),
    cont_vars
  )

  df <- do.call(rbind, lapply(cont_vars, function(v) {
    data.frame(
      variable = facet_labels[v],
      value    = c(original[[v]], synth[[v]]),
      source   = rep(c("Original", "Synthetic"),
                     c(nrow(original), nrow(synth))),
      stringsAsFactors = FALSE
    )
  }))
  df$source <- factor(df$source, levels = c("Original", "Synthetic"))

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$value, colour = .data$source,
                 fill = .data$source)
  ) +
    ggplot2::geom_density(alpha = 0.10, linewidth = 0.6) +
    ggplot2::facet_wrap(~ variable, scales = "free") +
    ggplot2::scale_colour_manual(
      values = c("Original" = col_orig, "Synthetic" = col_synth)
    ) +
    ggplot2::scale_fill_manual(
      values = c("Original" = col_orig, "Synthetic" = col_synth)
    ) +
    ggplot2::labs(x = NULL, y = "Density", colour = NULL, fill = NULL,
                  title = "Density comparison: original vs synthetic") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")
}


#' Side-by-side correlation heatmaps
#'
#' @param original,synth the two data frames
#' @param var_types named character vector
#' @param vars character vector of variable names
#' @param cor_method \code{"pearson"} (default, classical Pearson
#'   correlation) or \code{"robust"} (Orthogonalised Gnanadesikan-Kettenring
#'   via \code{robustbase::covOGK}).  Under cellwise contamination the
#'   classical correlation of the original data is itself distorted;
#'   the \code{"robust"} option gives a sense of what the clean
#'   correlation would look like.
#' @param include_robust_original logical; when \code{TRUE} and
#'   \code{cor_method = "pearson"}, the correlation plot becomes a
#'   four-panel display (classical original, classical synthetic,
#'   robust original, robust synthetic) arranged in a 2-by-2 grid.
#'   Comparing the rows shows the effect of contamination on the
#'   measurement itself; comparing the columns shows the effect of
#'   synthesis on the dataset.
#'
#' @keywords internal
.plot_correlation <- function(original, synth, var_types, vars,
                              cor_method = c("pearson", "robust"),
                              include_robust_original = FALSE) {
  cor_method <- match.arg(cor_method)
  cont_vars <- vars[var_types[vars] == "continuous"]
  if (length(cont_vars) < 2L) {
    stop("At least 2 continuous variables needed for correlation plot.",
         call. = FALSE)
  }

  ## convert to long format
  .cor_to_long <- function(mat, label) {
    vars_r <- rownames(mat)
    vars_c <- colnames(mat)
    df <- expand.grid(Var1 = vars_r, Var2 = vars_c,
                      stringsAsFactors = FALSE)
    df$correlation <- as.vector(mat)
    df$panel <- label
    df
  }

  if (isTRUE(include_robust_original) && cor_method == "pearson") {
    ## Four-panel 2x2 grid: rows = measurement (classical / robust),
    ## cols = data source (original / synthetic).
    cor_orig_p  <- .compute_cor(original[, cont_vars, drop = FALSE], "pearson")
    cor_synth_p <- .compute_cor(synth[,    cont_vars, drop = FALSE], "pearson")
    cor_orig_r  <- .compute_cor(original[, cont_vars, drop = FALSE], "robust")
    cor_synth_r <- .compute_cor(synth[,    cont_vars, drop = FALSE], "robust")
    panels <- list(
      .cor_to_long(cor_orig_p,  "Original (pearson)"),
      .cor_to_long(cor_synth_p, "Synthetic (pearson)"),
      .cor_to_long(cor_orig_r,  "Original (robust)"),
      .cor_to_long(cor_synth_r, "Synthetic (robust)")
    )
    panel_order <- c("Original (pearson)",  "Synthetic (pearson)",
                     "Original (robust)",   "Synthetic (robust)")
  } else {
    cor_orig  <- .compute_cor(original[, cont_vars, drop = FALSE], cor_method)
    cor_synth <- .compute_cor(synth[,    cont_vars, drop = FALSE], cor_method)
    panels <- list(
      .cor_to_long(cor_orig,  paste0("Original (",  cor_method, ")")),
      .cor_to_long(cor_synth, paste0("Synthetic (", cor_method, ")"))
    )
    panel_order <- sapply(panels, function(p) p$panel[1L])
  }
  df <- do.call(rbind, panels)
  df$panel <- factor(df$panel, levels = panel_order)

  ## reverse y-axis order for standard matrix display
  df$Var1 <- factor(df$Var1, levels = rev(cont_vars))
  df$Var2 <- factor(df$Var2, levels = cont_vars)

  ggplot2::ggplot(
    df,
    ggplot2::aes(x = .data$Var2, y = .data$Var1,
                 fill = .data$correlation)
  ) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.3) +
    ggplot2::geom_text(
      ggplot2::aes(label = formatC(.data$correlation, format = "f",
                                    digits = 2)),
      size = 3, colour = "grey20"
    ) +
    ggplot2::facet_wrap(~ panel, ncol = 2) +
    ggplot2::scale_fill_gradient2(
      low = "#2166AC", mid = "white", high = "#B2182B",
      midpoint = 0, limits = c(-1, 1),
      name = "Correlation"
    ) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = "Correlation heatmaps: original vs synthetic") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
}


#' Pairwise scatter plots
#' @keywords internal
.plot_scatter <- function(original, synth, var_types, vars,
                          layout = c("overlay", "facet"), alpha = 0.4) {
  layout <- match.arg(layout)
  col_orig_scatter <- "#999999"
  col_synth        <- "#E69F00"

  cont_vars <- vars[var_types[vars] == "continuous"]
  if (length(cont_vars) < 2L) {
    stop("At least 2 continuous variables needed for scatter plot.",
         call. = FALSE)
  }

  ## limit to first 3 continuous variables
  cont_vars <- cont_vars[seq_len(min(3L, length(cont_vars)))]

  ## generate all pairwise combinations
  pairs <- utils::combn(cont_vars, 2, simplify = FALSE)

  plot_list <- lapply(pairs, function(pair) {
    xvar <- pair[1L]
    yvar <- pair[2L]

    df <- data.frame(
      x = c(original[[xvar]], synth[[xvar]]),
      y = c(original[[yvar]], synth[[yvar]]),
      source = rep(c("Original", "Synthetic"),
                   c(nrow(original), nrow(synth))),
      stringsAsFactors = FALSE
    )
    df$source <- factor(df$source, levels = c("Original", "Synthetic"))

    base <- ggplot2::ggplot(
      df,
      ggplot2::aes(x = .data$x, y = .data$y, colour = .data$source)
    ) +
      ggplot2::geom_point(alpha = alpha, size = 1.2) +
      ggplot2::scale_colour_manual(
        values = c("Original" = col_orig_scatter,
                   "Synthetic" = col_synth)
      ) +
      ggplot2::labs(x = xvar, y = yvar, colour = NULL) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "bottom")

    if (layout == "facet") {
      ## side-by-side panels; suppress the redundant colour legend
      base +
        ggplot2::facet_wrap(~ .data$source) +
        ggplot2::guides(colour = "none")
    } else {
      base
    }
  })

  ## combine scatter panels
  if (length(plot_list) == 1L) {
    p <- plot_list[[1L]] +
      ggplot2::ggtitle("Scatter: original vs synthetic")
  } else {
    p <- .arrange_scatter_panels(plot_list)
  }

  p
}


#' Classical or robust correlation matrix of a numeric data frame
#'
#' Returns a Pearson correlation matrix when
#' \code{method = "pearson"}, or an OGK-based robust correlation when
#' \code{method = "robust"}.  The OGK estimator has bounded influence
#' and is therefore resistant to both casewise and cellwise outliers,
#' so it gives a usable estimate of the clean-data correlation even
#' when the input contains contamination.
#'
#' @keywords internal
.compute_cor <- function(X, method = c("pearson", "robust")) {
  method <- match.arg(method)
  X <- as.matrix(X[, vapply(as.data.frame(X), is.numeric, logical(1)),
                   drop = FALSE])
  nms <- colnames(X)
  if (method == "pearson") {
    out <- stats::cor(X, use = "pairwise.complete.obs")
  } else {
    if (!requireNamespace("robustbase", quietly = TRUE))
      stop("Package 'robustbase' required for robust correlation.")
    ogk <- robustbase::covOGK(X, sigmamu = robustbase::scaleTau2)
    out <- stats::cov2cor(ogk$cov)
  }
  dimnames(out) <- list(nms, nms)
  out
}

#' Stack two ggplot objects vertically
#'
#' Simple vertical stacking using \code{gridExtra::arrangeGrob} if
#' available, otherwise prints sequentially.
#' @keywords internal
.stack_plots <- function(p_top, p_bottom) {
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    gridExtra::arrangeGrob(p_top, p_bottom, ncol = 1)
  } else {
    ## fallback: return just the continuous plot with a message
    message("Install 'gridExtra' to combine continuous and categorical ",
            "plots in one figure. Showing continuous variables only.")
    p_top
  }
}


#' Arrange scatter panels in a grid
#' @keywords internal
.arrange_scatter_panels <- function(plot_list) {
  if (requireNamespace("gridExtra", quietly = TRUE)) {
    n <- length(plot_list)
    ncol <- min(n, 3L)
    do.call(gridExtra::arrangeGrob,
            c(plot_list, list(ncol = ncol,
                              top = "Scatter: original vs synthetic")))
  } else {
    ## fallback: return the first panel
    message("Install 'gridExtra' to arrange multiple scatter panels. ",
            "Showing first pair only.")
    plot_list[[1L]] +
      ggplot2::ggtitle("Scatter: original vs synthetic")
  }
}

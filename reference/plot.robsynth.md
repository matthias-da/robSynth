# Plot a robsynth object

Flexible plotting for synthetic data produced by
[`robsynth`](https://matthias-da.github.io/robSynth/reference/robsynth.md).
Supports density comparisons, correlation heatmaps, and pairwise scatter
plots.

## Usage

``` r
# S3 method for class 'robsynth'
plot(
  x,
  type = c("density", "correlation", "scatter"),
  original = NULL,
  vars = NULL,
  layout = c("overlay", "facet"),
  alpha = NULL,
  cor_method = c("pearson", "robust"),
  include_robust_original = FALSE,
  ...
)
```

## Arguments

- x:

  an object of class `"robsynth"`.

- type:

  character; type of plot. One of:

  `"density"`

  :   Overlay density plots of original vs synthetic (requires
      `original`).

  `"correlation"`

  :   Side-by-side correlation heatmaps (original vs synthetic, requires
      `original`).

  `"scatter"`

  :   Pairwise scatter plots of the first 2–3 continuous variables
      (requires `original`).

- original:

  a data.frame containing the original data. Required for all plot
  types.

- vars:

  character vector of variable names to include. For `type = "scatter"`,
  at most the first 3 continuous variables are used. Default: all
  variables.

- layout:

  for `type = "scatter"`: either `"overlay"` (default) to plot original
  and synthetic on the same axes with transparency, or `"facet"` to draw
  them in two side-by-side panels. Ignored for other plot types.

- alpha:

  point transparency for scatter plots. Defaults to `0.4` for
  `layout = "overlay"` and `0.6` for `layout = "facet"`.

- cor_method:

  for `type = "correlation"`: `"pearson"` (default, classical) or
  `"robust"` (OGK-based; resistant to cellwise contamination). Under
  contamination, the classical correlation of the original data can be
  heavily distorted and will not match a successful robust synthesiser's
  output even when the synthesis recovered the clean structure.

- include_robust_original:

  logical; when `TRUE` and `cor_method = "pearson"`, the correlation
  plot shows three panels side-by-side: the classical correlation of the
  original, the robust correlation of the original (what the clean truth
  "would" look like), and the classical correlation of the synthetic
  data. Useful for visualising the cellwise-correlation paradox.

- ...:

  additional arguments (currently unused).

## Value

A `ggplot` object (invisibly).

## Details

Colours follow the Okabe-Ito palette: original = `"#000000"` (black),
synthetic = `"#E69F00"` (orange).

For `type = "density"`, this method produces the same output as
[`compare.robsynth`](https://matthias-da.github.io/robSynth/reference/compare.md)
but with slightly different default styling (darker fill, thinner
lines).

For `type = "correlation"`, only continuous variables are used. The
heatmaps show Pearson correlations with a diverging blue–white–red
colour scale.

For `type = "scatter"` the `layout` argument controls how the two
sources are juxtaposed. `"overlay"` (default) plots them on a shared
axis, colour-coded, with point transparency controlled by `alpha`;
differences show up as orange clouds that do or do not track the grey
bulk. `"facet"` draws each source in its own panel side-by-side, which
makes shape-of-cloud differences (tails, holes) easier to read when the
two clouds overlap heavily.

## See also

[`compare.robsynth`](https://matthias-da.github.io/robSynth/reference/compare.md),
[`synth_utility`](https://matthias-da.github.io/robSynth/reference/synth_utility.md)

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
data(CrohnD, package = "robustbase")
dat <- CrohnD[, -1]
res <- robsynth(dat, method = "robust_conditional")
plot(res, type = "density",     original = dat)
plot(res, type = "correlation", original = dat)
plot(res, type = "scatter",     original = dat)                   # overlay
plot(res, type = "scatter",     original = dat, layout = "facet") # panels
} # }
```

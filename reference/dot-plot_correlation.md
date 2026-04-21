# Side-by-side correlation heatmaps

Side-by-side correlation heatmaps

## Usage

``` r
.plot_correlation(
  original,
  synth,
  var_types,
  vars,
  cor_method = c("pearson", "robust"),
  include_robust_original = FALSE
)
```

## Arguments

- original, synth:

  the two data frames

- var_types:

  named character vector

- vars:

  character vector of variable names

- cor_method:

  `"pearson"` (default, classical Pearson correlation) or `"robust"`
  (Orthogonalised Gnanadesikan-Kettenring via
  [`robustbase::covOGK`](https://rdrr.io/pkg/robustbase/man/covOGK.html)).
  Under cellwise contamination the classical correlation of the original
  data is itself distorted; the `"robust"` option gives a sense of what
  the clean correlation would look like.

- include_robust_original:

  logical; when `TRUE` and `cor_method = "pearson"`, the correlation
  plot becomes a four-panel display (classical original, classical
  synthetic, robust original, robust synthetic) arranged in a 2-by-2
  grid. Comparing the rows shows the effect of contamination on the
  measurement itself; comparing the columns shows the effect of
  synthesis on the dataset.

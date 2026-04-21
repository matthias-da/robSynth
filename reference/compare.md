# Compare original and synthetic data visually

Creates side-by-side visual comparisons of original versus synthetic
data for each variable. Continuous variables are shown as overlaid
density plots; categorical variables as grouped bar charts. Each
continuous facet label includes the Kolmogorov-Smirnov statistic.

## Usage

``` r
compare(object, ...)

# S3 method for class 'robsynth'
compare(object, original, vars = NULL, ...)
```

## Arguments

- object:

  an object of class `"robsynth"`.

- ...:

  additional arguments (currently unused).

- original:

  a data.frame containing the original data. Required because the
  `robsynth` object does not store the original data internally.

- vars:

  character vector of variable names to include. Default: all variables
  in the synthetic data.

## Value

A `ggplot` object (invisibly).

## Details

Colours follow the Okabe-Ito palette for colourblind safety: original
data in black (`"#000000"`), synthetic data in orange (`"#E69F00"`).

The Kolmogorov-Smirnov statistic shown in the facet labels for
continuous variables is computed via
[`ks.test`](https://rdrr.io/r/stats/ks.test.html) and measures the
maximum absolute difference between the empirical CDFs. Lower values
indicate better distributional preservation.

## See also

[`plot.robsynth`](https://matthias-da.github.io/robSynth/reference/plot.robsynth.md),
[`synth_utility`](https://matthias-da.github.io/robSynth/reference/synth_utility.md)

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
data(CrohnD, package = "robustbase")
dat <- CrohnD[, -1]
res <- robsynth(dat, method = "robust_conditional")
compare(res, original = dat)
compare(res, original = dat, vars = c("BMI", "treat"))
} # }
```

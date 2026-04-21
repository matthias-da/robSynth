# Classical or robust correlation matrix of a numeric data frame

Returns a Pearson correlation matrix when `method = "pearson"`, or an
OGK-based robust correlation when `method = "robust"`. The OGK estimator
has bounded influence and is therefore resistant to both casewise and
cellwise outliers, so it gives a usable estimate of the clean-data
correlation even when the input contains contamination.

## Usage

``` r
.compute_cor(X, method = c("pearson", "robust"))
```

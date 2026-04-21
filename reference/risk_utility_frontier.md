# Risk-utility frontier analysis

Compares multiple synthetic datasets on the risk-utility frontier
(Duncan-Lambert R-U confidentiality map). Each synthesizer produces a
point in (risk, utility) space; the frontier shows which methods are
Pareto-optimal.

## Usage

``` r
risk_utility_frontier(
  orig,
  synth_list,
  utility_metric = "pMSE",
  risk_metric = "distance",
  key_vars = NULL,
  sensitive_var = NULL
)
```

## Arguments

- orig:

  original data.frame

- synth_list:

  named list of synthetic data.frames to compare

- utility_metric:

  which utility metric to use as scalar. Default: `"pMSE"`.

- risk_metric:

  which risk metric to use as scalar. Default: `"distance"`.

- key_vars:

  passed to
  [`synth_risk`](https://matthias-da.github.io/robSynth/reference/synth_risk.md)

- sensitive_var:

  passed to
  [`synth_risk`](https://matthias-da.github.io/robSynth/reference/synth_risk.md)

## Value

A data.frame of class `"ru_frontier"` with columns: `method`, `utility`,
`risk`, `pareto_optimal`.

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
data(CrohnD, package = "robustbase")
dat  <- CrohnD[, -1]
cont <- c("BMI", "height", "age", "weight")
s1   <- dat[sample(nrow(dat), replace = TRUE), ]
s2   <- dat
s2[, cont] <- as.matrix(s2[, cont]) +
             matrix(rnorm(nrow(dat) * length(cont), sd = 0.5),
                    ncol = length(cont))
res <- risk_utility_frontier(dat, list(bootstrap = s1, noisy = s2))
plot(res$utility, res$risk, pch = 19)
text(res$utility, res$risk, res$method, pos = 3)
} # }
```

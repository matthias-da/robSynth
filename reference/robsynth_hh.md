# Robust synthesis for hierarchical (household/person) data

Synthesises data with household-person structure in two phases:
(1)~resample complete households and synthesise household-level
variables, (2)~synthesise person-level variables within each household
conditional on household-level variables.

## Usage

``` r
robsynth_hh(
  data,
  hhid,
  pid = NULL,
  hh_vars = NULL,
  person_vars = NULL,
  method = NULL,
  m = 1L,
  weights = NULL,
  seed = NULL,
  ...
)
```

## Arguments

- data:

  a data.frame with household and person identifiers

- hhid:

  character: name of the household identifier column

- pid:

  character: name of the person identifier column (optional; if NULL,
  row number within household is used)

- hh_vars:

  character vector of household-level variable names (constant within
  household)

- person_vars:

  character vector of person-level variable names to synthesise

- method:

  synthesis method(s), as in
  [`robsynth`](https://matthias-da.github.io/robSynth/reference/robsynth.md)

- m:

  number of synthetic copies

- weights:

  survey weight column name (optional)

- seed:

  random seed

- ...:

  additional arguments passed to internal `robsynth` calls

## Value

An S3 object of class `"robsynth_hh"` with components:

- synth:

  synthetic data.frame (or list when m \> 1)

- m:

  number of copies

- hhid:

  household ID column name

- n_hh:

  number of households

- n_persons:

  number of persons

- call:

  the matched call

## Details

Phase 1 (Structure): Households are resampled with replacement from the
original data. Household-level variables (specified via `hh_vars`) are
carried over from the resampled households. Household sizes are
preserved.

Phase 2 (Person-level): For each person-level variable, a robust model
is fitted on the full original data using all previously synthesised
variables (including household-level) as predictors. Synthetic values
are drawn conditional on the household context.

## Author

Matthias Templ

## Examples

``` r
if (FALSE) { # \dontrun{
# Create simple household data
hh_data <- data.frame(
  hhid = rep(1:100, each = 3),
  hh_income = rep(rnorm(100, 50000, 10000), each = 3),
  hh_region = rep(sample(c("A","B","C"), 100, TRUE), each = 3),
  age = rnorm(300, 40, 15),
  sex = factor(sample(c("M","F"), 300, TRUE)),
  employed = factor(sample(0:1, 300, TRUE))
)

res <- robsynth_hh(hh_data,
  hhid = "hhid",
  hh_vars = c("hh_income", "hh_region"),
  person_vars = c("age", "sex", "employed"))
} # }
```

# Detect variable types in a data.frame

Classifies each column as `"continuous"` or `"categorical"` based on
class and number of unique values.

## Usage

``` r
detect_var_types(data, max_levels = 5L)
```

## Arguments

- data:

  a data.frame

- max_levels:

  integer; numeric columns with at most this many unique values are
  treated as categorical. Default: 5.

## Value

named character vector with entries `"continuous"` or `"categorical"`,
one per column

## Author

Matthias Templ

## Examples

``` r
data(CrohnD, package = "robustbase")
detect_var_types(CrohnD)
#>            ID        nrAdvE           BMI        height       country 
#>  "continuous"  "continuous"  "continuous"  "continuous" "categorical" 
#>           sex           age        weight         treat 
#> "categorical"  "continuous"  "continuous" "categorical" 
```

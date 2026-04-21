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
detect_var_types(iris)
#>  Sepal.Length   Sepal.Width  Petal.Length   Petal.Width       Species 
#>  "continuous"  "continuous"  "continuous"  "continuous" "categorical" 
```

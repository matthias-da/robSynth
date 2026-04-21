# Strip diagnostics from a robsynth result

Removes the `contWeights` component from a robsynth result to prevent
disclosure of contamination flags. This should be called before
releasing synthetic data.

## Usage

``` r
strip_diagnostics(x)
```

## Arguments

- x:

  an object of class `"robsynth"`

## Value

the same object with `contWeights` set to `NULL`

## Author

Matthias Templ

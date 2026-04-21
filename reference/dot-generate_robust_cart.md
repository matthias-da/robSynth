# Generate from robust CART

Predicts the leaf node of each new observation and returns the stored
leaf median (plus Gaussian noise with MAD scale). Leaf-node assignment
uses
[`partykit::as.party`](https://rdrr.io/pkg/partykit/man/party-coercion.html)
when available and falls back to an rpart `$frame` lookup otherwise, so
the median-leaf logic is preserved rather than round-tripped through
rpart's mean predictions.

## Usage

``` r
.generate_robust_cart(fit, X_new, n)
```

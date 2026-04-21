# Predict leaf node IDs for an rpart fit on new data

Returns integer leaf IDs for each row of `newdata`, aligned to the names
used to store `leaf_medians` at fit time. Requires partykit (already in
Suggests for `ctree`); errors if absent with an informative message.

## Usage

``` r
.predict_rpart_nodes(tree, newdata)
```

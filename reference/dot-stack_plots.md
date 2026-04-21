# Stack two ggplot objects vertically

Simple vertical stacking using
[`gridExtra::arrangeGrob`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html)
if available, otherwise prints sequentially.

## Usage

``` r
.stack_plots(p_top, p_bottom)
```

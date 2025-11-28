# Create a reverse transformation function specifically for date scales

This helper function is exclusive for
[`gg_heatmap()`](https://tscnlab.github.io/LightLogR/reference/gg_heatmap.md),
to get a reversed date sequence.

## Usage

``` r
reverse2_trans()
```

## Source

from https://github.com/tidyverse/ggplot2/issues/4014

## Value

a transformation function

## Examples

``` r
reverse2_trans()
#> Transformer: reverse2 [-Inf, Inf]
```

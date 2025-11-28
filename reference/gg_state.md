# Add states to gg_day() or gg_days() plots

**\[deprecated\]** `gg_state()` has been deprecated. Use
[`gg_states()`](https://tscnlab.github.io/LightLogR/reference/gg_states.md)
instead

## Usage

``` r
gg_state(...)
```

## Arguments

- ...:

  arguments given to
  [`gg_states()`](https://tscnlab.github.io/LightLogR/reference/gg_states.md)

## Value

a ggplot object

## Examples

``` r
sample.data.irregular|> 
dplyr::mutate(movement = dplyr::na_if(movement, 0)) |>  
gg_days() |> gg_state(movement)
#> Warning: `gg_state()` was deprecated in LightLogR 0.10.0.
#> ℹ Please use `gg_states()` instead.
```

# Add a defined number to a numeric and log transform it

Frequently, light exposure data need to be log-transformed. Because
light exposure data frequently also contain many zero-values, adding a
small value avoids losing those observations. Must be applied with care
and reported.

`exp_zero_inflated()` is the reverse function to `log_zero_inflated()`.

## Usage

``` r
log_zero_inflated(x, offset = 0.1, base = 10)

exp_zero_inflated(x, offset = 0.1, base = 10)
```

## Arguments

- x:

  A numeric vector

- offset:

  the amount to add to `x`, by default `0.1`

- base:

  The logarithmic base, by default `10`

## Value

a transformed numeric vector

## References

Johannes Zauner, Carolina Guidolin, Manuel Spitschan (2025) How to deal
with darkness: Modelling and visualization of zero-inflated personal
light exposure data on a logarithmic scale. J Biol Rhythms. 2025
Oct;40(5):480-490. doi: https://doi.org/10.1177/07487304251336624

## Examples

``` r
c(0, 1, 10, 100, 1000, 10000) |> log_zero_inflated()
#> [1] -1.00000000  0.04139269  1.00432137  2.00043408  3.00004343  4.00000434

#For use in a function
sample.data.environment |>
  dplyr::filter(Id == "Participant") |>
  dplyr::group_by(Date = lubridate::wday(Datetime, label = TRUE, week_start = 1)) |>
  dplyr::summarize(
  TAT250 = duration_above_threshold(log_zero_inflated(MEDI),
                                    Datetime,
                                    threshold = log_zero_inflated(250)
                                    )
                   )
#> # A tibble: 6 × 2
#>   Date  TAT250              
#>   <ord> <Duration>          
#> 1 Tue   5810s (~1.61 hours) 
#> 2 Wed   9960s (~2.77 hours) 
#> 3 Thu   16080s (~4.47 hours)
#> 4 Fri   14130s (~3.92 hours)
#> 5 Sat   26930s (~7.48 hours)
#> 6 Sun   25610s (~7.11 hours)
                   

#Calling exp_zero_inflated on data transformed with log_zero_inflated yields to the original result
c(0, 1, 10, 100, 1000, 10000) |> log_zero_inflated() |> exp_zero_inflated()
#> [1]     0     1    10   100  1000 10000
```

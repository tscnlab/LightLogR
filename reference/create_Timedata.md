# create_Timedata

create_Timedata

## Usage

``` r
create_Timedata(...)
```

## Arguments

- ...:

  Input arguments to
  [`add_Time_col()`](https://tscnlab.github.io/LightLogR/reference/add_Time_col.md)

## Value

a `data.frame` object identical to `dataset` but with the added column
of Time-of-Day data, or a `vector` with the Time-of-Day-data

## Examples

``` r
sample.data.environment %>% create_Timedata()
#> Warning: `create_Timedata()` is deprecated as of LightLogR 0.9.1. Please use `add_Time_col()` instead. 
#> # A tibble: 69,120 × 4
#> # Groups:   Id [2]
#>    Id          Datetime             MEDI Time  
#>    <fct>       <dttm>              <dbl> <time>
#>  1 Participant 2023-08-29 00:00:04     0 00'04"
#>  2 Participant 2023-08-29 00:00:14     0 00'14"
#>  3 Participant 2023-08-29 00:00:24     0 00'24"
#>  4 Participant 2023-08-29 00:00:34     0 00'34"
#>  5 Participant 2023-08-29 00:00:44     0 00'44"
#>  6 Participant 2023-08-29 00:00:54     0 00'54"
#>  7 Participant 2023-08-29 00:01:04     0 01'04"
#>  8 Participant 2023-08-29 00:01:14     0 01'14"
#>  9 Participant 2023-08-29 00:01:24     0 01'24"
#> 10 Participant 2023-08-29 00:01:34     0 01'34"
#> # ℹ 69,110 more rows
```

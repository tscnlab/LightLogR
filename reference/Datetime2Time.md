# Convert Datetime columns to Time columns

Convert Datetime columns to Time columns

## Usage

``` r
Datetime2Time(
  dataset,
  cols = dplyr::where(lubridate::is.POSIXct),
  circular = FALSE,
  silent = FALSE
)
```

## Arguments

- dataset:

  A data.frame with POSIXct columns.

- cols:

  The column names to convert. Expects a `symbol`. The default will
  convert all POSIXct columns. If uncertain whether columns exist in the
  dataset, use
  [`dplyr::any_of()`](https://dplyr.tidyverse.org/reference/reexports.html).

- circular:

  Logical on whether the columns should be converted to a circular time
  instead of time stamps. Uses the
  [`circular::circular()`](https://rdrr.io/pkg/circular/man/circular.html)
  class with a `clock24` template for a clean round-trip with
  [`Circular2Time()`](https://tscnlab.github.io/LightLogR/reference/Circular2Time.md).
  Default is `FALSE`.

- silent:

  Logical on whether no message shall be shown if input and output are
  identical. Defaults to `FALSE` (i.e., a message is shown).

## Value

The input dataset with converted POSIXct columns as time (hms) columns.
With the default settings, if no POSIXct column exists, input and output
will be identical.

## Examples

``` r
sample.data.environment |> Datetime2Time()
#> # A tibble: 69,120 × 3
#> # Groups:   Id [2]
#>    Id          Datetime  MEDI
#>    <fct>       <time>   <dbl>
#>  1 Participant 00'04"       0
#>  2 Participant 00'14"       0
#>  3 Participant 00'24"       0
#>  4 Participant 00'34"       0
#>  5 Participant 00'44"       0
#>  6 Participant 00'54"       0
#>  7 Participant 01'04"       0
#>  8 Participant 01'14"       0
#>  9 Participant 01'24"       0
#> 10 Participant 01'34"       0
#> # ℹ 69,110 more rows
#more than one POSIX col
sample.data.environment |>
  dplyr::mutate(Datetime2 = lubridate::POSIXct(1)) |>
  Datetime2Time()
#> # A tibble: 69,120 × 4
#> # Groups:   Id [2]
#>    Id          Datetime  MEDI Datetime2
#>    <fct>       <time>   <dbl> <time>   
#>  1 Participant 00'04"       0 00'00"   
#>  2 Participant 00'14"       0 00'00"   
#>  3 Participant 00'24"       0 00'00"   
#>  4 Participant 00'34"       0 00'00"   
#>  5 Participant 00'44"       0 00'00"   
#>  6 Participant 00'54"       0 00'00"   
#>  7 Participant 01'04"       0 00'00"   
#>  8 Participant 01'14"       0 00'00"   
#>  9 Participant 01'24"       0 00'00"   
#> 10 Participant 01'34"       0 00'00"   
#> # ℹ 69,110 more rows
#only converting one of them
sample.data.environment |>
  dplyr::mutate(Datetime2 = lubridate::POSIXct(1)) |>
  Datetime2Time(Datetime)
#> # A tibble: 69,120 × 4
#> # Groups:   Id [2]
#>    Id          Datetime  MEDI Datetime2          
#>    <fct>       <time>   <dbl> <dttm>             
#>  1 Participant 00'04"       0 1970-01-01 00:00:00
#>  2 Participant 00'14"       0 1970-01-01 00:00:00
#>  3 Participant 00'24"       0 1970-01-01 00:00:00
#>  4 Participant 00'34"       0 1970-01-01 00:00:00
#>  5 Participant 00'44"       0 1970-01-01 00:00:00
#>  6 Participant 00'54"       0 1970-01-01 00:00:00
#>  7 Participant 01'04"       0 1970-01-01 00:00:00
#>  8 Participant 01'14"       0 1970-01-01 00:00:00
#>  9 Participant 01'24"       0 1970-01-01 00:00:00
#> 10 Participant 01'34"       0 1970-01-01 00:00:00
#> # ℹ 69,110 more rows
#if uncertain whether column exists
sample.data.environment |>
  Datetime2Time(dplyr::any_of("Datetime3"))
#> No columns were affected
#> # A tibble: 69,120 × 3
#> # Groups:   Id [2]
#>    Id          Datetime             MEDI
#>    <fct>       <dttm>              <dbl>
#>  1 Participant 2023-08-29 00:00:04     0
#>  2 Participant 2023-08-29 00:00:14     0
#>  3 Participant 2023-08-29 00:00:24     0
#>  4 Participant 2023-08-29 00:00:34     0
#>  5 Participant 2023-08-29 00:00:44     0
#>  6 Participant 2023-08-29 00:00:54     0
#>  7 Participant 2023-08-29 00:01:04     0
#>  8 Participant 2023-08-29 00:01:14     0
#>  9 Participant 2023-08-29 00:01:24     0
#> 10 Participant 2023-08-29 00:01:34     0
#> # ℹ 69,110 more rows
```

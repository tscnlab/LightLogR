# Extract gap episodes from the data

Finds and extracts gap episodes from a dataset. If no variable is
provided, it will look for implicit gaps (gaps in the regular interval),
if a variable is provided, it will look for implicit and explicit gaps
(NA in the variable)

## Usage

``` r
extract_gaps(
  dataset,
  Variable.colname = NULL,
  Datetime.colname = Datetime,
  epoch = "dominant.epoch",
  full.days = TRUE,
  include.implicit.gaps = TRUE
)
```

## Arguments

- dataset:

  A light logger dataset. Needs to be a dataframe.

- Variable.colname:

  Column name of the variable to check for NA values. Expects a symbol
  or NULL (only implicit gaps).

- Datetime.colname:

  The column that contains the datetime. Needs to be a `POSIXct` and
  part of the dataset.

- epoch:

  The epoch to use for the gapless sequence. Can be either a
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
  or a string. If it is a string, it needs to be either
  '"dominant.epoch"' (the default) for a guess based on the data or a
  valid
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

- full.days:

  If `TRUE`, the gapless sequence will include the whole first and last
  day where there is data.

- include.implicit.gaps:

  Logical. Whether to expand the datetime sequence and search for
  implicit gaps, or not. Default is `TRUE`. If no `Variable.colname` is
  provided, this argument will be ignored. **If there are implicit gaps,
  gap calculation can be incorrect whenever there are missing explicit
  gaps flanking implicit gaps!**

## Value

A dataframe containing gap times per grouping variable

## See also

Other regularize:
[`dominant_epoch()`](https://tscnlab.github.io/LightLogR/reference/dominant_epoch.md),
[`gap_finder()`](https://tscnlab.github.io/LightLogR/reference/gap_finder.md),
[`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md),
[`gapless_Datetimes()`](https://tscnlab.github.io/LightLogR/reference/gapless_Datetimes.md),
[`has_gaps()`](https://tscnlab.github.io/LightLogR/reference/has_gaps.md),
[`has_irregulars()`](https://tscnlab.github.io/LightLogR/reference/has_irregulars.md)

## Examples

``` r
#removing some data to create gaps
sample.data.environment |> 
 dplyr::filter(MEDI <= 50000) |> 
 extract_gaps() |> head()
#> # A tibble: 6 × 6
#> # Groups:   Id [1]
#>   Id     gap.id epoch start               end                 duration          
#>   <fct>   <int> <dbl> <dttm>              <dttm>              <Duration>        
#> 1 Envir…      1    30 2023-08-30 11:27:23 2023-08-30 11:28:53 90s (~1.5 minutes)
#> 2 Envir…      2    30 2023-08-30 11:29:53 2023-08-30 11:31:23 90s (~1.5 minutes)
#> 3 Envir…      3    30 2023-08-30 11:33:53 2023-08-30 11:35:23 90s (~1.5 minutes)
#> 4 Envir…      4    30 2023-08-30 11:48:53 2023-08-30 11:49:23 30s               
#> 5 Envir…      5    30 2023-08-30 12:14:23 2023-08-30 12:14:53 30s               
#> 6 Envir…      6    30 2023-08-30 12:15:23 2023-08-30 12:15:53 30s               

#not searching for implicit gaps
sample.data.environment |> 
  dplyr::filter(MEDI <= 50000) |> 
 extract_gaps(MEDI, include.implicit.gaps = FALSE)
#> Warning: There are implicit gaps in the dataset that will not be part of the extracted summary, due to `include.implicit.gaps = FALSE`.
#> No gaps found
#> # A tibble: 0 × 6
#> # Groups:   Id [0]
#> # ℹ 6 variables: Id <fct>, gap.id <int>, epoch <dbl>, start <dttm>, end <dttm>,
#> #   duration <Duration>

#making implicit gaps explicit changes the summary
sample.data.environment |> 
  dplyr::filter(MEDI <= 50000) |> 
  gap_handler()|> 
  extract_gaps(MEDI, include.implicit.gaps = FALSE) |> head()
#> # A tibble: 6 × 6
#> # Groups:   Id [1]
#>   Id     gap.id epoch start               end                 duration          
#>   <fct>   <int> <dbl> <dttm>              <dttm>              <Duration>        
#> 1 Envir…      1    30 2023-08-30 11:27:23 2023-08-30 11:28:53 90s (~1.5 minutes)
#> 2 Envir…      2    30 2023-08-30 11:29:53 2023-08-30 11:31:23 90s (~1.5 minutes)
#> 3 Envir…      3    30 2023-08-30 11:33:53 2023-08-30 11:35:23 90s (~1.5 minutes)
#> 4 Envir…      4    30 2023-08-30 11:48:53 2023-08-30 11:49:23 30s               
#> 5 Envir…      5    30 2023-08-30 12:14:23 2023-08-30 12:14:53 30s               
#> 6 Envir…      6    30 2023-08-30 12:15:23 2023-08-30 12:15:53 30s               
```

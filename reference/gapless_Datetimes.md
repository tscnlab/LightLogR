# Create a gapless sequence of Datetimes

Create a gapless sequence of Datetimes. The Datetimes are determined by
the minimum and maximum Datetime in the dataset and an epoch. The epoch
can either be guessed from the dataset or specified by the user.

## Usage

``` r
gapless_Datetimes(
  dataset,
  Datetime.colname = Datetime,
  epoch = "dominant.epoch",
  full.days = FALSE
)
```

## Arguments

- dataset:

  A light logger dataset. Needs to be a dataframe.

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

## Value

A `tibble` with a gapless sequence of `Datetime` as specified by
`epoch`.

## See also

Other regularize:
[`dominant_epoch()`](https://tscnlab.github.io/LightLogR/reference/dominant_epoch.md),
[`extract_gaps()`](https://tscnlab.github.io/LightLogR/reference/extract_gaps.md),
[`gap_finder()`](https://tscnlab.github.io/LightLogR/reference/gap_finder.md),
[`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md),
[`has_gaps()`](https://tscnlab.github.io/LightLogR/reference/has_gaps.md),
[`has_irregulars()`](https://tscnlab.github.io/LightLogR/reference/has_irregulars.md)

## Examples

``` r
  dataset <-
  tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
                 Datetime = lubridate::as_datetime(1) +
                 lubridate::days(c(0:2, 4, 6, 8))) %>%
                 dplyr::group_by(Id)

  dataset %>% gapless_Datetimes()
#> # A tibble: 6 × 2
#> # Groups:   Id [2]
#>   Id    Datetime           
#>   <chr> <dttm>             
#> 1 A     1970-01-01 00:00:01
#> 2 A     1970-01-02 00:00:01
#> 3 A     1970-01-03 00:00:01
#> 4 B     1970-01-05 00:00:01
#> 5 B     1970-01-07 00:00:01
#> 6 B     1970-01-09 00:00:01
  dataset %>% dplyr::ungroup() %>%  gapless_Datetimes()
#> # A tibble: 5 × 1
#>   Datetime           
#>   <dttm>             
#> 1 1970-01-01 00:00:01
#> 2 1970-01-03 00:00:01
#> 3 1970-01-05 00:00:01
#> 4 1970-01-07 00:00:01
#> 5 1970-01-09 00:00:01
  dataset %>% gapless_Datetimes(epoch = "1 day")
#> # A tibble: 8 × 2
#> # Groups:   Id [2]
#>   Id    Datetime           
#>   <chr> <dttm>             
#> 1 A     1970-01-01 00:00:01
#> 2 A     1970-01-02 00:00:01
#> 3 A     1970-01-03 00:00:01
#> 4 B     1970-01-05 00:00:01
#> 5 B     1970-01-06 00:00:01
#> 6 B     1970-01-07 00:00:01
#> 7 B     1970-01-08 00:00:01
#> 8 B     1970-01-09 00:00:01
```

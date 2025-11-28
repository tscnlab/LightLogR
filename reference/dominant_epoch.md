# Determine the dominant epoch/interval of a dataset

Calculate the dominant epoch/interval of a dataset. The dominant
epoch/interval is the epoch/interval that is most frequent in the
dataset. The calculation is done per group, so that you might get
multiple variables. If two or more epochs/intervals are equally
frequent, the first one (shortest one) is chosen.

## Usage

``` r
dominant_epoch(dataset, Datetime.colname = Datetime)
```

## Arguments

- dataset:

  A light logger dataset. Needs to be a dataframe.

- Datetime.colname:

  The column that contains the datetime. Needs to be a `POSIXct` and
  part of the dataset.

## Value

A `tibble` with one row per group and a column with the `dominant.epoch`
as a
[`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html).
Also a column with the `group.indices`, which is helpful for referencing
the `dominant.epoch` across dataframes of equal grouping.

## See also

Other regularize:
[`extract_gaps()`](https://tscnlab.github.io/LightLogR/reference/extract_gaps.md),
[`gap_finder()`](https://tscnlab.github.io/LightLogR/reference/gap_finder.md),
[`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md),
[`gapless_Datetimes()`](https://tscnlab.github.io/LightLogR/reference/gapless_Datetimes.md),
[`has_gaps()`](https://tscnlab.github.io/LightLogR/reference/has_gaps.md),
[`has_irregulars()`](https://tscnlab.github.io/LightLogR/reference/has_irregulars.md)

## Examples

``` r
dataset <-
tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
              Datetime = lubridate::as_datetime(1) +
                         lubridate::days(c(0:2, 4, 6, 8)))
dataset
#> # A tibble: 6 × 2
#>   Id    Datetime           
#>   <chr> <dttm>             
#> 1 A     1970-01-01 00:00:01
#> 2 A     1970-01-02 00:00:01
#> 3 A     1970-01-03 00:00:01
#> 4 B     1970-01-05 00:00:01
#> 5 B     1970-01-07 00:00:01
#> 6 B     1970-01-09 00:00:01
#get the dominant epoch by group
dataset %>%
dplyr::group_by(Id) %>%
dominant_epoch()
#> # A tibble: 2 × 3
#>   Id    dominant.epoch    group.indices
#>   <chr> <Duration>                <int>
#> 1 A     86400s (~1 days)              1
#> 2 B     172800s (~2 days)             2

#get the dominant epoch of the whole dataset
dataset %>%
dominant_epoch()              
#> # A tibble: 1 × 2
#>   dominant.epoch    group.indices
#>   <Duration>                <int>
#> 1 172800s (~2 days)             1
```

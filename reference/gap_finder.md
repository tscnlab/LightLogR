# Check for and output gaps in a dataset

Quickly check for implicit missing `Datetime` data. Outputs a message
with a short summary, and can optionally return the gaps as a `tibble`.
Uses
[`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md)
internally.

## Usage

``` r
gap_finder(
  dataset,
  Datetime.colname = Datetime,
  epoch = "dominant.epoch",
  gap.data = FALSE,
  silent = FALSE,
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

- gap.data:

  Logical. If `TRUE`, returns a `tibble` of the gaps in the dataset.
  Default is `FALSE`.

- silent:

  Logical. If `TRUE`, suppresses the message with the summary of the
  gaps in the dataset. Default is `FALSE`. Only used for unit tests.

- full.days:

  If `TRUE`, the gapless sequence will include the whole first and last
  day where there is data.

## Value

Prints message with a short summary of the gaps in the dataset. If
`gap.data = TRUE`, returns a `tibble` of the gaps in the dataset.

## Details

The `gap_finder()` function is a wrapper around
[`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md)
with the `behavior` argument set to `"gaps"`. The main difference is
that `gap_finder()` returns a message with a short summary of the gaps
in the dataset, and that the `tibble` with the gaps contains a column
`gap.id` that indicates the gap number, which is useful to determine,
e.g., the consecutive number of gaps between measurement data.

## See also

Other regularize:
[`dominant_epoch()`](https://tscnlab.github.io/LightLogR/reference/dominant_epoch.md),
[`extract_gaps()`](https://tscnlab.github.io/LightLogR/reference/extract_gaps.md),
[`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md),
[`gapless_Datetimes()`](https://tscnlab.github.io/LightLogR/reference/gapless_Datetimes.md),
[`has_gaps()`](https://tscnlab.github.io/LightLogR/reference/has_gaps.md),
[`has_irregulars()`](https://tscnlab.github.io/LightLogR/reference/has_irregulars.md)

## Examples

``` r
dataset <-
tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
              Datetime = lubridate::as_datetime(1) +
                         lubridate::days(c(0:2, 4, 6, 8)) +
                         lubridate::hours(c(0,12,rep(0,4)))) %>%
dplyr::group_by(Id)
dataset
#> # A tibble: 6 × 2
#> # Groups:   Id [2]
#>   Id    Datetime           
#>   <chr> <dttm>             
#> 1 A     1970-01-01 00:00:01
#> 2 A     1970-01-02 12:00:01
#> 3 A     1970-01-03 00:00:01
#> 4 B     1970-01-05 00:00:01
#> 5 B     1970-01-07 00:00:01
#> 6 B     1970-01-09 00:00:01

#look for gaps assuming the epoch is the dominant epoch of each group
gap_finder(dataset)
#> Found 2 gaps. 6 Datetimes fall into the regular sequence.

#return the gaps as a tibble
gap_finder(dataset, gap.data = TRUE)
#> Found 2 gaps. 6 Datetimes fall into the regular sequence.
#> # A tibble: 2 × 3
#> # Groups:   Id [1]
#>   gap.id Datetime            Id   
#>    <int> <dttm>              <chr>
#> 1      1 1970-01-01 12:00:01 A    
#> 2      1 1970-01-02 00:00:01 A    

#assuming the epoch is 1 day, we have different gaps, and the datapoint at noon is now `irregular`
gap_finder(dataset, epoch = "1 day")
#> Found 3 gaps. 5 Datetimes fall into the regular sequence.
```

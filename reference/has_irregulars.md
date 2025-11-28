# Does a dataset have irregular data

Returns `TRUE` if there are irregular data in the dataset and `FALSE` if
not. Irregular data can make sense if two datasets within a single group
are shifted to one another, e.g., if it contains data from two separate
recording sessions. The second session will be unlikely to have started
at the exact interval timing of the first session. While this is not
problematic in itself, it is still recommended to rectify the Datetimes
to a common timestamp if time precision permits it, e.g., through
[`aggregate_Datetime()`](https://tscnlab.github.io/LightLogR/reference/aggregate_Datetime.md)
or
[`cut_Datetime()`](https://tscnlab.github.io/LightLogR/reference/cut_Datetime.md).

## Usage

``` r
has_irregulars(dataset, Datetime.colname = Datetime, epoch = "dominant.epoch")
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

## Value

logical

## See also

Other regularize:
[`dominant_epoch()`](https://tscnlab.github.io/LightLogR/reference/dominant_epoch.md),
[`extract_gaps()`](https://tscnlab.github.io/LightLogR/reference/extract_gaps.md),
[`gap_finder()`](https://tscnlab.github.io/LightLogR/reference/gap_finder.md),
[`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md),
[`gapless_Datetimes()`](https://tscnlab.github.io/LightLogR/reference/gapless_Datetimes.md),
[`has_gaps()`](https://tscnlab.github.io/LightLogR/reference/has_gaps.md)

## Examples

``` r
#the sample dataset does not have any irregular data
sample.data.environment |> has_irregulars()
#> [1] FALSE

#even removing some data does not make it irregular, as all the Datetimes
#still fall in the regular interval
sample.data.environment |> dplyr::filter(MEDI <= 50000) |> has_irregulars()
#> [1] FALSE

#shifting some of the data will create irregular data
sample.data.environment |> 
  dplyr::mutate(
   Datetime = dplyr::if_else(
     sample(c(TRUE, FALSE), dplyr::n(), replace = TRUE), Datetime, Datetime + 1
     )
   ) |> 
   has_irregulars()
#> [1] TRUE
```

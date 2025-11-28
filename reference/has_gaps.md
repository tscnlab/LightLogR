# Does a dataset have implicit gaps

Returns `TRUE` if there are implicit gaps in the dataset and `FALSE` if
it is gapless. Gaps can make sense depending on the grouping structure,
but the general sequence of Datetimes within a dataset should always be
gapless.

## Usage

``` r
has_gaps(dataset, Datetime.colname = Datetime, epoch = "dominant.epoch")
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
[`has_irregulars()`](https://tscnlab.github.io/LightLogR/reference/has_irregulars.md)

## Examples

``` r
#The sample dataset does not have gaps
sample.data.environment |> has_gaps()
#> [1] FALSE
#removing some of the data creates gaps
sample.data.environment |> dplyr::filter(MEDI <= 50000) |> has_gaps()
#> [1] TRUE

#having a grouped dataframe where the groups span multiple unconnected parts 
#is considered a gap, which can be relevant, e.g., when searching for clusters
sample.data.environment |>
  add_photoperiod(c(47.1, 10)) |>
  dplyr::group_by(photoperiod.state) |>
  has_gaps()
#> [1] TRUE

 #to avoid this, use `number_states()` for grouping
 sample.data.environment |>
  add_photoperiod(c(48.52, 9.06)) |>
  number_states(photoperiod.state) |>
  dplyr::group_by(photoperiod.state.count, .add = TRUE) |>
  has_gaps()
#> [1] FALSE
```

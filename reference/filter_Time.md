# Filter Times in a dataset.

Filter Times in a dataset.

## Usage

``` r
filter_Time(
  dataset,
  Datetime.colname = Datetime,
  start = NULL,
  end = NULL,
  length = NULL
)
```

## Arguments

- dataset:

  A light logger dataset. Expects a `dataframe`. If not imported by
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md),
  take care to choose a sensible variable for the `Datetime.colname`.

- Datetime.colname:

  column name that contains the datetime. Defaults to `"Datetime"` which
  is automatically correct for data imported with
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md).
  Expects a `symbol`. Needs to be part of the `dataset`. Must be of type
  `POSIXct`.

- start, end, length:

  a `character` scalar in the form of `"hh-mm-ss"` giving the respective
  start, end, or length for the filtered dataframe. The input can also
  come from a `POSIXct` datetime, where only the time component will be
  used.

  - If one or both of start/end are not provided, the times will be
    taken from the respective extreme values of the `dataset`.

  - If `length` is provided and one of start/end is not, the other will
    be calculated based on the given value.

  - If `length` is provided and both of start/end are not, the time from
    the respective start is taken.

## Value

a `data.frame` object identical to `dataset` but with only the specified
Times.

## See also

Other filter:
[`filter_Datetime()`](https://tscnlab.github.io/LightLogR/reference/filter_Datetime.md)

## Examples

``` r
sample.data.environment %>%
filter_Time(start = "4:00:34", length = "12:00:00") %>%
dplyr::pull(Time) %>% range() %>% hms::as_hms()
#> 04:00:34
#> 16:00:34
```

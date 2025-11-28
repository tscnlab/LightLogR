# Create a Time-of-Day column in the dataset

Create a Time-of-Day column in the dataset

## Usage

``` r
add_Time_col(
  dataset,
  Datetime.colname = Datetime,
  Time.colname = Time,
  output.dataset = TRUE
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

- Time.colname:

  Name of the newly created column. Expects a `symbol`. The
  default(`Time`) works well with other functions in
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md).
  Will overwrite existing columns of identical name.

- output.dataset:

  should the output be a `data.frame` (Default `TRUE`) or a vector with
  `hms` (`FALSE`) times? Expects a `logical` scalar.

## Value

a `data.frame` object identical to `dataset` but with the added column
of Time-of-Day data, or a `vector` with the Time-of-Day-data

## Examples

``` r
sample.data.environment %>% add_Time_col()
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

# Create a Date column in the dataset

Create a Date column in the dataset

## Usage

``` r
add_Date_col(
  dataset,
  Date.colname = Date,
  group.by = FALSE,
  as.wday = FALSE,
  as.count = FALSE,
  Datetime.colname = Datetime
)
```

## Arguments

- dataset:

  A light logger dataset. Expects a `dataframe`. If not imported by
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md),
  take care to choose a sensible variable for the `Datetime.colname`.

- Date.colname:

  Name of the newly created column. Expects a `symbol`. The
  default(`Date`) works well with other functions in
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md).
  Will overwrite existing columns of identical name.

- group.by:

  Logical whether the output should be (additionally) grouped by the new
  column

- as.wday:

  Logical of whether the added column should calculate day of the week
  instead of date. If `TRUE` will create a factor with weekday
  abbreviations, where the week starts with `Mon`. Will be ignored if
  `as.count = TRUE`.

- as.count:

  Logical of whether the added column should give a day count from the
  starting day of the group.

- Datetime.colname:

  column name that contains the datetime. Defaults to `"Datetime"` which
  is automatically correct for data imported with
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md).
  Expects a `symbol`. Needs to be part of the `dataset`. Must be of type
  `POSIXct`.

## Value

a `data.frame` object identical to `dataset` but with the added column
of Date data

## Examples

``` r
sample.data.environment %>% add_Date_col()
#> # A tibble: 69,120 × 4
#> # Groups:   Id [2]
#>    Id          Datetime             MEDI Date      
#>    <fct>       <dttm>              <dbl> <date>    
#>  1 Participant 2023-08-29 00:00:04     0 2023-08-29
#>  2 Participant 2023-08-29 00:00:14     0 2023-08-29
#>  3 Participant 2023-08-29 00:00:24     0 2023-08-29
#>  4 Participant 2023-08-29 00:00:34     0 2023-08-29
#>  5 Participant 2023-08-29 00:00:44     0 2023-08-29
#>  6 Participant 2023-08-29 00:00:54     0 2023-08-29
#>  7 Participant 2023-08-29 00:01:04     0 2023-08-29
#>  8 Participant 2023-08-29 00:01:14     0 2023-08-29
#>  9 Participant 2023-08-29 00:01:24     0 2023-08-29
#> 10 Participant 2023-08-29 00:01:34     0 2023-08-29
#> # ℹ 69,110 more rows
#days of the week
sample.data.environment %>% 
  add_Date_col(as.wday = TRUE, group.by = TRUE) |> 
  summarize_numeric(remove = c("Datetime"))
#> # A tibble: 12 × 4
#> # Groups:   Id [2]
#>    Id          Date  mean_MEDI episodes
#>    <fct>       <ord>     <dbl>    <int>
#>  1 Environment Tue      6362.      2880
#>  2 Environment Wed     13966.      2880
#>  3 Environment Thu     17513.      2880
#>  4 Environment Fri     17180.      2880
#>  5 Environment Sat     23869.      2880
#>  6 Environment Sun     18086.      2880
#>  7 Participant Tue        91.5     8640
#>  8 Participant Wed        93.8     8640
#>  9 Participant Thu       188.      8640
#> 10 Participant Fri       748.      8640
#> 11 Participant Sat      1641.      8640
#> 12 Participant Sun      1712.      8640
```

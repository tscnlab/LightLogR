# Create Datetime bins for visualization and calculation

`cut_Datetime` is a wrapper around
[`lubridate::round_date()`](https://lubridate.tidyverse.org/reference/round_date.html)
(and friends) combined with
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
to create a new column in a light logger dataset with a specified
binsize. This can be `"3 hours"`, `"15 secs"`, or `"0.5 days"`. It is a
useful step between a dataset and a visualization or summary step.

## Usage

``` r
cut_Datetime(
  dataset,
  unit = "3 hours",
  type = c("round", "floor", "ceiling"),
  Datetime.colname = Datetime,
  New.colname = Datetime.rounded,
  group_by = FALSE,
  ...
)
```

## Arguments

- dataset:

  A light logger dataset. Expects a `dataframe`. If not imported by
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md),
  take care to choose a sensible variable for the `Datetime.colname`.

- unit:

  Unit of binning. See
  [`lubridate::round_date()`](https://lubridate.tidyverse.org/reference/round_date.html)
  for examples. The default is `"3 hours"`.

- type:

  One of `"round"`(the default), `"ceiling"` or `"floor"`. Setting
  chooses the relevant function from lubridate.

- Datetime.colname:

  column name that contains the datetime. Defaults to `"Datetime"` which
  is automatically correct for data imported with
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md).
  Expects a `symbol`. Needs to be part of the `dataset`. Must be of type
  `POSIXct`.

- New.colname:

  Column name for the added column in the `dataset`.

- group_by:

  Should the data be grouped by the new column? Defaults to `FALSE`

- ...:

  Parameter handed over to
  [`lubridate::round_date()`](https://lubridate.tidyverse.org/reference/round_date.html)
  and siblings

## Value

a `data.frame` object identical to `dataset` but with the added column
of binned datetimes.

## Examples

``` r
#compare Datetime and Datetime.rounded
sample.data.environment %>%
  cut_Datetime() %>%
  dplyr::slice_sample(n = 5)
#> # A tibble: 10 × 4
#> # Groups:   Id [2]
#>    Id          Datetime            Datetime.rounded        MEDI
#>    <fct>       <dttm>              <dttm>                 <dbl>
#>  1 Environment 2023-08-29 23:39:38 2023-08-30 00:00:00     0   
#>  2 Environment 2023-09-02 14:42:08 2023-09-02 15:00:00 76316.  
#>  3 Environment 2023-08-30 13:34:08 2023-08-30 15:00:00 67072.  
#>  4 Environment 2023-08-30 18:06:08 2023-08-30 18:00:00 16914.  
#>  5 Environment 2023-08-30 14:45:08 2023-08-30 15:00:00 71971.  
#>  6 Participant 2023-08-30 20:08:14 2023-08-30 21:00:00     3.17
#>  7 Participant 2023-08-30 06:39:34 2023-08-30 06:00:00     0   
#>  8 Participant 2023-08-30 05:58:44 2023-08-30 06:00:00     0   
#>  9 Participant 2023-08-31 18:53:44 2023-08-31 18:00:00  1777.  
#> 10 Participant 2023-09-02 23:39:24 2023-09-03 00:00:00     0   
```

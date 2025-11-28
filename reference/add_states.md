# Add states to a dataset based on groups and start/end times

`add_states()` brings states to a time series dataset. It uses the
`States.dataset` to add states to the `dataset`. The `States.dataset`
must at least contain the same variables as the `dataset` grouping, as
well as a start and end time (or an interval column). Beware if both
datasets operate on different time zones and consider to set
`force.tz = TRUE`.

## Usage

``` r
add_states(
  dataset,
  States.dataset,
  Datetime.colname = Datetime,
  start.colname = start,
  end.colname = end,
  force.tz = FALSE,
  leave.out = c("duration", "epoch")
)
```

## Arguments

- dataset:

  A light logger dataset. Needs to be a dataframe.

- States.dataset:

  A light logger dataset. Needs to be a dataframe. This dataset must
  contain the same variables as the `dataset` grouping, as well as a
  start and end time. Any other column, that is not in `leave.out` will
  be added to the dataset.

- Datetime.colname:

  The column that contains the datetime. Needs to be a `POSIXct` and
  part of the dataset.

- start.colname, end.colname:

  The columns that contain the start and end time. Need to be `POSIXct`
  and part of the `States.dataset`. Can also be an `Interval` column, in
  which case the start and end times will be extracted via `lubridate`
  functions.

- force.tz:

  If `TRUE`, the start and end times of the `States.dataset` will be
  forced to the same time zone as the `dataset` using
  [`lubridate::force_tz()`](https://lubridate.tidyverse.org/reference/force_tz.html).
  If `FALSE` (default), the start and end times of the `States.dataset`
  will be used as is.

- leave.out:

  A character vector of columns that should not be carried over to the
  `dataset`

## Value

a modified `dataset` with the states added. The states are added as new
columns to the `dataset`. The columns are named after the columns in the
`States.dataset`, except for the start and end times, which are removed.

## Details

Beware if columns in the `dataset` and `States.dataset` have the same
name (other then grouping variables). The underlying function,
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
will mark the columns in the `dataset` with a suffix `.x`, and in the
`States.dataset` with a suffix `.y`.

Also be careful if grouping variables have the same name, but a
different class - usually, this will result in an error that can be
fixed by making sure the classes are identical. This is especially an
issue with labelled variables (e.g., a labelled `Id` factor-variable in
the main dataset, and a factor variable `Id` in the state dataset) - in
those cases, either the unlabelled variable has to be labelled as well,
or the other one unlabelled.

## Examples

``` r
states <-
sample.data.environment |>
  filter_Date(length = "1 day") |>
  extract_states(Daylight, MEDI > 1000)

states |> head(2)
#> # A tibble: 2 × 7
#> # Groups:   Id, Daylight [1]
#>   Id          Daylight state.count epoch start               end                
#>   <fct>       <lgl>    <chr>       <dbl> <dttm>              <dttm>             
#> 1 Environment FALSE    FALSE 1        30 2023-08-28 23:59:53 2023-08-29 06:57:53
#> 2 Environment FALSE    FALSE 2        30 2023-08-29 19:45:23 2023-08-29 23:59:53
#> # ℹ 1 more variable: duration <Duration>

#add states to a dataset and plot them - as we only looked for states on the
# first day (see above), only the first day will show up in the plot
sample.data.environment |>
 filter_Date(length = "2 day") |>
 add_states(states) |>
 gg_days() |>
 gg_states(Daylight)
```

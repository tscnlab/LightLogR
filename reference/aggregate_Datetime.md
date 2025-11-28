# Aggregate Datetime data

Condenses a `dataset` by aggregating the data to a given (shorter)
interval `unit`. `aggregate_Datetime()` is opinionated in the sense that
it sets default handlers for each data type of `numeric`, `character`,
`logical`, `factor`, `duration`, `time`, and `datetime`. These can be
overwritten by the user. Columns that do not fall into one of these
categories need to be handled individually by the user (`...` argument)
or will be removed during aggregation. If no unit is specified the data
will simply be aggregated to the most common interval
(`dominant.epoch`), which is most often not an aggregation but a
rounding.)

## Usage

``` r
aggregate_Datetime(
  dataset,
  unit = "dominant.epoch",
  Datetime.colname = Datetime,
  type = c("round", "floor", "ceiling"),
  numeric.handler = mean,
  character.handler = function(x) names(which.max(table(x, useNA = "ifany"))),
  logical.handler = function(x) mean(x) >= 0.5,
  factor.handler = function(x) factor(names(which.max(table(x, useNA = "ifany")))),
  datetime.handler = mean,
  duration.handler = function(x) lubridate::duration(mean(x)),
  time.handler = function(x) hms::as_hms(mean(x)),
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
  for examples. The default is `"dominant.epoch"`, which means
  everything will be aggregated to the most common interval. This is
  especially useful for slightly irregular data, but can be
  computationally expensive. `"none"` will not aggregate the data at
  all.

- Datetime.colname:

  column name that contains the datetime. Defaults to `"Datetime"` which
  is automatically correct for data imported with
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md).
  Expects a `symbol`. Needs to be part of the `dataset`. Must be of type
  `POSIXct`.

- type:

  One of `"round"`(the default), `"ceiling"` or `"floor"`. Setting
  chooses the relevant function from lubridate.

- numeric.handler, character.handler, logical.handler, factor.handler,
  datetime.handler, duration.handler, time.handler:

  functions that handle the respective data types. The default handlers
  calculate the `mean` or `median` for `numeric`, `POSIXct`, `duration`,
  and `hms`, and the `mode` for `character`, `factor` and `logical`
  types.

- ...:

  arguments given over to
  [`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)
  to handle columns that do not fall into one of the categories above.
  Be careful with partial matching of argument names. E.g., creating a
  new column `n`, without having changed the default for
  `numeric.handler` will match `n` to that argument. You can avoid this
  be either explicitly supplying the `numeric.handler` argument (using
  the default again), or you change the variable name, e.g, to `n.`,
  which avoids the partial matching.

## Value

A `tibble` with aggregated `Datetime` data. Usually the number of rows
will be smaller than the input `dataset`. If the handler arguments
capture all column types, the number of columns will be the same as in
the input `dataset`.

## Details

Summary values for type `POSIXct` are calculated as the mean, which can
be nonsensical at times (e.g., the mean of Day1 18:00 and Day2 18:00, is
Day2 6:00, which can be the desired result, but if the focus is on time,
rather then on datetime, it is recommended that values are converted to
times via
[`hms::as_hms()`](https://hms.tidyverse.org/reference/hms.html) before
applying the function (the mean of 18:00 and 18:00 is still 18:00, not
6:00).

## Examples

``` r
#dominant epoch without aggregation
sample.data.environment %>%
 dominant_epoch()
#> # A tibble: 2 × 3
#>   Id          dominant.epoch group.indices
#>   <fct>       <Duration>             <int>
#> 1 Environment 30s                        1
#> 2 Participant 10s                        2

#dominant epoch with 5 minute aggregation
sample.data.environment %>%
 aggregate_Datetime(unit = "5 mins") %>%
 dominant_epoch()
#> # A tibble: 2 × 3
#>   Id          dominant.epoch    group.indices
#>   <fct>       <Duration>                <int>
#> 1 Environment 300s (~5 minutes)             1
#> 2 Participant 300s (~5 minutes)             2

#dominant epoch with 1 day aggregation
sample.data.environment %>%
 aggregate_Datetime(unit = "1 day") %>%
 dominant_epoch()
#> # A tibble: 2 × 3
#>   Id          dominant.epoch   group.indices
#>   <fct>       <Duration>               <int>
#> 1 Environment 86400s (~1 days)             1
#> 2 Participant 86400s (~1 days)             2
```

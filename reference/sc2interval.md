# Statechange (sc) Timestamps to Intervals

Takes an input of `datetimes` and `Statechanges` and creates a column
with `Intervals`. If `full = TRUE`, it will also create intervals for
the day prior to the first state change and after the last. If
`output.dataset = FALSE` it will give a named vector, otherwise a
`tibble`. The `state change` info requires a description or name of the
state (like `"sleep"` or `"wake"`, or `"wear"`) that goes into effect at
the given `Datetime`. Works for grouped data so that it does not mix up
intervals between participants. Missing data should be explicit if at
all possible. Also, the maximum allowed length of an interval can be
set, so that implicit missing timestamps after a set period of times can
be enforced.

## Usage

``` r
sc2interval(
  dataset,
  Datetime.colname = Datetime,
  Statechange.colname = State,
  State.colname = State,
  Interval.colname = Interval,
  full = TRUE,
  starting.state = NA,
  output.dataset = TRUE,
  Datetime.keep = FALSE,
  length.restriction = 60 * 60 * 24
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

- Statechange.colname, Interval.colname, State.colname:

  Column names that do contain the name/description of the
  `state change` and that will contain the `Interval` and `State` (which
  are also the default). Expects a `symbol`. The `Statechange` column
  needs do be part of the `dataset`.

- full, starting.state:

  These arguments handle the state on the first day before the first
  state change and after the last state change on the last day. If
  `full = TRUE`(the default, expects a `logical`), it will create an
  interval on the first day from 00:00:00 up until the state change.
  This interval will be given the state specified in `starting.state`,
  which is `NA` by default, but can be any `character` scalar. It will
  further extend the interval for the last state change until the end of
  the last given day (more specifically until 00:00:00 the next day).

- output.dataset:

  should the output be a `data.frame` (Default `TRUE`) or a vector with
  `hms` (`FALSE`) times? Expects a `logical` scalar.

- Datetime.keep:

  If `TRUE`, the original `Datetime` column will be kept.

- length.restriction:

  If the length between intervals is too great, the interval state can
  be set to `NA`, which effectively produces a gap in the data. This
  makes sense when intervals are implausibly wrong (e.g. someone slept
  for 50 hours), because when this data is combined with light logger
  data, e.g., through
  [`interval2state()`](https://tscnlab.github.io/LightLogR/reference/interval2state.md),
  metrics and visualizations will remove the interval.

## Value

One of

- a `data.frame` object identical to `dataset` but with the interval
  instead of the datetime. The original `Statechange` column now
  indicates the `State` during the `Interval`.

- a named `vector` with the intervals, where the names are the states

## Examples

``` r
library(tibble)
library(lubridate)
library(dplyr)
sample <- tibble::tibble(Datetime = c("2023-08-29 6:00:00",
                                      "2023-08-29 23:00:00",
                                      "2023-08-30 6:00:00",
                                      "2023-08-30 22:00:00",
                                      "2023-08-31 6:30:00",
                                      "2023-09-01 1:00:00"),
                         State = rep(c("wake", "sleep"), 3),
                         Id = "Participant")
#intervals from sample
sc2interval(sample)
#> # A tibble: 7 × 3
#>   State Id          Interval                                        
#>   <chr> <chr>       <Interval>                                      
#> 1 NA    NA          2023-08-29 00:00:00 UTC--2023-08-29 06:00:00 UTC
#> 2 wake  Participant 2023-08-29 06:00:00 UTC--2023-08-29 23:00:00 UTC
#> 3 sleep Participant 2023-08-29 23:00:00 UTC--2023-08-30 06:00:00 UTC
#> 4 wake  Participant 2023-08-30 06:00:00 UTC--2023-08-30 22:00:00 UTC
#> 5 sleep Participant 2023-08-30 22:00:00 UTC--2023-08-31 06:30:00 UTC
#> 6 wake  Participant 2023-08-31 06:30:00 UTC--2023-09-01 01:00:00 UTC
#> 7 sleep Participant 2023-09-01 01:00:00 UTC--2023-09-02 00:00:00 UTC

#compare sample (y) and intervals (x)
sc2interval(sample) %>%
 mutate(Datetime = int_start(Interval)) %>%
 dplyr::left_join(sample, by = c("Id", "State"),
                  relationship = "many-to-many") %>%
 head()
#> # A tibble: 6 × 5
#>   State Id          Interval                                        
#>   <chr> <chr>       <Interval>                                      
#> 1 NA    NA          2023-08-29 00:00:00 UTC--2023-08-29 06:00:00 UTC
#> 2 wake  Participant 2023-08-29 06:00:00 UTC--2023-08-29 23:00:00 UTC
#> 3 wake  Participant 2023-08-29 06:00:00 UTC--2023-08-29 23:00:00 UTC
#> 4 wake  Participant 2023-08-29 06:00:00 UTC--2023-08-29 23:00:00 UTC
#> 5 sleep Participant 2023-08-29 23:00:00 UTC--2023-08-30 06:00:00 UTC
#> 6 sleep Participant 2023-08-29 23:00:00 UTC--2023-08-30 06:00:00 UTC
#> # ℹ 2 more variables: Datetime.x <dttm>, Datetime.y <chr>
```

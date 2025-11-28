# Find and extract clusters from a dataset

`extract_clusters()` searches for and summarizes clusters where data
meets a certain condition. Clusters have a specified duration and can be
interrupted while still counting as one cluster. The variable can either
be a column in the dataset or an expression that gets evaluated in a
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
call.

Cluster start and end times are shifted by half of the epoch each. E.g.,
a state lasting for 4 measurement points will have a duration of 4
measurement intervals, and a state only occuring once, of one interval.
This deviates from simply using the time difference between the first
and last occurance, which would be one epoch shorter (e.g., the start
and end points for a state lasting a single point is identical, i.e.,
zero duration)

Groups will not be dropped, meaning that summaries based on the clusters
will account for groups without clusters.

**For correct cluster identification, there can be no gaps in the
data!** Gaps can inadvertently be introduced to a gapless dataset
through grouping. E.g., when grouping by photoperiod (day/night) within
a participant, this introduces gaps between the individual days and
nights that together form the group. To avoid this, either group by
individual days and nights (e.g., by using
[`number_states()`](https://tscnlab.github.io/LightLogR/reference/number_states.md)
before grouping), which will make sure a cluster cannot extend beyond
any grouping. Alternatively, you can set `handle.gaps = TRUE` (at
computational cost).

`add_clusters()` identifies clusters and adds them back into the dataset
through a rolling join. This is a convenience function built on
`extract_clusters()`.

## Usage

``` r
extract_clusters(
  data,
  Variable,
  Datetime.colname = Datetime,
  cluster.duration = "30 mins",
  duration.type = c("min", "max"),
  interruption.duration = 0,
  interruption.type = c("max", "min"),
  cluster.colname = state.count,
  return.only.clusters = TRUE,
  drop.empty.groups = TRUE,
  handle.gaps = FALSE,
  add.label = FALSE
)

add_clusters(
  data,
  Variable,
  Datetime.colname = Datetime,
  cluster.duration = "30 mins",
  duration.type = c("min", "max"),
  interruption.duration = 0,
  interruption.type = c("max", "min"),
  cluster.colname = state,
  handle.gaps = FALSE
)
```

## Arguments

- data:

  A light logger dataset. Expects a dataframe.

- Variable:

  The variable or condition to be evaluated for clustering. Can be a
  column name or an expression.

- Datetime.colname:

  Column name that contains the datetime. Defaults to "Datetime" which
  is automatically correct for data imported with LightLogR. Expects a
  symbol.

- cluster.duration:

  The minimum or maximum duration of a cluster. Defaults to 30 minutes.
  Expects a lubridate duration object (or a numeric in seconds).

- duration.type:

  Type of the duration requirement for clusters. Either "min" (minimum
  duration) or "max" (maximum duration). Defaults to "min".

- interruption.duration:

  The duration of allowed interruptions within a cluster. Defaults to 0
  (no interruptions allowed).

- interruption.type:

  Type of the interruption duration. Either "max" (maximum interruption)
  or "min" (minimum interruption). Defaults to "max".

- cluster.colname:

  Name of the column to use for the cluster identification. Defaults to
  "state.count". Expects a symbol.

- return.only.clusters:

  Whether to return only the identified clusters (TRUE) or also include
  non-clusters (FALSE). Defaults to TRUE.

- drop.empty.groups:

  Logical. Should empty groups be dropped? Only works if `.drop = FALSE`
  has not been used with the current grouping prior to calling the
  function. Default to `TRUE`. If set to `FALSE` can lead to an error if
  factors are present in the grouping that have more levels than actual
  data. Can, however, be useful and necessary when summarizing the
  groups further, e.g. through
  [`summarize_numeric()`](https://tscnlab.github.io/LightLogR/reference/summarize_numeric.md) -
  having an empty group present is important when averaging numbers.

- handle.gaps:

  Logical whether the data shall be treated with
  [`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md).
  Is set to `FALSE` by default, due to computational costs.

- add.label:

  Logical. Option to add a label to the output containing the condition.
  E.g., `MEDI>500|d>=30min|i<=5min` for clusters of melanopic EDI larger
  than 500, at least 30 minutes long (`d`), allowing interruptions of up
  to 5 minutes at a time (`i`).

## Value

For `extract_clusters()` a dataframe containing the identified clusters
or all time periods, depending on `return.only.clusters`.

For `add_clusters()` a dataframe containing the original data with an
additional column for cluster identification.

## Examples

``` r
dataset <-
sample.data.environment |>
dplyr::filter(Id == "Participant") |>
filter_Date(length = "1 day")

# Extract clusters with minimum duration of 1 hour and interruptions of up to 5 minutes
dataset |>
 extract_clusters(
  MEDI > 250,
  cluster.duration = "1 hour",
  interruption.duration = "5 mins"
)
#> # A tibble: 1 × 6
#> # Groups:   Id [1]
#>   Id          state.count start               end                 epoch     
#>   <fct>       <chr>       <dttm>              <dttm>              <Duration>
#> 1 Participant 1           2023-08-29 16:34:49 2023-08-29 17:38:09 10s       
#> # ℹ 1 more variable: duration <Duration>

# Add clusters to a dataset where lux values are above 20 for at least 30 minutes
dataset_with_clusters <- 
dataset %>% add_clusters(MEDI > 20)

#peak into the dataset
dataset_with_clusters[4500:4505,]
#> # A tibble: 6 × 4
#> # Groups:   Id [1]
#>   Id          Datetime             MEDI state
#>   <fct>       <dttm>              <dbl> <chr>
#> 1 Participant 2023-08-29 12:29:54  40.6 2    
#> 2 Participant 2023-08-29 12:30:04  48.3 2    
#> 3 Participant 2023-08-29 12:30:14  46.5 2    
#> 4 Participant 2023-08-29 12:30:24  48.5 2    
#> 5 Participant 2023-08-29 12:30:34  42.3 2    
#> 6 Participant 2023-08-29 12:30:44  44.6 2    
```

# Summarize numeric columns in dataframes to means

This simple helper function was created to summarize episodes of gaps,
clusters, or states, focusing on numeric variables. It calculates mean
values for all numeric columns and handles Duration objects
appropriately.

Despite its name, the function actually summarizes all double columns,
which is more inclusive compared to just numeric columns.

## Usage

``` r
summarize_numeric(
  data,
  remove = NULL,
  prefix = "mean_",
  na.rm = TRUE,
  complete.groups.on = NULL,
  add.total.duration = TRUE,
  durations.dec = 0,
  Datetime2Time = TRUE,
  Datetime2Time.circular = FALSE
)

summarise_numeric(
  data,
  remove = NULL,
  prefix = "mean_",
  na.rm = TRUE,
  complete.groups.on = NULL,
  add.total.duration = TRUE,
  durations.dec = 0,
  Datetime2Time = TRUE,
  Datetime2Time.circular = FALSE
)
```

## Arguments

- data:

  A dataframe containing numeric data, typically from
  [`extract_clusters()`](https://tscnlab.github.io/LightLogR/reference/extract_clusters.md)
  or
  [`extract_gaps()`](https://tscnlab.github.io/LightLogR/reference/extract_gaps.md).

- remove:

  Character vector of columns removed from the summary.

- prefix:

  A prefix to add to the column names of summarized metrics. Defaults to
  "mean\_".

- na.rm:

  Whether to remove NA values when calculating means. Defaults to TRUE.

- complete.groups.on:

  Column name that, together with grouping variables, can be used to
  provide a complete set. For example, with
  [`extract_clusters()`](https://tscnlab.github.io/LightLogR/reference/extract_clusters.md),
  some days might not have clusters. They do not show up in the summary
  output then. If it is important however, to consider that there are
  zero instances, one could extract the complete set of clusters and
  non-clusters, and then set `is.cluster` in this argument, which would
  then show zero clusters for those days.

- add.total.duration:

  Logical, whether the total duration for a given group should be
  calculated. Only relevant if a column `duration` is part of the input
  data.

- durations.dec:

  Numeric of number of decimals for the mean calculation of durations
  and times. Defaults to 0.

- Datetime2Time:

  Logical of whether POSIXct columns should be transformed into
  hms(time) columns, which is usually sensible for averaging (default is
  `TRUE`). Calls
  [`Datetime2Time()`](https://tscnlab.github.io/LightLogR/reference/Datetime2Time.md)
  with default settings (all POSIXct are affected).

- Datetime2Time.circular:

  Logical of whether Time should be circular. Will be ignored if
  `Datetime2Time = FALSE`. Default is `FALSE`.

## Value

A dataframe containing the summarized metrics.

## Examples

``` r
# Extract clusters and summarize them
dataset <-
sample.data.environment %>%
aggregate_Datetime(unit = "15 mins") |>
extract_clusters(MEDI > 1000)

#input to summarize_numeric
dataset |> utils::head()
#> # A tibble: 6 × 6
#> # Groups:   Id [1]
#>   Id      state.count start               end                 epoch             
#>   <fct>   <chr>       <dttm>              <dttm>              <Duration>        
#> 1 Enviro… 1           2023-08-29 06:52:30 2023-08-29 19:52:30 900s (~15 minutes)
#> 2 Enviro… 2           2023-08-30 06:52:30 2023-08-30 20:07:30 900s (~15 minutes)
#> 3 Enviro… 3           2023-08-31 06:52:30 2023-08-31 19:37:30 900s (~15 minutes)
#> 4 Enviro… 4           2023-09-01 07:07:30 2023-09-01 19:52:30 900s (~15 minutes)
#> 5 Enviro… 5           2023-09-02 06:52:30 2023-09-02 19:52:30 900s (~15 minutes)
#> 6 Enviro… 6           2023-09-03 06:52:30 2023-09-03 19:52:30 900s (~15 minutes)
#> # ℹ 1 more variable: duration <Duration>
#output of summarize_numeric (removing state.count and epoch from the summary)
dataset |> summarize_numeric(c("state.count", "epoch"))
#> # A tibble: 2 × 6
#>   Id    mean_start mean_end mean_duration         total_duration        episodes
#>   <fct> <time>     <time>   <Duration>            <Duration>               <int>
#> 1 Envi… 06:55:00   19:52:30 46650s (~12.96 hours) 279900s (~3.24 days)         6
#> 2 Part… 13:40:30   15:27:00 6390s (~1.77 hours)   63900s (~17.75 hours)       10
```

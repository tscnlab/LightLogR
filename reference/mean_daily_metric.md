# Calculate mean daily metrics from Time Series

`mean_daily_metric` is a convenience wrapper around `mean_daily` that
summarizes data imported with LightLogR per weekday and calculates mean
daily values for a specific metric. Examples include
[`duration_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/duration_above_threshold.md)
(the default), or
[`durations()`](https://tscnlab.github.io/LightLogR/reference/durations.md).

## Usage

``` r
mean_daily_metric(
  data,
  Variable,
  Weekend.type = Date,
  Datetime.colname = Datetime,
  metric_type = duration_above_threshold,
  prefix = "average_",
  filter.empty = FALSE,
  Datetime2Time = TRUE,
  ...
)
```

## Arguments

- data:

  A dataframe containing light logger data imported with LightLogR

- Variable:

  The variable column to analyze. Expects a `symbol`. Needs to be part
  of the dataset.

- Weekend.type:

  A (new) column in the dataframe that specifies the day of the week as
  a factor

- Datetime.colname:

  Column name containing datetime values. Defaults to `Datetime`

- metric_type:

  The metric function to apply, default is
  [`duration_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/duration_above_threshold.md)

- prefix:

  String that is the prefix on summarized values

- filter.empty:

  Filter out empty rows. Default is FALSE

- Datetime2Time:

  Logical of whether POSIXct columns should be transformed into
  hms(time) columns, which is usually sensible for averaging (default is
  `TRUE`). Calls
  [`Datetime2Time()`](https://tscnlab.github.io/LightLogR/reference/Datetime2Time.md)
  with default settings (all POSIXct are affected).

- ...:

  Additional arguments passed to the metric function

## Value

A dataframe with three rows representing average weekday, weekend, and
mean daily values for the specified metric

## Examples

``` r
# Calculate mean daily duration above threshold. As the data only contains
# data for two days, Weekend and Mean daily will throw NA
sample.data.irregular |> 
aggregate_Datetime(unit = "1 min") |> 
mean_daily_metric(
  Variable = lux,
  threshold = 100
)
#> # A tibble: 3 × 3
#> # Groups:   Id [1]
#>   Id    Date       average_duration_above_100
#>   <chr> <chr>      <Duration>                
#> 1 P1    Mean daily NA                        
#> 2 P1    Weekday    17220s (~4.78 hours)      
#> 3 P1    Weekend    NA                        

# again with another dataset
sample.data.environment |> 
  mean_daily_metric(
  Variable = MEDI,
  threshold = 250)
#> # A tibble: 6 × 3
#> # Groups:   Id [2]
#>   Id          Date       average_duration_above_250
#>   <fct>       <chr>      <Duration>                
#> 1 Environment Mean daily 48710s (~13.53 hours)     
#> 2 Environment Weekday    48712s (~13.53 hours)     
#> 3 Environment Weekend    48705s (~13.53 hours)     
#> 4 Participant Mean daily 15716s (~4.37 hours)      
#> 5 Participant Weekday    11495s (~3.19 hours)      
#> 6 Participant Weekend    26270s (~7.3 hours)       

# by default, datetime columns are converted to time
sample.data.environment |> 
  mean_daily_metric(
  Variable = MEDI,
  metric_type = timing_above_threshold,
  threshold = 250)
#> # A tibble: 6 × 5
#> # Groups:   Id [2]
#>   Id          Date       average_mean_timing_above_250 average_first_timing_ab…¹
#>   <fct>       <chr>      <time>                        <time>                   
#> 1 Environment Mean daily 13:24:00                      06:38:19                 
#> 2 Environment Weekday    13:24:12                      06:38:30                 
#> 3 Environment Weekend    13:23:30                      06:37:53                 
#> 4 Participant Mean daily 15:15:33                      09:58:06                 
#> 5 Participant Weekday    15:11:08                      09:58:09                 
#> 6 Participant Weekend    15:26:37                      09:57:59                 
#> # ℹ abbreviated name: ¹​average_first_timing_above_250
#> # ℹ 1 more variable: average_last_timing_above_250 <time>
```

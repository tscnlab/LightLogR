# Calculate mean daily metrics from daily summary

`mean_daily` calculates a three-row summary of metrics showing average
weekday, weekend, and mean daily values of all non-grouping numeric
columns. The basis is a dataframe that contains metrics per weekday, or
per date (with `calculate.from.Date = Datetime`). The function requires
a column specifying the day of the week as a factor (with Monday as the
weekstart), or it can calculate this from a date column if provided.

## Usage

``` r
mean_daily(
  data,
  Weekend.type = Date,
  na.rm = TRUE,
  calculate.from.Date = NULL,
  prefix = "average_",
  filter.empty = FALSE,
  sub.zero = FALSE,
  Datetime2Time = TRUE,
  Datetime2Time.circular = FALSE
)
```

## Arguments

- data:

  A dataframe containing the metrics to summarize

- Weekend.type:

  A column in the dataframe that specifies the day of the week as a
  factor, where weekstart is Monday (so weekends are 6 and 7 in numeric
  representation). If it is a date, it will be converted to this factor

- na.rm:

  Logical, whether to remove NA values when calculating means. Default
  is TRUE.

- calculate.from.Date:

  Optional. A column in the dataframe containing dates from which to
  calculate the Weekend.type. If provided, Weekend.type will be
  generated from this column.

- prefix:

  String that is the prefix on summarized values

- filter.empty:

  Filter out empty rows. Default is FALSE

- sub.zero:

  Logical. Should missing values be replaced by zero? Defaults to
  `FALSE`. Will throw an error, if it happens on a type other than
  `double`.

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

A dataframe with three rows representing average weekday, weekend, and
mean daily values of all numeric columns

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
# Create sample data
sample_data <- data.frame(
  Date = factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
               levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
  lux = c(250, 300, 275, 280, 290, 350, 320),
  duration = lubridate::as.duration(c(120, 130, 125, 135, 140, 180, 160))
)

# Calculate mean daily metrics
mean_daily(sample_data)
#> # A tibble: 3 × 3
#>   Date       average_lux average_duration    
#>   <chr>            <dbl> <Duration>          
#> 1 Mean daily         295 141s (~2.35 minutes)
#> 2 Weekday            279 130s (~2.17 minutes)
#> 3 Weekend            335 170s (~2.83 minutes)

# With a Date column
sample_data_with_date <- data.frame(
  Date = seq(as.Date("2023-05-01"), as.Date("2023-05-07"), by = "day"),
  lux = c(250, 300, 275, 280, 290, 350, 320),
  duration = lubridate::as.duration(c(120, 130, 125, 135, 140, 180, 160))
)

mean_daily(sample_data_with_date)
#> # A tibble: 3 × 3
#>   Date       average_lux average_duration    
#>   <chr>            <dbl> <Duration>          
#> 1 Mean daily         295 141s (~2.35 minutes)
#> 2 Weekday            279 130s (~2.17 minutes)
#> 3 Weekend            335 170s (~2.83 minutes)
```

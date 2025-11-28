# Length of longest continuous period above/below threshold

This function finds the length of the longest continous period
above/below a specified threshold light level or within a specified
range of light levels.

## Usage

``` r
period_above_threshold(
  Light.vector,
  Time.vector,
  comparison = c("above", "below"),
  threshold,
  epoch = "dominant.epoch",
  loop = FALSE,
  na.replace = FALSE,
  na.rm = FALSE,
  as.df = FALSE
)
```

## Arguments

- Light.vector:

  Numeric vector containing the light data.

- Time.vector:

  Vector containing the time data. Can be
  [POSIXct](https://rdrr.io/r/base/DateTimeClasses.html),
  [hms](https://hms.tidyverse.org/reference/hms.html),
  [duration](https://lubridate.tidyverse.org/reference/duration.html),
  or [difftime](https://rdrr.io/r/base/difftime.html).

- comparison:

  String specifying whether the period of light levels above or below
  threshold should be calculated. Can be either `"above"` (the default)
  or `"below"`. If two values are provided for `threshold`, this
  argument will be ignored.

- threshold:

  Single numeric value or two numeric values specifying the threshold
  light level(s) to compare with. If a vector with two values is
  provided, the period of light levels within the two thresholds will be
  calculated.

- epoch:

  The epoch at which the data was sampled. Can be either a
  [duration](https://lubridate.tidyverse.org/reference/duration.html) or
  a string. If it is a string, it needs to be either `"dominant.epoch"`
  (the default) for a guess based on the data, or a valid
  [duration](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

- loop:

  Logical. Should the data be looped? Defaults to `FALSE`.

- na.replace:

  Logical. Should missing values (NA) be replaced for the calculation?
  If `TRUE` missing values will not be removed but will result in
  `FALSE` when comparing `Light.vector` with `threshold`. Defaults to
  `FALSE`.

- na.rm:

  Logical. Should missing values (NA) be removed for the calculation? If
  `TRUE`, this argument will override `na.replace`. Defaults to `FALSE`.

- as.df:

  Logical. Should a data frame be returned? If `TRUE`, a data frame with
  a single column named `period_{comparison}_{threshold}` will be
  returned. Defaults to `FALSE`.

## Value

A duration object (see
[`duration`](https://lubridate.tidyverse.org/reference/duration.html))
as single value, or single column data frame.

## See also

Other metrics:
[`bright_dark_period()`](https://tscnlab.github.io/LightLogR/reference/bright_dark_period.md),
[`centroidLE()`](https://tscnlab.github.io/LightLogR/reference/centroidLE.md),
[`disparity_index()`](https://tscnlab.github.io/LightLogR/reference/disparity_index.md),
[`dose()`](https://tscnlab.github.io/LightLogR/reference/dose.md),
[`duration_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/duration_above_threshold.md),
[`exponential_moving_average()`](https://tscnlab.github.io/LightLogR/reference/exponential_moving_average.md),
[`frequency_crossing_threshold()`](https://tscnlab.github.io/LightLogR/reference/frequency_crossing_threshold.md),
[`interdaily_stability()`](https://tscnlab.github.io/LightLogR/reference/interdaily_stability.md),
[`intradaily_variability()`](https://tscnlab.github.io/LightLogR/reference/intradaily_variability.md),
[`midpointCE()`](https://tscnlab.github.io/LightLogR/reference/midpointCE.md),
[`nvRC()`](https://tscnlab.github.io/LightLogR/reference/nvRC.md),
[`nvRD()`](https://tscnlab.github.io/LightLogR/reference/nvRD.md),
[`nvRD_cumulative_response()`](https://tscnlab.github.io/LightLogR/reference/nvRD_cumulative_response.md),
[`pulses_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/pulses_above_threshold.md),
[`threshold_for_duration()`](https://tscnlab.github.io/LightLogR/reference/threshold_for_duration.md),
[`timing_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/timing_above_threshold.md)

## Examples

``` r
N <- 60
# Dataset with continous period of >250lx for 35min
dataset1 <-
  tibble::tibble(
    Id = rep("A", N),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(1:N),
    MEDI = c(sample(1:249, N-35, replace = TRUE), 
             sample(250:1000, 35, replace = TRUE))
  )

dataset1 %>%
  dplyr::reframe("Period >250lx" = period_above_threshold(MEDI, Datetime, threshold = 250))
#> # A tibble: 1 × 1
#>   `Period >250lx`    
#>   <Duration>         
#> 1 2100s (~35 minutes)

dataset1 %>%
  dplyr::reframe("Period <250lx" = period_above_threshold(MEDI, Datetime, "below", threshold = 250))
#> # A tibble: 1 × 1
#>   `Period <250lx`    
#>   <Duration>         
#> 1 1500s (~25 minutes)

# Dataset with continous period of 100-250lx for 20min
dataset2 <-
  tibble::tibble(
    Id = rep("B", N),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(1:N),
    MEDI = c(sample(c(1:99, 251-1000), N-20, replace = TRUE), 
             sample(100:250, 20, replace = TRUE)),
  )
dataset2 %>%
  dplyr::reframe("Period 250lx" = period_above_threshold(MEDI, Datetime, threshold = c(100,250)))
#> # A tibble: 1 × 1
#>   `Period 250lx`     
#>   <Duration>         
#> 1 1200s (~20 minutes)

# Return data frame
dataset1 %>%
  dplyr::reframe(period_above_threshold(MEDI, Datetime, threshold = 250, as.df = TRUE))
#> # A tibble: 1 × 1
#>   period_above_250   
#>   <Duration>         
#> 1 2100s (~35 minutes)
```

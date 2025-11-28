# Find threshold for given duration

This function finds the threshold for which light levels are above/below
for a given duration. This function can be considered as the inverse of
[`duration_above_threshold`](https://tscnlab.github.io/LightLogR/reference/duration_above_threshold.md).

## Usage

``` r
threshold_for_duration(
  Light.vector,
  Time.vector,
  duration,
  comparison = c("above", "below"),
  epoch = "dominant.epoch",
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

- duration:

  The duration for which the threshold should be found. Can be either a
  [duration](https://lubridate.tidyverse.org/reference/duration.html) or
  a string. If it is a string, it needs to be a valid
  [duration](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

- comparison:

  String specifying whether light levels above or below the threshold
  should be considered. Can be either `"above"` (the default) or
  `"below"`.

- epoch:

  The epoch at which the data was sampled. Can be either a
  [duration](https://lubridate.tidyverse.org/reference/duration.html) or
  a string. If it is a string, it needs to be either `"dominant.epoch"`
  (the default) for a guess based on the data, or a valid
  [duration](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

- na.rm:

  Logical. Should missing values (NA) be removed for the calculation?
  Defaults to `FALSE`.

- as.df:

  Logical. Should a data frame with be returned? If `TRUE`, a data frame
  with a single column named `threshold_{comparison}_for_{duration}`
  will be returned. Defaults to `FALSE`.

## Value

Single numeric value or single column data frame.

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
[`period_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/period_above_threshold.md),
[`pulses_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/pulses_above_threshold.md),
[`timing_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/timing_above_threshold.md)

## Examples

``` r
N <- 60
# Dataset with 30 min < 250lx and 30min > 250lx
dataset1 <-
  tibble::tibble(
    Id = rep("A", N),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(1:N),
    MEDI = sample(c(sample(1:249, N / 2, replace = TRUE), 
                    sample(250:1000, N / 2, replace = TRUE))),
  )

dataset1 %>%
  dplyr::reframe("Threshold above which for 30 mins" = 
                   threshold_for_duration(MEDI, Datetime, duration = "30 mins"))
#> # A tibble: 1 × 1
#>   `Threshold above which for 30 mins`
#>                                 <int>
#> 1                                 276

dataset1 %>%
  dplyr::reframe("Threshold below which for 30 mins" = 
                   threshold_for_duration(MEDI, Datetime, duration = "30 mins",
                                          comparison = "below"))
#> # A tibble: 1 × 1
#>   `Threshold below which for 30 mins`
#>                                 <int>
#> 1                                 248

dataset1 %>%
  dplyr::reframe(threshold_for_duration(MEDI, Datetime, duration = "30 mins",
                                        as.df = TRUE))
#> # A tibble: 1 × 1
#>   threshold_above_for_30_minutes
#>                            <int>
#> 1                            276
```

# Midpoint of cumulative light exposure.

This function calculates the timing corresponding to half of the
cumulative light exposure within the given time series.

## Usage

``` r
midpointCE(Light.vector, Time.vector, na.rm = FALSE, as.df = FALSE)
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

- na.rm:

  Logical. Should missing values be removed for the calculation? If
  `TRUE`, missing values will be replaced by zero. Defaults to `FALSE`.

- as.df:

  Logical. Should the output be returned as a data frame? If `TRUE`, a
  data frame with a single column named `midpointCE` will be returned.
  Defaults to `FALSE`.

## Value

Single column data frame or vector.

## References

Shochat, T., Santhi, N., Herer, P., Flavell, S. A., Skeldon, A. C., &
Dijk, D.-J. (2019). Sleep Timing in Late Autumn and Late Spring
Associates With Light Exposure Rather Than Sun Time in College Students.
*Frontiers in Neuroscience*, 13.
[doi:10.3389/fnins.2019.00882](https://doi.org/10.3389/fnins.2019.00882)

Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for
light-dosimetry studies: Quantification metrics. *Lighting Research &
Technology*.
[doi:10.1177/14771535231170500](https://doi.org/10.1177/14771535231170500)

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
[`nvRC()`](https://tscnlab.github.io/LightLogR/reference/nvRC.md),
[`nvRD()`](https://tscnlab.github.io/LightLogR/reference/nvRD.md),
[`nvRD_cumulative_response()`](https://tscnlab.github.io/LightLogR/reference/nvRD_cumulative_response.md),
[`period_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/period_above_threshold.md),
[`pulses_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/pulses_above_threshold.md),
[`threshold_for_duration()`](https://tscnlab.github.io/LightLogR/reference/threshold_for_duration.md),
[`timing_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/timing_above_threshold.md)

## Examples

``` r
dataset1 <-
  tibble::tibble(
    Id = rep("A", 24),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )
dataset1 %>%
  dplyr::reframe(
    "Midpoint of cmulative exposure" = midpointCE(MEDI, Datetime)
  )
#> # A tibble: 1 × 1
#>   `Midpoint of cmulative exposure`
#>   <dttm>                          
#> 1 1970-01-01 11:00:00             

# Dataset with HMS time vector
dataset2 <-
  tibble::tibble(
    Id = rep("A", 24),
    Time = hms::as_hms(lubridate::as_datetime(0) + lubridate::hours(0:23)),
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )
dataset2 %>%
  dplyr::reframe(
    "Midpoint of cmulative exposure" = midpointCE(MEDI, Time)
  )
#> # A tibble: 1 × 1
#>   `Midpoint of cmulative exposure`
#>   <time>                          
#> 1 11:00                           

# Dataset with duration time vector
dataset3 <-
  tibble::tibble(
    Id = rep("A", 24),
    Hour = lubridate::duration(0:23, "hours"),
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )
dataset3 %>%
  dplyr::reframe(
    "Midpoint of cmulative exposure" = midpointCE(MEDI, Hour)
  )
#> # A tibble: 1 × 1
#>   `Midpoint of cmulative exposure`
#>   <Duration>                      
#> 1 39600s (~11 hours)              
```

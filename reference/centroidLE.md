# Centroid of light exposure

This function calculates the centroid of light exposure as the mean of
the time vector weighted in proportion to the corresponding binned light
intensity.

## Usage

``` r
centroidLE(
  Light.vector,
  Time.vector,
  bin.size = NULL,
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

- bin.size:

  Value specifying size of bins to average the light data over. Must be
  either a
  [duration](https://lubridate.tidyverse.org/reference/duration.html) or
  a [duration](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`. If nothing is provided, no
  binning will be performed.

- na.rm:

  Logical. Should missing values be removed for the calculation?
  Defaults to `FALSE`.

- as.df:

  Logical. Should the output be returned as a data frame? If `TRUE`, a
  data frame with a single column named `centroidLE` will be returned.
  Defaults to `FALSE`.

## Value

Single column data frame or vector.

## References

Phillips, A. J. K., Clerx, W. M., O’Brien, C. S., Sano, A., Barger, L.
K., Picard, R. W., Lockley, S. W., Klerman, E. B., & Czeisler, C. A.
(2017). Irregular sleep/wake patterns are associated with poorer
academic performance and delayed circadian and sleep/wake timing.
*Scientific Reports*, 7(1), 3216.
[doi:10.1038/s41598-017-03171-4](https://doi.org/10.1038/s41598-017-03171-4)

Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for
light-dosimetry studies: Quantification metrics. *Lighting Research &
Technology*.
[doi:10.1177/14771535231170500](https://doi.org/10.1177/14771535231170500)

## See also

Other metrics:
[`bright_dark_period()`](https://tscnlab.github.io/LightLogR/reference/bright_dark_period.md),
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
[`threshold_for_duration()`](https://tscnlab.github.io/LightLogR/reference/threshold_for_duration.md),
[`timing_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/timing_above_threshold.md)

## Examples

``` r
# Dataset with POSIXct time vector
dataset1 <-
  tibble::tibble(
    Id = rep("A", 24),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )
dataset1 %>%
  dplyr::reframe(
    "Centroid of light exposure" = centroidLE(MEDI, Datetime, "2 hours")
  )
#> # A tibble: 1 × 1
#>   `Centroid of light exposure`
#>   <dttm>                      
#> 1 1970-01-01 11:32:04         

# Dataset with hms time vector
dataset2 <-
  tibble::tibble(
    Id = rep("A", 24),
    Time = hms::as_hms(lubridate::as_datetime(0) + lubridate::hours(0:23)),
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )
dataset2 %>%
  dplyr::reframe(
    "Centroid of light exposure" = centroidLE(MEDI, Time, "2 hours")
  )
#> # A tibble: 1 × 1
#>   `Centroid of light exposure`
#>   <time>                      
#> 1 11:32:04                    

# Dataset with duration time vector
dataset3 <-
  tibble::tibble(
    Id = rep("A", 24),
    Hour = lubridate::duration(0:23, "hours"),
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )
dataset3 %>%
  dplyr::reframe(
    "Centroid of light exposure" = centroidLE(MEDI, Hour, "2 hours")
  )
#> # A tibble: 1 × 1
#>   `Centroid of light exposure`
#>   <Duration>                  
#> 1 41524s (~11.53 hours)       
```

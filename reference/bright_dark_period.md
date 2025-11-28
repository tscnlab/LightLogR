# Brightest or darkest continuous period

This function finds the brightest or darkest continuous period of a
given timespan and calculates its `mean` light level, as well as the
timing of the period's `onset`, `midpoint`, and `offset`. It is defined
as the period with the maximum or minimum mean light level. Note that
the data need to be regularly spaced (i.e., no gaps) for correct
results.

## Usage

``` r
bright_dark_period(
  Light.vector,
  Time.vector,
  period = c("brightest", "darkest"),
  timespan = "10 hours",
  epoch = "dominant.epoch",
  loop = FALSE,
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

- period:

  String indicating the type of period to look for. Can be either
  `"brightest"`(the default) or `"darkest"`.

- timespan:

  The timespan across which to calculate. Can be either a
  [duration](https://lubridate.tidyverse.org/reference/duration.html) or
  a [duration](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

- epoch:

  The epoch at which the data was sampled. Can be either a
  [duration](https://lubridate.tidyverse.org/reference/duration.html) or
  a string. If it is a string, it needs to be either `"dominant.epoch"`
  (the default) for a guess based on the data, or a valid
  [duration](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

- loop:

  Logical. Should the data be looped? If `TRUE`, a full copy of the data
  will be concatenated at the end of the data. Makes only sense for 24 h
  data. Defaults to `FALSE`.

- na.rm:

  Logical. Should missing values be removed for the calculation?
  Defaults to `FALSE`.

- as.df:

  Logical. Should the output be returned as a data frame? Defaults to
  `TRUE`.

## Value

A named list with the `mean`, `onset`, `midpoint`, and `offset` of the
calculated brightest or darkest period, or if `as.df == TRUE` a data
frame with columns named `{period}_{timespan}_{metric}`. The output type
corresponds to the type of `Time.vector`, e.g., if `Time.vector` is HMS,
the timing metrics will be also HMS, and vice versa for POSIXct.

## Details

Assumes regular 24h light data. Otherwise, results may not be
meaningful. Looping the data is recommended for finding the darkest
period.

## References

Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for
light-dosimetry studies: Quantification metrics. *Lighting Research &
Technology*.
[doi:10.1177/14771535231170500](https://doi.org/10.1177/14771535231170500)

## See also

Other metrics:
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
[`threshold_for_duration()`](https://tscnlab.github.io/LightLogR/reference/threshold_for_duration.md),
[`timing_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/timing_above_threshold.md)

## Examples

``` r
# Dataset with light > 250lx between 06:00 and 18:00
dataset1 <-
  tibble::tibble(
    Id = rep("A", 24),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )

dataset1 %>%
  dplyr::reframe(bright_dark_period(MEDI, Datetime, "brightest", "10 hours",
    as.df = TRUE))
#> # A tibble: 1 × 4
#>   brightest_10h_mean brightest_10h_midpoint brightest_10h_onset
#>                <dbl> <dttm>                 <dttm>             
#> 1                250 1970-01-01 10:00:00    1970-01-01 06:00:00
#> # ℹ 1 more variable: brightest_10h_offset <dttm>
dataset1 %>%
  dplyr::reframe(bright_dark_period(MEDI, Datetime, "darkest", "7 hours",
    loop = TRUE, as.df = TRUE))
#> # A tibble: 1 × 4
#>   darkest_7h_mean darkest_7h_midpoint darkest_7h_onset    darkest_7h_offset  
#>             <dbl> <dttm>              <dttm>              <dttm>             
#> 1               1 1970-01-01 22:00:00 1970-01-01 19:00:00 1970-01-01 02:00:00

# Dataset with duration as Time.vector
dataset2 <-
  tibble::tibble(
    Id = rep("A", 24),
    Datetime = lubridate::dhours(0:23),
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )

dataset2 %>%
  dplyr::reframe(bright_dark_period(MEDI, Datetime, "brightest", "10 hours",
                                    as.df = TRUE))
#> # A tibble: 1 × 4
#>   brightest_10h_mean brightest_10h_midpoint brightest_10h_onset
#>                <dbl> <Duration>             <Duration>         
#> 1                250 36000s (~10 hours)     21600s (~6 hours)  
#> # ℹ 1 more variable: brightest_10h_offset <Duration>
dataset2 %>%
  dplyr::reframe(bright_dark_period(MEDI, Datetime, "darkest", "5 hours",
                                    loop = TRUE, as.df = TRUE))
#> # A tibble: 1 × 4
#>   darkest_5h_mean darkest_5h_midpoint darkest_5h_onset darkest_5h_offset
#>             <dbl> <Duration>          <Duration>       <Duration>       
#> 1               1 7200s (~2 hours)    0s               18000s (~5 hours)
```

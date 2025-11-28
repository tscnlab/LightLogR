# Cumulative non-visual direct response

This function calculates the cumulative non-visual direct response
(nvRD). This is basically the integral of the nvRD over the provided
time period in hours. The unit of the resulting value thus is "nvRD\*h".

## Usage

``` r
nvRD_cumulative_response(
  nvRD,
  Time.vector,
  epoch = "dominant.epoch",
  as.df = FALSE
)
```

## Arguments

- nvRD:

  Numeric vector containing the non-visual direct response. See
  [`nvRD`](https://tscnlab.github.io/LightLogR/reference/nvRD.md).

- Time.vector:

  Vector containing the time data. Can be
  [POSIXct](https://rdrr.io/r/base/DateTimeClasses.html),
  [hms](https://hms.tidyverse.org/reference/hms.html),
  [duration](https://lubridate.tidyverse.org/reference/duration.html),
  or [difftime](https://rdrr.io/r/base/difftime.html).

- epoch:

  The epoch at which the data was sampled. Can be either a
  [duration](https://lubridate.tidyverse.org/reference/duration.html) or
  a string. If it is a string, it needs to be either `"dominant.epoch"`
  (the default) for a guess based on the data, or a valid
  [duration](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

- as.df:

  Logical. Should a data frame with be returned? If `TRUE`, a data frame
  with a single column named `nvRD_cumulative` will be returned.
  Defaults to `FALSE`.

## Value

A numeric value or single column data frame.

## References

Amundadottir, M.L. (2016). Light-driven model for identifying indicators
of non-visual health potential in the built environment \[Doctoral
dissertation, EPFL\]. EPFL infoscience.
[doi:10.5075/epfl-thesis-7146](https://doi.org/10.5075/epfl-thesis-7146)

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
[`period_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/period_above_threshold.md),
[`pulses_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/pulses_above_threshold.md),
[`threshold_for_duration()`](https://tscnlab.github.io/LightLogR/reference/threshold_for_duration.md),
[`timing_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/timing_above_threshold.md)

## Examples

``` r
dataset1 <-
  tibble::tibble(
    Id = rep("A", 60 * 24),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*24-1)),
    Illuminance = c(rep(0, 60*8), rep(sample(1:1000, 14, replace = TRUE), each = 60), rep(0, 60*2)),
    MEDI = Illuminance * rep(sample(0.5:1.5, 24, replace = TRUE), each = 60)
  ) %>%
  dplyr::mutate(
    nvRD = nvRD(MEDI, Illuminance, Datetime)
  ) 
dataset1 %>% 
  dplyr::summarise(
    "cumulative nvRD" = nvRD_cumulative_response(nvRD, Datetime)
  )
#> # A tibble: 1 × 1
#>   `cumulative nvRD`
#>               <dbl>
#> 1              10.7
```

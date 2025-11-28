# Calculate the dose (value·hours)

This function calculates the dose from a time series. For light, this is
equal to the actual definition of light exposure (CIE term luminous
exposure). Output will always be provided in value·hours (e.g., for
light, lx·hours).

## Usage

``` r
dose(
  Light.vector,
  Time.vector,
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
  with a single column named `dose` will be returned. Defaults to
  `FALSE`.

## Value

A numeric object as single value, or single column data frame with the
dose in value·hours

## Details

The time series does not have to be regular, however, it will be
aggregated to a regular timeseries of the given epoch. Implicit gaps
(i.e., no observations), will be converted to NA values (which can be
ignored with `na.rm = TRUE`).

## References

Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for
light-dosimetry studies: Quantification metrics. *Lighting Research &
Technology*.
[doi:10.1177/14771535231170500](https://doi.org/10.1177/14771535231170500)

## See also

Other metrics:
[`bright_dark_period()`](https://tscnlab.github.io/LightLogR/reference/bright_dark_period.md),
[`centroidLE()`](https://tscnlab.github.io/LightLogR/reference/centroidLE.md),
[`disparity_index()`](https://tscnlab.github.io/LightLogR/reference/disparity_index.md),
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
dose(c(1,1,1,1), lubridate::dhours(c(1:4)), na.rm = TRUE)
#> [1] 4
#with gaps
dose(c(1,1,1), lubridate::dhours(c(1,3:4)), na.rm = TRUE)
#> [1] 3
#gaps can be aggregated to a coarser interval, which can be sensibe
#if they are still representative
dose(c(1,1,1), lubridate::dhours(c(1,3:4)), na.rm = TRUE, epoch = "2 hours")
#> [1] 4
```

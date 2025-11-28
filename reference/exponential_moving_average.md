# Exponential moving average filter (EMA)

This function smoothes the data using an exponential moving average
filter with a specified decay half-life.

## Usage

``` r
exponential_moving_average(
  Light.vector,
  Time.vector,
  decay = "90 min",
  epoch = "dominant.epoch"
)
```

## Arguments

- Light.vector:

  Numeric vector containing the light data. Missing values are replaced
  by 0.

- Time.vector:

  Vector containing the time data. Can be
  [POSIXct](https://rdrr.io/r/base/DateTimeClasses.html),
  [hms](https://hms.tidyverse.org/reference/hms.html),
  [duration](https://lubridate.tidyverse.org/reference/duration.html),
  or [difftime](https://rdrr.io/r/base/difftime.html).

- decay:

  The decay half-life controlling the exponential smoothing. Can be
  either a
  [duration](https://lubridate.tidyverse.org/reference/duration.html) or
  a string. If it is a string, it needs to be a valid
  [duration](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`. The default is set to
  `"90 mins"` for a biologically relevant estimate (see the reference
  paper).

- epoch:

  The epoch at which the data was sampled. Can be either a
  [duration](https://lubridate.tidyverse.org/reference/duration.html) or
  a string. If it is a string, it needs to be either `"dominant.epoch"`
  (the default) for a guess based on the data, or a valid
  [duration](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

## Value

A numeric vector containing the smoothed light data. The output has the
same length as `Light.vector`.

## Details

The timeseries is assumed to be regular. Missing values in the light
data will be replaced by 0.

## References

Price, L. L. A. (2014). On the Role of Exponential Smoothing in
Circadian Dosimetry. *Photochemistry and Photobiology*, 90(5),
1184-1192. [doi:10.1111/php.12282](https://doi.org/10.1111/php.12282)

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
sample.data.environment.EMA = sample.data.environment %>%
  dplyr::filter(Id == "Participant") %>%
  filter_Datetime(length = lubridate::days(2)) %>%
  dplyr::mutate(MEDI.EMA = exponential_moving_average(MEDI, Datetime))

# Plot to compare results
sample.data.environment.EMA %>%
  ggplot2::ggplot(ggplot2::aes(x = Datetime)) +
  ggplot2::geom_line(ggplot2::aes(y = MEDI), colour = "black") +
  ggplot2::geom_line(ggplot2::aes(y = MEDI.EMA), colour = "red")

  
```

# Non-visual circadian response

This function calculates the non-visual circadian response (nvRC). It
takes into account the assumed response dynamics of the non-visual
system and the circadian rhythm and processes the light exposure signal
to quantify the effective circadian-weighted input to the non-visual
system (see Details).

## Usage

``` r
nvRC(
  MEDI.vector,
  Illuminance.vector,
  Time.vector,
  epoch = "dominant.epoch",
  sleep.onset = NULL
)
```

## Arguments

- MEDI.vector:

  Numeric vector containing the melanopic EDI data.

- Illuminance.vector:

  Numeric vector containing the Illuminance data.

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

- sleep.onset:

  The time of habitual sleep onset. Can be HMS, numeric, or NULL. If
  NULL (the default), then the data is assumed to start at habitual
  sleep onset. If `Time.vector` is HMS or POSIXct, `sleep.onset` must be
  HMS. Likewise, if `Time.vector` is numeric, `sleep.onset` must be
  numeric.

## Value

A numeric vector containing the nvRC data. The output has the same
length as `Time.vector`.

## Details

The timeseries is assumed to be regular. Missing values in the light
data will be replaced by 0.

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
    Id = rep("B", 60 * 48),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*48-1)),
    Illuminance = c(rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60),
                    rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60)),
    MEDI = Illuminance * rep(sample(0.5:1.5, 48, replace = TRUE), each = 60)
  )
# Time.vector as POSIXct
dataset1.nvRC <- dataset1 %>%
  dplyr::mutate(
    nvRC = nvRC(MEDI, Illuminance, Datetime, sleep.onset = hms::as_hms("22:00:00"))
  )
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `nvRC = nvRC(MEDI, Illuminance, Datetime, sleep.onset =
#>   hms::as_hms("22:00:00"))`.
#> Caused by warning:
#> ! `is.hms()` was deprecated in hms 1.2.0.
#> ℹ Please use `is_hms()` instead.
#> ℹ The deprecated feature was likely used in the LightLogR package.
#>   Please report the issue at <https://github.com/tscnlab/LightLogR/issues>.

# Time.vector as difftime
dataset2 <- dataset1 %>% 
  dplyr::mutate(Datetime = Datetime - lubridate::as_datetime(lubridate::dhours(22)))
dataset2.nvRC <- dataset2 %>%
  dplyr::mutate(
    nvRC = nvRC(MEDI, Illuminance, Datetime, sleep.onset = lubridate::dhours(0))
  )
  
```

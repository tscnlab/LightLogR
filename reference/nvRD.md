# Non-visual direct response

This function calculates the non-visual direct response (nvRD). It takes
into account the assumed response dynamics of the non-visual system and
processes the light exposure signal to quantify the effective direct
input to the non-visual system (see Details).

## Usage

``` r
nvRD(MEDI.vector, Illuminance.vector, Time.vector, epoch = "dominant.epoch")
```

## Arguments

- MEDI.vector:

  Numeric vector containing the melanopic EDI data.

- Illuminance.vector:

  Numeric vector containing the Illuminance data.

- Time.vector:

  Vector containing the time data. Can be
  [`POSIXct()`](https://lubridate.tidyverse.org/reference/posix_utils.html),[`hms::hms()`](https://hms.tidyverse.org/reference/hms.html),
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html),
  [`difftime()`](https://rdrr.io/r/base/difftime.html).

- epoch:

  The epoch at which the data was sampled. Can be either a
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
  or a string. If it is a string, it needs to be either
  `"dominant.epoch"` (the default) for a guess based on the data, or a
  valid
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

## Value

A numeric vector containing the nvRD data. The output has the same
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
[`nvRC()`](https://tscnlab.github.io/LightLogR/reference/nvRC.md),
[`nvRD_cumulative_response()`](https://tscnlab.github.io/LightLogR/reference/nvRD_cumulative_response.md),
[`period_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/period_above_threshold.md),
[`pulses_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/pulses_above_threshold.md),
[`threshold_for_duration()`](https://tscnlab.github.io/LightLogR/reference/threshold_for_duration.md),
[`timing_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/timing_above_threshold.md)

## Examples

``` r
# Dataset 1 with 24h measurement
dataset1 <-
  tibble::tibble(
    Id = rep("A", 60 * 24),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*24-1)),
    Illuminance = c(rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60)),
    MEDI = Illuminance * rep(sample(0.5:1.5, 24, replace = TRUE), each = 60)
  ) 
# Dataset 2 with 48h measurement
dataset2 <-
  tibble::tibble(
    Id = rep("B", 60 * 48),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*48-1)),
    Illuminance = c(rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60), 
                    rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60)),
    MEDI = Illuminance * rep(sample(0.5:1.5, 48, replace = TRUE), each = 60)
  )
# Combined datasets
dataset.combined <- rbind(dataset1, dataset2)

# Calculate nvRD per ID
dataset.combined.nvRD <- dataset.combined %>% 
  dplyr::group_by(Id) %>% 
  dplyr::mutate(
    nvRD = nvRD(MEDI, Illuminance, Datetime)
  )
```

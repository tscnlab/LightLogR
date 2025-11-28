# Frequency of crossing light threshold

This functions calculates the number of times a given threshold light
level is crossed.

## Usage

``` r
frequency_crossing_threshold(
  Light.vector,
  threshold,
  na.rm = FALSE,
  as.df = FALSE
)
```

## Arguments

- Light.vector:

  Numeric vector containing the light data.

- threshold:

  Single numeric value specifying the threshold light level to compare
  with.

- na.rm:

  Logical. Should missing light values be removed? Defaults to `FALSE`.

- as.df:

  Logical. Should the output be returned as a data frame? If `TRUE`, a
  data frame with a single column named `frequency_crossing_{threshold}`
  will be returned. Defaults to `FALSE`.

## Value

Data frame or matrix with pairs of threshold and calculated values.

## References

Alvarez, A. A., & Wildsoet, C. F. (2013). Quantifying light exposure
patterns in young adult students. *Journal of Modern Optics*, 60(14),
1200–1208.
[doi:10.1080/09500340.2013.845700](https://doi.org/10.1080/09500340.2013.845700)

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
N = 60
dataset1 <-
  tibble::tibble(
    Id = rep("A", N),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(1:N),
    MEDI = sample(c(sample(1:249, N / 2), sample(250:1000, N / 2))),
  )

dataset1 %>%
  dplyr::reframe("Frequency crossing 250lx" = frequency_crossing_threshold(MEDI, threshold = 250))
#> # A tibble: 1 × 1
#>   `Frequency crossing 250lx`
#>                        <int>
#> 1                         34

dataset1 %>%
  dplyr::reframe(frequency_crossing_threshold(MEDI, threshold = 250, as.df = TRUE))
#> # A tibble: 1 × 1
#>   frequency_crossing_250
#>                    <int>
#> 1                     34
```

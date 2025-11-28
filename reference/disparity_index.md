# Disparity index

This function calculates the continuous disparity index as described in
Fernández-Martínez et al. (2018).

## Usage

``` r
disparity_index(Light.vector, na.rm = FALSE, as.df = FALSE)
```

## Arguments

- Light.vector:

  Numeric vector containing the light data.

- na.rm:

  Logical. Should missing values be removed? Defaults to FALSE

- as.df:

  Logical. Should the output be returned as a data frame? If `TRUE`, a
  data frame with a single column named `disparity_index` will be
  returned. Defaults to `FALSE`.

## Value

Single column data frame or vector.

## References

Fernández-Martínez, M., Vicca, S., Janssens, I. A., Carnicer, J.,
Martín-Vide, J., & Peñuelas, J. (2018). The consecutive disparity index,
D: A measure of temporal variability in ecological studies. *Ecosphere*,
9(12), e02527.
[doi:10.1002/ecs2.2527](https://doi.org/10.1002/ecs2.2527)

Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for
light-dosimetry studies: Quantification metrics. *Lighting Research &
Technology*.
[doi:10.1177/14771535231170500](https://doi.org/10.1177/14771535231170500)

## See also

Other metrics:
[`bright_dark_period()`](https://tscnlab.github.io/LightLogR/reference/bright_dark_period.md),
[`centroidLE()`](https://tscnlab.github.io/LightLogR/reference/centroidLE.md),
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
dataset1 <-
  tibble::tibble(
    Id = rep("A", 24),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
    MEDI = sample(0:1000, 24),
  )
dataset1 %>%
  dplyr::reframe(
    "Disparity index" = disparity_index(MEDI)
  )
#> # A tibble: 1 × 1
#>   `Disparity index`
#>               <dbl>
#> 1             0.675
```

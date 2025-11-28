# Intradaily variability (IV)

This function calculates the variability of consecutive Light levels
within a 24h day. Calculated as the ratio of the variance of the
differences between consecutive Light levels to the total variance
across the day. Calculated with mean hourly Light levels. Higher values
indicate more fragmentation.

## Usage

``` r
intradaily_variability(
  Light.vector,
  Datetime.vector,
  use.samplevar = FALSE,
  na.rm = FALSE,
  as.df = FALSE
)
```

## Arguments

- Light.vector:

  Numeric vector containing the light data.

- Datetime.vector:

  Vector containing the time data. Must be POSIXct.

- use.samplevar:

  Logical. Should the sample variance be used (divide by N-1)? By
  default (`FALSE`), the population variance (divide by N) is used, as
  described in Van Someren et al. (1999).

- na.rm:

  Logical. Should missing values be removed? Defaults to `FALSE`.

- as.df:

  Logical. Should the output be returned as a data frame? If `TRUE`, a
  data frame with a single column named `intradaily_variability` will be
  returned. Defaults to `FALSE`.

## Value

Numeric value or dataframe with column 'IV'.

## References

Van Someren, E. J. W., Swaab, D. F., Colenda, C. C., Cohen, W., McCall,
W. V., & Rosenquist, P. B. (1999). Bright Light Therapy: Improved
Sensitivity to Its Effects on Rest-Activity Rhythms in Alzheimer
Patients by Application of Nonparametric Methods. *Chronobiology
International*, 16(4), 505–518.
[doi:10.3109/07420529908998724](https://doi.org/10.3109/07420529908998724)

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
set.seed(1)
N <- 24 * 2
# Calculate metric for two 24 h days with two measurements per hour
dataset1 <-
  tibble::tibble(
    Id = rep("A", N * 2),
    Datetime = lubridate::as_datetime(0) + c(lubridate::minutes(seq(0, N * 60 - 30, 30))),
    MEDI = sample(1:1000, N * 2)
  )
dataset1 %>%
  dplyr::summarise(
    "Intradaily variability" = intradaily_variability(MEDI, Datetime)
  )
#> # A tibble: 1 × 1
#>   `Intradaily variability`
#>                      <dbl>
#> 1                     1.75
```

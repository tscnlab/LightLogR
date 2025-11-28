# Mean/first/last timing above/below threshold.

This function calculates the mean, first, and last timepoint (MLiT,
FLiT, LLiT) where light levels are above or below a given threshold
intensity within the given time interval.

## Usage

``` r
timing_above_threshold(
  Light.vector,
  Time.vector,
  comparison = c("above", "below"),
  threshold,
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

- comparison:

  String specifying whether the time above or below threshold should be
  calculated. Can be either `"above"` (the default) or `"below"`. If two
  values are provided for `threshold`, this argument will be ignored.

- threshold:

  Single numeric value or two numeric values specifying the threshold
  light level(s) to compare with. If a vector with two values is
  provided, the timing corresponding to light levels between the two
  thresholds will be calculated.

- na.rm:

  Logical. Should missing values be removed for the calculation?
  Defaults to `FALSE`.

- as.df:

  Logical. Should a data frame be returned? If `TRUE`, a data frame with
  three columns (MLiT, FLiT, LLiT) and the threshold (e.g.,
  `MLiT_{threshold}`) will be returned. Defaults to `FALSE`.

## Value

List or dataframe with the three values: `mean`, `first`, and `last`
timing above threshold. The output type corresponds to the type of
`Time.vector`, e.g., if `Time.vector` is HMS, the timing metrics will be
also HMS, and vice versa for POSIXct and numeric.

## References

Reid, K. J., Santostasi, G., Baron, K. G., Wilson, J., Kang, J., & Zee,
P. C. (2014). Timing and Intensity of Light Correlate with Body Weight
in Adults. *PLOS ONE*, 9(4), e92251.
[doi:10.1371/journal.pone.0092251](https://doi.org/10.1371/journal.pone.0092251)

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
[`intradaily_variability()`](https://tscnlab.github.io/LightLogR/reference/intradaily_variability.md),
[`midpointCE()`](https://tscnlab.github.io/LightLogR/reference/midpointCE.md),
[`nvRC()`](https://tscnlab.github.io/LightLogR/reference/nvRC.md),
[`nvRD()`](https://tscnlab.github.io/LightLogR/reference/nvRD.md),
[`nvRD_cumulative_response()`](https://tscnlab.github.io/LightLogR/reference/nvRD_cumulative_response.md),
[`period_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/period_above_threshold.md),
[`pulses_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/pulses_above_threshold.md),
[`threshold_for_duration()`](https://tscnlab.github.io/LightLogR/reference/threshold_for_duration.md)

## Examples

``` r
# Dataset with light > 250lx between 06:00 and 18:00
dataset1 <-
  tibble::tibble(
    Id = rep("A", 24),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )

# Above threshold
dataset1 %>%
  dplyr::reframe(timing_above_threshold(MEDI, Datetime, "above", 250, as.df = TRUE))
#> # A tibble: 1 × 3
#>   mean_timing_above_250 first_timing_above_250 last_timing_above_250
#>   <dttm>                <dttm>                 <dttm>               
#> 1 1970-01-01 12:00:00   1970-01-01 06:00:00    1970-01-01 18:00:00  

# Below threshold
dataset1 %>%
  dplyr::reframe(timing_above_threshold(MEDI, Datetime, "below", 10, as.df = TRUE))
#> # A tibble: 1 × 3
#>   mean_timing_below_10 first_timing_below_10 last_timing_below_10
#>   <dttm>               <dttm>                <dttm>              
#> 1 1970-01-01 10:54:33  1970-01-01 00:00:00   1970-01-01 23:00:00 

# Input = HMS -> Output = HMS
dataset1 %>%
  dplyr::reframe(timing_above_threshold(MEDI, hms::as_hms(Datetime), "above", 250, as.df = TRUE))
#> # A tibble: 1 × 3
#>   mean_timing_above_250 first_timing_above_250 last_timing_above_250
#>   <time>                <time>                 <time>               
#> 1 12:00                 06:00                  18:00                
```

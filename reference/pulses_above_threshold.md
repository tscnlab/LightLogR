# Pulses above threshold

This function clusters the light data into continuous clusters (pulses)
of light above/below a given threshold. Clustering may be fine-tuned by
setting the minimum length of the clusters and by allowing brief
interruptions to be included in a single cluster, with a specified
maximum length of interruption episodes and proportion of total amount
of interruptions to light above threshold.

## Usage

``` r
pulses_above_threshold(
  Light.vector,
  Time.vector,
  comparison = c("above", "below"),
  threshold,
  min.length = "2 mins",
  max.interrupt = "8 mins",
  prop.interrupt = 0.25,
  epoch = "dominant.epoch",
  return.indices = FALSE,
  na.rm = FALSE,
  as.df = FALSE
)
```

## Arguments

- Light.vector:

  Numeric vector containing the light data. Missing values will be
  considered as `FALSE` when comparing light levels against the
  threshold.

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

- min.length:

  The minimum length of a pulse. Can be either a
  [duration](https://lubridate.tidyverse.org/reference/duration.html) or
  a string. If it is a string, it needs to be a valid
  [duration](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`. Defaults to `"2 mins"` as in
  Wilson et al. (2018).

- max.interrupt:

  Maximum length of each episode of interruptions. Can be either a
  [duration](https://lubridate.tidyverse.org/reference/duration.html) or
  a string. If it is a string, it needs to be a valid
  [duration](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`. Defaults to `"8 mins"` as in
  Wilson et al. (2018).

- prop.interrupt:

  Numeric value between `0` and `1` specifying the maximum proportion of
  the total number of interruptions. Defaults to `0.25` as in Wilson et
  al. (2018).

- epoch:

  The epoch at which the data was sampled. Can be either a
  [duration](https://lubridate.tidyverse.org/reference/duration.html) or
  a string. If it is a string, it needs to be either `"dominant.epoch"`
  (the default) for a guess based on the data, or a valid
  [duration](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

- return.indices:

  Logical. Should the cluster indices be returned? Only works if `as.df`
  is `FALSE`. Defaults to `FALSE`.

- na.rm:

  Logical. Should missing values be removed for the calculation of pulse
  metrics? Defaults to `FALSE`.

- as.df:

  Logical. Should a data frame be returned? If `TRUE`, a data frame with
  seven columns ("n", "mean_level", "mean_duration", "total_duration",
  "mean_onset", "mean_midpoint", "mean_offset") and the threshold (e.g.,
  `_{threshold}`) will be returned. Defaults to `FALSE`.

## Value

List or data frame with calculated values.

## Details

The timeseries is assumed to be regular. Missing values in the light
data will be replaced by 0.

## References

Wilson, J., Reid, K. J., Braun, R. I., Abbott, S. M., & Zee, P. C.
(2018). Habitual light exposure relative to circadian timing in delayed
sleep-wake phase disorder. *Sleep*, 41(11).
[doi:10.1093/sleep/zsy166](https://doi.org/10.1093/sleep/zsy166)

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
[`threshold_for_duration()`](https://tscnlab.github.io/LightLogR/reference/threshold_for_duration.md),
[`timing_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/timing_above_threshold.md)

## Examples

``` r
# Sample data
data = sample.data.environment %>%
  dplyr::filter(Id == "Participant") %>%
  filter_Datetime(length = lubridate::days(1)) %>% 
  dplyr::mutate(
    Time = hms::as_hms(Datetime),
  )

# Time vector as datetime
data %>%
  dplyr::reframe(pulses_above_threshold(MEDI, Datetime, threshold = 250, as.df = TRUE))
#> # A tibble: 1 × 8
#>   Id          n_pulses_above_250 mean_level_pulses_abov…¹ mean_duration_pulses…²
#>   <fct>                    <int>                    <dbl> <Duration>            
#> 1 Participant                  5                     717. 1044s (~17.4 minutes) 
#> # ℹ abbreviated names: ¹​mean_level_pulses_above_250,
#> #   ²​mean_duration_pulses_above_250
#> # ℹ 4 more variables: total_duration_pulses_above_250 <Duration>,
#> #   mean_onset_pulses_above_250 <dttm>, mean_midpoint_pulses_above_250 <dttm>,
#> #   mean_offset_pulses_above_250 <dttm>

# Time vector as hms time
data %>%
  dplyr::reframe(pulses_above_threshold(MEDI, Time, threshold = 250, as.df = TRUE))
#> # A tibble: 1 × 8
#>   Id          n_pulses_above_250 mean_level_pulses_abov…¹ mean_duration_pulses…²
#>   <fct>                    <int>                    <dbl> <Duration>            
#> 1 Participant                  5                     717. 1044s (~17.4 minutes) 
#> # ℹ abbreviated names: ¹​mean_level_pulses_above_250,
#> #   ²​mean_duration_pulses_above_250
#> # ℹ 4 more variables: total_duration_pulses_above_250 <Duration>,
#> #   mean_onset_pulses_above_250 <time>, mean_midpoint_pulses_above_250 <time>,
#> #   mean_offset_pulses_above_250 <time>

# Pulses below threshold 
data %>%
  dplyr::reframe(pulses_above_threshold(MEDI, Datetime, "below", threshold = 250, as.df = TRUE))
#> # A tibble: 1 × 8
#>   Id          n_pulses_below_250 mean_level_pulses_belo…¹ mean_duration_pulses…²
#>   <fct>                    <int>                    <dbl> <Duration>            
#> 1 Participant                  5                     40.2 16062s (~4.46 hours)  
#> # ℹ abbreviated names: ¹​mean_level_pulses_below_250,
#> #   ²​mean_duration_pulses_below_250
#> # ℹ 4 more variables: total_duration_pulses_below_250 <Duration>,
#> #   mean_onset_pulses_below_250 <dttm>, mean_midpoint_pulses_below_250 <dttm>,
#> #   mean_offset_pulses_below_250 <dttm>

# Pulses within threshold range
data %>%
  dplyr::reframe(pulses_above_threshold(MEDI, Datetime, threshold = c(250,1000), as.df = TRUE))
#> # A tibble: 1 × 8
#>   Id        n_pulses_within_250-…¹ mean_level_pulses_wi…² mean_duration_pulses…³
#>   <fct>                      <int>                  <dbl> <Duration>            
#> 1 Particip…                      4                   491. 452s (~7.53 minutes)  
#> # ℹ abbreviated names: ¹​`n_pulses_within_250-1000`,
#> #   ²​`mean_level_pulses_within_250-1000`,
#> #   ³​`mean_duration_pulses_within_250-1000`
#> # ℹ 4 more variables: `total_duration_pulses_within_250-1000` <Duration>,
#> #   `mean_onset_pulses_within_250-1000` <dttm>,
#> #   `mean_midpoint_pulses_within_250-1000` <dttm>,
#> #   `mean_offset_pulses_within_250-1000` <dttm>
```

# Duration above/below threshold or within threshold range

This function calculates the duration spent above/below a specified
threshold light level or within a specified range of light levels.

## Usage

``` r
duration_above_threshold(
  Light.vector,
  Time.vector,
  comparison = c("above", "below"),
  threshold,
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

- comparison:

  String specifying whether the time above or below threshold should be
  calculated. Can be either `"above"` (the default) or `"below"`. If two
  values are provided for `threshold`, this argument will be ignored.

- threshold:

  Single numeric value or two numeric values specifying the threshold
  light level(s) to compare with. If a vector with two values is
  provided, the time within the two thresholds will be calculated.

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
  with a single column named `duration_{comparison}_{threshold}` will be
  returned. Defaults to `FALSE`.

## Value

A [duration](https://lubridate.tidyverse.org/reference/duration.html)
object as single value, or single column data frame.

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
[`dose()`](https://tscnlab.github.io/LightLogR/reference/dose.md),
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
N <- 60
# Dataset with epoch = 1min
dataset1 <-
  tibble::tibble(
    Id = rep("A", N),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(1:N),
    MEDI = sample(c(sample(1:249, N / 2), sample(250:1000, N / 2))),
  )
# Dataset with epoch = 30s
dataset2 <-
  tibble::tibble(
    Id = rep("B", N),
    Datetime = lubridate::as_datetime(0) + lubridate::seconds(seq(30, N * 30, 30)),
    MEDI = sample(c(sample(1:249, N / 2), sample(250:1000, N / 2))),
  )
dataset.combined <- rbind(dataset1, dataset2)

dataset1 %>%
  dplyr::reframe("TAT >250lx" = duration_above_threshold(MEDI, Datetime, threshold = 250))
#> # A tibble: 1 × 1
#>   `TAT >250lx`       
#>   <Duration>         
#> 1 1800s (~30 minutes)

dataset1 %>%
  dplyr::reframe(duration_above_threshold(MEDI, Datetime, threshold = 250, as.df = TRUE))
#> # A tibble: 1 × 1
#>   duration_above_250 
#>   <Duration>         
#> 1 1800s (~30 minutes)

# Group by Id to account for different epochs
dataset.combined %>%
  dplyr::group_by(Id) %>%
  dplyr::reframe("TAT >250lx" = duration_above_threshold(MEDI, Datetime, threshold = 250))
#> # A tibble: 2 × 2
#>   Id    `TAT >250lx`       
#>   <chr> <Duration>         
#> 1 A     1800s (~30 minutes)
#> 2 B     900s (~15 minutes) 
```

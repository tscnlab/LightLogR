# Circadian lighting metrics from Barroso et al. (2014)

This function calculates the metrics proposed by Barroso et al. (2014)
for light-dosimetry in the context of research on the non-visual effects
of light. The following metrics are calculated:

## Usage

``` r
barroso_lighting_metrics(
  Light.vector,
  Time.vector,
  epoch = "dominant.epoch",
  loop = FALSE,
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

- loop:

  Logical. Should the data be looped? Defaults to `FALSE`.

- na.rm:

  Logical. Should missing values (NA) be removed for the calculation?
  Defaults to `FALSE`. If `TRUE`, for the calculation of
  `bright_cluster` and `dark_cluster`, missing values will be replaced
  by 0 (see
  [`period_above_threshold`](https://tscnlab.github.io/LightLogR/reference/period_above_threshold.md)).

- as.df:

  Logical. Should a data frame be returned? If `TRUE`, a data frame with
  seven columns will be returned. Defaults to `FALSE`.

## Value

List or dataframe with the seven values: `bright_threshold`,
`dark_threshold`, `bright_mean_level`, `dark_mean_level`,
`bright_cluster`, `dark_cluster`, `circadian_variation`. The output type
of `bright_cluster`, `dark_cluster`, is a
[duration](https://lubridate.tidyverse.org/reference/duration.html)
object.

## Details

- `bright_threshold`:

  The maximum light intensity for which at least six hours of
  measurements are at the same or higher level.

- `dark_threshold`:

  The minimum light intensity for which at least eight hours of
  measurements are at the same or lower level.

- `bright_mean_level`:

  The 20% trimmed mean of all light intensity measurements equal or
  above the `bright_threshold`.

- `dark_mean_level`:

  The 20% trimmed mean of all light intensity measurements equal or
  below the `dark_threshold`.

- `bright_cluster`:

  The longest continuous time interval above the `bright_threshold`.

- `dark_cluster`:

  The longest continuous time interval below the `dark_threshold`.

- `circadian_variation`:

  A measure of periodicity of the daily lighting schedule over a given
  set of days. Calculated as the coefficient of variation of input light
  data.

## References

Barroso, A., Simons, K., & Jager, P. de. (2014). Metrics of circadian
lighting for clinical investigations. *Lighting Research & Technology*,
46(6), 637–649.
[doi:10.1177/1477153513502664](https://doi.org/10.1177/1477153513502664)

Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for
light-dosimetry studies: Quantification metrics. *Lighting Research &
Technology*.
[doi:10.1177/14771535231170500](https://doi.org/10.1177/14771535231170500)

## Examples

``` r
dataset1 <-
  tibble::tibble(
    Id = rep("B", 60 * 24),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*24-1)),
    MEDI = c(rep(sample(seq(0,1,0.1), 60*8, replace = TRUE)), 
             rep(sample(1:1000, 16, replace = TRUE), each = 60))
  )

dataset1 %>%
  dplyr::reframe(barroso_lighting_metrics(MEDI, Datetime, as.df = TRUE))
#> # A tibble: 1 × 7
#>   bright_threshold dark_threshold bright_mean_level dark_mean_level
#>              <dbl>          <dbl>             <dbl>           <dbl>
#> 1              509              1              734.           0.501
#> # ℹ 3 more variables: bright_cluster <Duration>, dark_cluster <Duration>,
#> #   circadian_variation <dbl>
  
```

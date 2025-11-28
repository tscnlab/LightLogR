# Performance metrics for circadian response

These functions compare the non-visual circadian response (see
[`nvRC`](https://tscnlab.github.io/LightLogR/reference/nvRC.md)) for
measured personal light exposure to the nvRC for a reference light
exposure pattern, such as daylight.

## Usage

``` r
nvRC_circadianDisturbance(nvRC, nvRC.ref, as.df = FALSE)

nvRC_circadianBias(nvRC, nvRC.ref, as.df = FALSE)

nvRC_relativeAmplitudeError(nvRC, nvRC.ref, as.df = FALSE)
```

## Arguments

- nvRC:

  Time series of non-visual circadian response (see
  [`nvRC`](https://tscnlab.github.io/LightLogR/reference/nvRC.md).

- nvRC.ref:

  Time series of non-visual circadian response circadian response (see
  [`nvRC`](https://tscnlab.github.io/LightLogR/reference/nvRC.md) for a
  reference light exposure pattern (e.g., daylight). Must be the same
  length as `nvRC`.

- as.df:

  Logical. Should the output be returned as a data frame? Defaults to
  TRUE.

## Value

A numeric value or single column data frame.

## Details

`nvRC_circadianDisturbance()` calculates the circadian disturbance (CD).
It is expressed as

\$\$CD(i,T)=\frac{1}{T}\int\_{t\_{i}}^{t\_{i}+T} {\lvert
r\_{C}(t)-r\_{C}^{ref}(t)\rvert dt},\$\$

and quantifies the total difference between the measured circadian
response and the circadian response to a reference profile.

`nvRC_circadianBias()` calculates the circadian bias (CB). It is
expressed as

\$\$CB(i,T)=\frac{1}{T}\int\_{t\_{i}}^{t\_{i}+T}
{(r\_{C}(t)-r\_{C}^{ref}(t))dt},\$\$

and provides a measure of the overall trend for the difference in
circadian response, i.e. positive values for overestimating and negative
for underestimating between the measured circadian response and the
circadian response to a reference profile.

`nvRC_relativeAmplitudeError()` calculates the relative amplitude error
(RAE). It is expressed as

\$\$RAE(i,T)=r\_{C,max}-r\_{C,max}^{ref},\$\$

and quantifies the difference between the maximum response achieved in a
period to the reference signal.

## References

Amundadottir, M.L. (2016). Light-driven model for identifying indicators
of non-visual health potential in the built environment \[Doctoral
dissertation, EPFL\]. EPFL infoscience.
[doi:10.5075/epfl-thesis-7146](https://doi.org/10.5075/epfl-thesis-7146)

## Examples

``` r
dataset1 <- 
  tibble::tibble(
    Id = rep("B", 60 * 24),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*24-1)),
    Illuminance = c(rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60)),
    MEDI = Illuminance * rep(sample(0.5:1.5, 24, replace = TRUE), each = 60),
  ) %>%
  dplyr::mutate(
    nvRC = nvRC(MEDI, Illuminance, Datetime, sleep.onset = hms::as_hms("22:00:00"))
  )

dataset.reference <-
  tibble::tibble(
    Id = rep("Daylight", 60 * 24),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*24-1)),
    Illuminance = c(rep(0, 60*6), rep(10000, 12*60), rep(0, 60*6)),
    MEDI = Illuminance
  ) %>%
  dplyr::mutate(
    nvRC = nvRC(MEDI, Illuminance, Datetime, sleep.onset = hms::as_hms("22:00:00"))
  )

# Circadian disturbance
nvRC_circadianDisturbance(dataset1$nvRC, dataset.reference$nvRC)
#> [1] 0.2416101

# Circadian bias
nvRC_circadianBias(dataset1$nvRC, dataset.reference$nvRC)
#> [1] -0.0315973

# Relative amplitude error
nvRC_relativeAmplitudeError(dataset1$nvRC, dataset.reference$nvRC)
#> [1] 0.2158102
```

# Reconstruct spectral irradiance from sensor counts

This function takes sensor data in the form of (normalized) counts and
reconstructs a spectral power distribution (SPD) through a calibration
matrix. The matrix takes the form of `sensor channel x wavelength`, and
the spectrum results form a linear combination of
`counts x calibration-value` for any wavelength in the matrix. Handles
multiple sensor readings by returning a list of spectra

## Usage

``` r
spectral_reconstruction(
  sensor_channels,
  calibration_matrix,
  format = c("long", "wide")
)
```

## Arguments

- sensor_channels:

  Named numeric vector or dataframe with sensor readings. Names must
  match calibration matrix columns.

- calibration_matrix:

  Matrix or dataframe with sensor-named columns and wavelength-indexed
  rows

- format:

  Output format: "long" (list of tibbles) or "wide" (dataframe)

## Value

- "long": List of tibbles (wavelength, irradiance)

- "wide": Dataframe with wavelength columns and one row per spectrum

## Details

Please note that calibration matrices are not provided by LightLogR, but
can be provided by a wearable device manufacturer. Counts can be
normalized with the
[`normalize_counts()`](https://tscnlab.github.io/LightLogR/reference/normalize_counts.md)
function, provided that the output also contains a `gain` column.

## See also

Other Spectrum:
[`normalize_counts()`](https://tscnlab.github.io/LightLogR/reference/normalize_counts.md),
[`spectral_integration()`](https://tscnlab.github.io/LightLogR/reference/spectral_integration.md)

## Examples

``` r
# Calibration matrix example
calib <- matrix(1:12, ncol=3, dimnames = list(400:403, c("R", "G", "B")))

# Named vector input
spectral_reconstruction(c(R=1, G=2, B=3), calib)
#> # A tibble: 4 × 2
#>   wavelength irradiance
#>        <dbl>      <dbl>
#> 1        400         38
#> 2        401         44
#> 3        402         50
#> 4        403         56

# Dataframe input
df <- data.frame(R=1, G=2, B=3, other_col=10)
spectral_reconstruction(dplyr::select(df, R:B), calib)
#> # A tibble: 4 × 2
#>   wavelength irradiance
#>        <dbl>      <dbl>
#> 1        400         38
#> 2        401         44
#> 3        402         50
#> 4        403         56

# Multiple spectra: as list columns
df <- data.frame(Measurement = c(1,2), R=c(1,2), G=c(2,4), B=c(3,6))
df <- 
df |> 
  dplyr::mutate(
      Spectrum = spectral_reconstruction(dplyr::pick(R:B), calib)
      )
df |> tidyr::unnest(Spectrum)
#> # A tibble: 8 × 6
#>   Measurement     R     G     B wavelength irradiance
#>         <dbl> <dbl> <dbl> <dbl>      <dbl>      <dbl>
#> 1           1     1     2     3        400         38
#> 2           1     1     2     3        401         44
#> 3           1     1     2     3        402         50
#> 4           1     1     2     3        403         56
#> 5           2     2     4     6        400         76
#> 6           2     2     4     6        401         88
#> 7           2     2     4     6        402        100
#> 8           2     2     4     6        403        112

# Multiple spectra: as extended dataframes
df |> 
  dplyr::mutate(
      Spectrum = spectral_reconstruction(dplyr::pick(R:B), calib, "wide"))
#>   Measurement R G B Spectrum.400 Spectrum.401 Spectrum.402 Spectrum.403
#> 1           1 1 2 3           38           44           50           56
#> 2           2 2 4 6           76           88          100          112
```

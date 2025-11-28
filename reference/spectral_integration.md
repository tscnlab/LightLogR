# Integrate spectral irradiance with optional weighting

Integrates over a given spectrum, optionally over only a portion of the
spectrum, optionally with a weighing function. Can be used to calculate
spectral contributions in certain wavelength ranges, or to calculate
(alphaopically equivalent daylight) illuminance.

## Usage

``` r
spectral_integration(
  spectrum,
  wavelength.range = NULL,
  action.spectrum = NULL,
  general.weight = 1
)
```

## Arguments

- spectrum:

  Tibble with spectral data (1st col: wavelength, 2nd col: SPD values)

- wavelength.range:

  Optional integration bounds (length-2 numeric)

- action.spectrum:

  Either:

  - Tibble with wavelength and weighting columns

  - Name of built-in spectrum: "photopic", "melanopic", "rhodopic",
    "l_cone_opic", "m_cone_opic", "s_cone_opic"

- general.weight:

  Scalar multiplier or "auto" for built-in efficacies

## Value

Numeric integrated value

## Details

The function uses trapezoidal integration and recognizes differing
step-widths in the spectrum. If an action spectrum is used, values of
the action spectrum at the spectral wavelenghts are interpolated with
[`stats::approx()`](https://rdrr.io/r/stats/approxfun.html).

The used efficacies for for the auto-weighting are:

- photopic: 683.0015478

- melanopic: 1/0.0013262

- rhodopic: 1/0.0014497

- l_cone_opic: 1/0.0016289

- m_cone_opic: 1/0.0014558

- s_cone_opic: 1/0.0008173

This requires input values in W/(m^2) for the spectrum. If it is
provided in other units, the result has to be rescaled afterwards.

## See also

Other Spectrum:
[`normalize_counts()`](https://tscnlab.github.io/LightLogR/reference/normalize_counts.md),
[`spectral_reconstruction()`](https://tscnlab.github.io/LightLogR/reference/spectral_reconstruction.md)

## Examples

``` r
# creating an equal energy spectrum of value 1
spd <- data.frame(wl = 380:780, values = 1)

#integrating over the full spectrum
spectral_integration(spd)
#> [1] 400

#integrating over wavelengths 400-500 nm
spectral_integration(spd, wavelength.range = c(400, 500))
#> [1] 100

#calculating the photopic illuminance of an equal energy spectrum with 1 W/(m^2*nm)
spectral_integration(spd, action.spectrum = "photopic", general.weight = "auto")
#> [1] 72983.09

#calculating the melanopic EDI of an equal energy spectrum with 1 W/(m^2*nm)
spectral_integration(spd, action.spectrum = "melanopic", general.weight = "auto")
#> [1] 66109.89

# Custom action spectrum
custom_act <- data.frame(wavelength = 400:700, weight = 0.5)
spectral_integration(spd, wavelength.range = c(400,700), 
                     action.spectrum = custom_act, general.weight = 2)
#> [1] 300
                     
#using a spectrum that is broader then the action spectrum will not change the
#output, as the action spectrum will use zeros beyond its range

```

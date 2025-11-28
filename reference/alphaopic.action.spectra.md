# Alphaopic (+ photopic) action spectra

A dataframe of alphaopic action spectra plus the photopic action
spectrum. The alphaopic action spectra are according to the [CIE S
026/E:2018](https://www.cie.co.at/publications/cie-system-metrology-optical-radiation-iprgc-influenced-responses-light-0)
standard. The alphaopic action spectra are for a 32-year-old standard
observer. The photopic action spectrum is for a 2° standard observer.

## Usage

``` r
alphaopic.action.spectra
```

## Format

`alphaopic.action.spectra` A datafram with 471 rows and 7 columns:

- wavelength:

  integer of wavelength, from 360 to 830 nm. Unit is nm

- melanopic:

  numeric melanopic action spectrum

- l_cone_opic:

  numeric L-cone opic action spectrum

- m_cone_opic:

  numeric M-cone opic action spectrum

- s_cone_opic:

  numeric S-cone opic action spectrum

- rhodopic:

  numeric rhodopic action spectrum

- photopic:

  numeric photopic action spectrum

## Source

<https://www.cie.co.at/publications/cie-system-metrology-optical-radiation-iprgc-influenced-responses-light-0>

<https://cie.co.at/datatable/cie-spectral-luminous-efficiency-photopic-vision>

\<https://files.cie.co.at/CIE S 026 alpha-opic Toolbox.xlsx\>

## References

CIE (2019). ISO/CIE 11664-1:2019(E). Colorimetry — Part 1: CIE standard
colorimetric observers. Vienna, CIE

CIE (2018). CIE S 026/E:2018. CIE system for metrology of optical
radiation for ipRGC-influenced responses of light. Vienna, CIE

# Get all the supported devices in LightLogR

Returns a vector of all the supported devices in LightLogR.

## Usage

``` r
supported_devices()
```

## Value

A character vector of all supported devices

## Details

These are all supported devices where there is a dedicated import
function. Import functions can be called either through
[`import_Dataset()`](https://tscnlab.github.io/LightLogR/reference/import_Dataset.md)
with the respective `device = "device"` argument, or directly, e.g.,
`import$ActLumus()`.

## See also

[import_Dataset](https://tscnlab.github.io/LightLogR/reference/import_Dataset.md)

## Examples

``` r
supported_devices()
#>  [1] "ActLumus"           "ActTrust"           "Actiwatch_Spectrum"
#>  [4] "Circadian_Eye"      "Clouclip"           "DeLux"             
#>  [7] "GENEActiv_GGIR"     "Kronowise"          "LIMO"              
#> [10] "LYS"                "LiDo"               "LightWatcher"      
#> [13] "MiEye"              "MotionWatch8"       "OcuWEAR"           
#> [16] "Speccy"             "SpectraWear"        "VEET"              
#> [19] "nanoLambda"        
```

# Import a light logger dataset or related data

Imports a dataset and does the necessary transformations to get the
right column formats. Unless specified otherwise, the function will set
the timezone of the data to `UTC`. It will also enforce an `Id` to
separate different datasets and will order/arrange the dataset within
each `Id` by Datetime. See the Details and Devices section for more
information and the full list of arguments.

## Usage

``` r
import_Dataset(device, ...)

import
```

## Format

An object of class `list` of length 19.

## Arguments

- device:

  From what device do you want to import? For a few devices, there is a
  sample data file that you can use to test the function (see the
  examples). See
  [`supported_devices()`](https://tscnlab.github.io/LightLogR/reference/supported_devices.md)
  for a list of supported devices and see below for more information on
  devices with specific requirements.

- ...:

  Parameters that get handed down to the specific import functions

## Value

Tibble/Dataframe with a POSIXct column for the datetime

## Details

There are specific and a general import function. The general import
function is described below, whereas the specific import functions take
the form of `import$device()`. The general import function is a thin
wrapper around the specific import functions. The specific import
functions take the following arguments:

- `filename`: Filename(s) for the Dataset. Can also contain the
  filepath, but `path` must then be `NULL`. Expects a `character`. If
  the vector is longer than `1`, multiple files will be read in into one
  Tibble.

- `path`: Optional path for the dataset(s). `NULL` is the default.
  Expects a `character`.

- `n_max`: maximum number of lines to read. Default is `Inf`.

- `tz`: Timezone of the data. `"UTC"` is the default. Expects a
  `character`. You can look up the supported timezones with
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html).

- `version`: Data formats can change, e.g. with software updates. This
  argument allows switching between known data formats of the same
  device model. Expects a `character` scalar. The default is
  `"default"`, which will always use the latest version. To find out
  which software versions are contained, call
  [`supported_versions()`](https://tscnlab.github.io/LightLogR/reference/supported_versions.md).

- `Id.colname`: Lets you specify a column for the id of a dataset.
  Expects a symbol (Default is `Id`). This column will be used for
  grouping
  ([`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)).

- `auto.id`: If the `Id.colname` column is not part of the `dataset`,
  the `Id` can be automatically extracted from the filename. The
  argument expects a regular expression
  [regex](https://rdrr.io/r/base/regex.html) and will by default just
  give the whole filename without file extension.

- `manual.id`: If this argument is not `NULL`, and no `Id` column is
  part of the `dataset`, this `character` scalar will be used. **We
  discourage the use of this arguments when importing more than one
  file**

- `silent`: If set to `TRUE`, the function will not print a summary
  message of the import or plot the overview. Default is `FALSE`.

- `locale`: The locale controls defaults that vary from place to place.

- `not.before`: Remove data prior to this date. This argument is
  provided to `start` of
  [`filter_Date()`](https://tscnlab.github.io/LightLogR/reference/filter_Datetime.md).
  Data will be filtered out before any of the summaries are shown.

- `dst_adjustment`: If a file crosses daylight savings time, but the
  device does not adjust time stamps accordingly, you can set this
  argument to `TRUE`, to apply this shift manually. It is selective, so
  it will only be done in files that cross between DST and standard
  time. Default is `FALSE`. Uses
  [`dst_change_handler()`](https://tscnlab.github.io/LightLogR/reference/dst_change_handler.md)
  to do the adjustment. Look there for more infos. It is not equipped to
  handle two jumps in one file (so back and forth between DST and
  standard time), but will work fine if jums occur in separate files.

- `auto.plot`: a logical on whether to call
  [`gg_overview()`](https://tscnlab.github.io/LightLogR/reference/gg_overview.md)
  after import. Default is `TRUE`. But is set to `FALSE` if the argument
  `silent` is set to `TRUE`.

- `...`: supply additional arguments to the readr import functions, like
  `na`. Might also be used to supply arguments to the specific import
  functions, like `column_names` for `Actiwatch_Spectrum` devices. Those
  devices will always throw a helpful error message if you forget to
  supply the necessary arguments. If the `Id` column is already part of
  the `dataset` it will just use this column. If the column is not
  present it will add this column and fill it with the filename of the
  importfile (see param `auto.id`).

- `print_n` can be used if you want to see more rows from the
  observation intervals

- `remove_duplicates` can be used if identical observations are present
  within or across multiple files. The default is `FALSE`. The function
  keeps only unique observations (=rows) if set to' TRUE'. This is a
  convenience implementation of
  [`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html).

## Devices

The set of import functions provide a convenient way to import light
logger data that is then perfectly formatted to add metadata, make
visualizations and analyses. There are a number of devices supported,
where import should just work out of the box. To get an overview, you
can simply call the
[`supported_devices()`](https://tscnlab.github.io/LightLogR/reference/supported_devices.md)
dataset. The list will grow continuously as the package is maintained.
More than one data formats may be available for a given device. Check
with
[`supported_versions()`](https://tscnlab.github.io/LightLogR/reference/supported_versions.md)
if you run into problems with imports, despite a correct device setting.

    supported_devices()
    #>  [1] "ActLumus"           "ActTrust"           "Actiwatch_Spectrum"
    #>  [4] "Circadian_Eye"      "Clouclip"           "DeLux"
    #>  [7] "GENEActiv_GGIR"     "Kronowise"          "LIMO"
    #> [10] "LYS"                "LiDo"               "LightWatcher"
    #> [13] "MiEye"              "MotionWatch8"       "OcuWEAR"
    #> [16] "Speccy"             "SpectraWear"        "VEET"
    #> [19] "nanoLambda"

### ActLumus

Manufacturer: Condor Instruments

Model: ActLumus

Implemented: Sep 2023

A sample file is provided with the package, it can be accessed through
`system.file("extdata/205_actlumus_Log_1020_20230904101707532.txt.zip", package = "LightLogR")`.
It does not need to be unzipped to be imported. This sample file is a
good example for a regular dataset without gaps.

### LYS

Manufacturer: LYS Technologies

Model: LYS Button

Implemented: Sep 2023

A sample file is provided with the package, it can be accessed through
`sample.data.irregular`. This sample file is a good example for an
irregular dataset.

### Actiwatch_Spectrum & Actiwatch_Spectrum_de

Manufacturer: Philips Respironics

Model: Actiwatch Spectrum

Implemented: Nov 2023 / July 2024

### ActTrust

Manufacturer: Condor Instruments

Model: ActTrust1, ActTrust2

Implemented: Mar 2024

This function works for both ActTrust 1 and 2 devices

### Speccy

Manufacturer: Monash University

Model: Speccy

Implemented: Feb 2024

### DeLux

Manufacturer: Intelligent Automation Inc

Model: DeLux

Implemented: Dec 2023

### LiDo

Manufacturer: University of Lucerne

Model: LiDo

Implemented: Nov 2023

### SpectraWear

Manufacturer: University of Manchester

Model: SpectraWear

Implemented: May 2024

### NanoLambda

Manufacturer: NanoLambda

Model: XL-500 BLE

Implemented: May 2024

### LightWatcher

Manufacturer: Object-Tracker

Model: LightWatcher

Implemented: June 2024

### VEET

Manufacturer: Meta Reality Labs

Model: VEET

Implemented: July 2024

**Required Argument: `modality`** A character scalar describing the
modality to be imported from. Can be one of `"ALS"` (Ambient light
sensor), `"IMU"` (Inertial Measurement Unit), `"INF"` (Information),
`"PHO"` (Spectral Sensor), `"TOF"` (Time of Flight)

### Circadian_Eye

Manufacturer: Max-Planck-Institute for Biological Cybernetics, Tübingen

Model: melanopiQ Circadian Eye (Prototype)

Implemented: July 2024

### Kronowise

Manufacturer: Kronohealth

Model: Kronowise

Implemented: July 2024

### GENEActiv with GGIR preprocessing

Manufacturer: Activeinsights

Model: GENEActiv

**Note:** This import function takes GENEActiv data that was
preprocessed through the [GGIR](https://cran.r-project.org/package=GGIR)
package. By default, `GGIR` aggregates light data into intervals of 15
minutes. This can be set by the `windowsizes` argument in GGIR, which is
a three-value vector, where the second values is set to 900 seconds by
default. To import the preprocessed data with `LightLogR`, the
`filename` argument requires a path to the parent directory of the GGIR
output folders, specifically the `meta` folder, which contains the light
exposure data. Multiple `filename`s can be specified, each of which
needs to be a path to a different GGIR parent directory. GGIR exports
can contain data from multiple participants, these will always be
imported fully by providing the parent directory. Use the `pattern`
argument to extract sensible `Id`s from the *.RData* filenames within
the *meta/basic/* folder. As per the author, [Dr. Vincent van
Hees](https://www.accelting.com), GGIR preprocessed data are always in
local time, provided the `desiredtz`/`configtz` are properly set in
GGIR. `LightLogR` still requires a timezone to be set, but will not
timeshift the import data.

### MotionWatch 8

Manufacturer: CamNtech

Implemented: September 2024

### LIMO

Manufacturer: ENTPE

Implemented: September 2024

LIMO exports `LIGHT` data and `IMU` (inertia measurements, also UV) in
separate files. Both can be read in with this function, but not at the
same time. Please decide what type of data you need and provide the
respective filenames.

### OcuWEAR

Manufacturer: Ocutune

Implemented: September 2024

OcuWEAR data contains spectral data. Due to the format of the data file,
the spectrum is not directly part of the tibble, but rather a list
column of tibbles within the imported data, containing a `Wavelength`
(nm) and `Intensity` (mW/m^2) column.

### Clouclip

Manufacturer: Clouclip

Implemented: April 2025

Clouclip export files have the ending `.xls`, but are no real Microsoft
Excel files, rather they are tab-separated text files. LightLogR thus
does not read them in with an excel import routine. The measurement
columns `Lux` and `Dis` contain sentinel values. `-1` (`Dis` and `Lux`)
indicates sleep mode, whereas `204` (only `Dis`) indicates an out of
range measurement. These values will be set to `NA`, and an additional
column is added that translates these status codes. The columns carry
the name `{.col}_status`.

### MiEye

Manufacturer: CHI. Circadian Health Innovations

Implemented: October 2025

## Examples

### Imports made easy

To import a file, simple specify the filename (and path) and feed it to
the `import_Dataset` function. There are sample datasets for all
devices.

The import functions provide a basic overview of the data after import,
such as the intervals between measurements or the start and end dates.

    filepath <- system.file("extdata/205_actlumus_Log_1020_20230904101707532.txt.zip", package = "LightLogR")
    dataset <- import_Dataset("ActLumus", filepath, auto.plot = FALSE)
    #> Multiple files in zip: reading '205_actlumus_Log_1020_20230904101707532.txt'
    #>
    #> Successfully read in 61'016 observations across 1 Ids from 1 ActLumus-file(s).
    #> Timezone set is UTC.
    #> The system timezone is Europe/Berlin. Please correct if necessary!
    #>
    #> First Observation: 2023-08-28 08:47:54
    #> Last Observation: 2023-09-04 10:17:04
    #> Timespan: 7.1 days
    #>
    #> Observation intervals:
    #>   Id                                          interval.time     n pct
    #> 1 205_actlumus_Log_1020_20230904101707532.txt 10s           61015 100%

Import functions can also be called directly:

    dataset <- import$ActLumus(filepath, auto.plot = FALSE)
    #> Multiple files in zip: reading '205_actlumus_Log_1020_20230904101707532.txt'
    #>
    #> Successfully read in 61'016 observations across 1 Ids from 1 ActLumus-file(s).
    #> Timezone set is UTC.
    #> The system timezone is Europe/Berlin. Please correct if necessary!
    #>
    #> First Observation: 2023-08-28 08:47:54
    #> Last Observation: 2023-09-04 10:17:04
    #> Timespan: 7.1 days
    #>
    #> Observation intervals:
    #>   Id                                          interval.time     n pct
    #> 1 205_actlumus_Log_1020_20230904101707532.txt 10s           61015 100%

    dataset %>%
    dplyr::select(Datetime, TEMPERATURE, LIGHT, MEDI, Id) %>%
    dplyr::slice(1500:1505)
    #> # A tibble: 6 x 5
    #> # Groups:   Id [1]
    #>   Datetime            TEMPERATURE LIGHT  MEDI Id
    #>   <dttm>                    <dbl> <dbl> <dbl> <fct>
    #> 1 2023-08-28 12:57:44        26.9  212.  202. 205_actlumus_Log_1020_20230904101~
    #> 2 2023-08-28 12:57:54        26.9  208.  199. 205_actlumus_Log_1020_20230904101~
    #> 3 2023-08-28 12:58:04        26.9  205.  196. 205_actlumus_Log_1020_20230904101~
    #> 4 2023-08-28 12:58:14        26.8  204.  194. 205_actlumus_Log_1020_20230904101~
    #> 5 2023-08-28 12:58:24        26.9  203.  194. 205_actlumus_Log_1020_20230904101~
    #> 6 2023-08-28 12:58:34        26.8  204.  195. 205_actlumus_Log_1020_20230904101~

## See also

[supported_devices](https://tscnlab.github.io/LightLogR/reference/supported_devices.md)

# Gain / Gain-ratio tables to normalize counts

A list of tables containing gain and gain-ratios to normalize counts
across different sensor gains.

## Usage

``` r
gain.ratio.tables
```

## Format

`gain.ratio.tables` A list containing two-column tibbles

- TSL2585:

  gain table for the ambient light sensor
  [TSL2585](https://look.ams-osram.com/m/7899f3742d5a3f00/original/TSL2585-Miniature-Ambient-Light-Sensor-with-UV-and-Light-Flicker-Detection.pdf)

- Info:

  A named `character` vector specifying the version and date a sensor
  was added

## Details

**Utility:** Some sensors provide raw counts and gain levels as part of
their output. In some cases it is desirable to compare counts between
sensors, e.g., to gauge daylight outside by comparing UV counts to
photopic counts (a high ratio of UV/Pho indicates outside daylight). Or
to gauge daylight inside by comparing IR counts to photopic counts (a
high ratio of IR/Pho with a low ratio of UV/Pho indicates daylight in
the context of LED or fluorescent lighting)

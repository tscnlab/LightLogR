# Sample of highly irregular wearable data

A dataset collected with a wearable device that has a somewhat irregular
recording pattern. Overall, the data are recorded every 15 seconds.
Every tenth or so measurement takes 16 seconds, every hundredths 17
seconds, every thousandths 18 seconds, and so on. This makes the dataset
a prime example for handling and dealing with irregular data.

## Usage

``` r
sample.data.irregular
```

## Format

`sample.data.irregular` A tibble with 11,422 rows and 13 columns:

- Id:

  A `character` vector indicating the participant (only `P1`).

- Datetime:

  POSIXct Datetime

- lux:

  numeric Illuminance. Unit is lux.

- kelvin:

  numeric correlated colour temperature (CCT). Unit is Kelvin.

- rgbR:

  numeric red sensor channel output. Unit is W/m2/nm.

- rgbG:

  numeric green sensor channel output. Unit is W/m2/nm.

- rgbB:

  numeric blue sensor channel output. Unit is W/m2/nm.

- rgbIR:

  numeric infrared sensor channel output. Unit is W/m2/nm.

- movement:

  numeric indicator for movement (intensity) of the device. Movement is
  given in discrete counts correlating to the number of instances the
  accelerometer records instances greater than 0.1875g per 15s sampling
  interval.

- MEDI:

  melanopic EDI measurement data. Unit is lux.

- R.:

  Unknown, but likely direct or derived output from the red sensor
  channel

- G.:

  Unknown, but likely direct or derived output from the green sensor
  channel

- B.:

  Unknown, but likely direct or derived output from the blue sensor
  channel

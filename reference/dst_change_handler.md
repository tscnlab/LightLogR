# Handle jumps in Daylight Savings (DST) that are missing in the data

When data is imported through `LightLogR` and a timezone applied, it is
assumed that the timestamps are correct - which is the case, e.g., if
timestamps are stored in `UTC`, or they are in local time. Some if not
most measurement devices are set to local time before a recording
interval starts. If during the recording a daylight savings jump happens
(in either direction), the device might not adjust timestamps for this
change. This results in an unwanted shift in the data, starting at the
time of the DST jump and likely continues until the end of a file.
`dst_change_handler` is used to detect such jumps within a group and
apply the correct shift in the data (i.e., the shift that should have
been applied by the device).

**important** Note that this function is only useful if the time stamp
in the raw data deviates from the actual date-time. Note also, that this
function results in a gap during the DST jump, which should be handled
by
[`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md)
afterwards. It will also result in potentially double the timestamps
during the jum back from DST to standard time. This will result in some
inconsistencies with some functions, so we recommend to use
[`aggregate_Datetime()`](https://tscnlab.github.io/LightLogR/reference/aggregate_Datetime.md)
afterwards with a `unit` equal to the dominant epoch. Finally, the
function is not equipped to handle more than one jump per group. The
jump is based on whether the group starts out with DST or not. **the
function will remove datetime rows with `NA` values**.

## Usage

``` r
dst_change_handler(
  dataset,
  Datetime.colname = Datetime,
  filename.colname = NULL
)
```

## Arguments

- dataset:

  dataset to be summarized, must be a `dataframe`

- Datetime.colname:

  name of the column that contains the Datetime data, expects a `symbol`

- filename.colname:

  (optional) column name that contains the filename. If provided, it
  will use this column as a temporary grouping variable additionally to
  the `dataset` grouping.

## Value

A `tibble` with the same columns as the input dataset, but shifted

## Details

The detection of a DST jump is based on the function
[`lubridate::dst()`](https://lubridate.tidyverse.org/reference/dst.html)
and jumps are only applied within a group. During import, this function
is used if `dst_adjustment = TRUE` is set and includes by default the
filename as the grouping variable, additionally to `Id`.

## See also

Other DST:
[`dst_change_summary()`](https://tscnlab.github.io/LightLogR/reference/dst_change_summary.md)

## Examples

``` r
#create some data that crosses a DST jump
data <- 
 tibble::tibble(
 Datetime = seq.POSIXt(from = as.POSIXct("2023-03-26 01:30:00", tz = "Europe/Berlin"),
                     to = as.POSIXct("2023-03-26 03:00:00", tz = "Europe/Berlin"),
                     by = "30 mins"),
                     Value = 1)
 #as can be seen next, there is a gap in the data - this is necessary when
 #using a timezone with DST. 
 data$Datetime
#> [1] "2023-03-26 01:30:00 CET"  "2023-03-26 03:00:00 CEST"
 #Let us say now, that the device did not adjust for the DST - thus the 03:00 
 #timestamp is actually 04:00 in local time. This can be corrected for by:
 data %>% dst_change_handler() %>% .$Datetime
#> [1] "2023-03-26 01:30:00 CET"  "2023-03-26 04:00:00 CEST"
```

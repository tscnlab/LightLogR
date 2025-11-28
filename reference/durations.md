# Calculate duration of data in each group

This function calculates the total duration of data in each group of a
dataset, based on a datetime column and a variable column. It uses the
dominant epoch (interval) of each group to calculate the duration.

## Usage

``` r
durations(
  dataset,
  Variable.colname = Datetime,
  Datetime.colname = Datetime,
  count.NA = FALSE,
  show.missing = FALSE,
  show.interval = FALSE,
  FALSE.as.NA = FALSE
)
```

## Arguments

- dataset:

  A light logger dataset. Expects a dataframe. If not imported by
  LightLogR, take care to choose sensible variables for the
  Datetime.colname and Variable.colname.

- Variable.colname:

  Column name that contains the variable for which to calculate the
  duration. Expects a symbol. Needs to be part of the dataset.

- Datetime.colname:

  Column name that contains the datetime. Defaults to "Datetime" which
  is automatically correct for data imported with LightLogR. Expects a
  symbol. Needs to be part of the dataset. Must be of type POSIXct.

- count.NA:

  Logical. Should NA values in Variable.colname be counted as part of
  the duration? Defaults to FALSE.

- show.missing:

  Logical. Should the duration of NAs be provided in a separate column
  "Missing"? Defaults to FALSE.

- show.interval:

  Logical. Should the dominant epoch (interval) be shown in a column
  "interval"? Defaults to FALSE.

- FALSE.as.NA:

  Logical. Should FALSE values in the Variable.colname be treated as NA
  (i.e., missing)?

## Value

A tibble with one row per group and a column "duration" containing the
duration of each group as a
[`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html).
If `show.missing = TRUE`, a column "missing" is added with the duration
of NAs, and a column "total" with the total duration. If
`show.interval = TRUE`, a column "interval" is added with the dominant
epoch of each group.

## Examples

``` r
# Calculate the duration of a dataset
durations(sample.data.environment)
#> # A tibble: 2 × 2
#> # Groups:   Id [2]
#>   Id          duration         
#>   <fct>       <Duration>       
#> 1 Environment 518400s (~6 days)
#> 2 Participant 518400s (~6 days)

# create artificial gaps in the data
gapped_data <-
sample.data.environment |>
  dplyr::filter(MEDI >= 10) |>
  gap_handler(full.days = TRUE)

#by default, the Datetime column is selected for the `Variable.colname`, 
#basically ignoring NA measurement values
gapped_data |>
 durations(count.NA = TRUE)
#> # A tibble: 2 × 2
#> # Groups:   Id [2]
#>   Id          duration         
#>   <fct>       <Duration>       
#> 1 Environment 518400s (~6 days)
#> 2 Participant 518400s (~6 days)

# Calculate the duration where MEDI are available
durations(gapped_data, MEDI)
#> # A tibble: 2 × 2
#> # Groups:   Id [2]
#>   Id          duration            
#>   <fct>       <Duration>          
#> 1 Environment 307950s (~3.56 days)
#> 2 Participant 257470s (~2.98 days)

# Calculate the duration, show the duration of NAs separately
durations(gapped_data, MEDI, show.missing = TRUE)
#> # A tibble: 2 × 4
#> # Groups:   Id [2]
#>   Id          duration             missing              total            
#>   <fct>       <Duration>           <Duration>           <Duration>       
#> 1 Environment 307950s (~3.56 days) 210450s (~2.44 days) 518400s (~6 days)
#> 2 Participant 257470s (~2.98 days) 260930s (~3.02 days) 518400s (~6 days)

# Calculate the duration, show the dominant epoch
durations(gapped_data, Variable.colname = MEDI, show.interval = TRUE)
#> # A tibble: 2 × 3
#> # Groups:   Id [2]
#>   Id          duration             interval  
#>   <fct>       <Duration>           <Duration>
#> 1 Environment 307950s (~3.56 days) 30s       
#> 2 Participant 257470s (~2.98 days) 10s       

# Calculate durations for day and night separately
gapped_data |>
  add_photoperiod(coordinates = c(48.52, 9.06)) |>
  dplyr::group_by(photoperiod.state, .add = TRUE) |>
  durations(Variable.colname = MEDI, show.interval = TRUE, show.missing = TRUE)
#> # A tibble: 4 × 6
#> # Groups:   Id, photoperiod.state [4]
#>   Id          photoperiod.state duration             missing              
#>   <fct>       <chr>             <Duration>           <Duration>           
#> 1 Environment day               307950s (~3.56 days) 6270s (~1.74 hours)  
#> 2 Environment night             0s                   204180s (~2.36 days) 
#> 3 Participant day               232550s (~2.69 days) 81680s (~22.69 hours)
#> 4 Participant night             24920s (~6.92 hours) 179250s (~2.07 days) 
#> # ℹ 2 more variables: total <Duration>, interval <Duration>
```

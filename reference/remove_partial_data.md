# Remove groups that have too few data points

This function removes groups from a dataframe that do not have
sufficient data points. Groups of one data point will automatically be
removed. Single data points are common after using
[`aggregate_Datetime()`](https://tscnlab.github.io/LightLogR/reference/aggregate_Datetime.md).

## Usage

``` r
remove_partial_data(
  dataset,
  Variable.colname = Datetime,
  threshold.missing = 0.2,
  by.date = FALSE,
  Datetime.colname = Datetime,
  show.result = FALSE,
  handle.gaps = FALSE
)
```

## Arguments

- dataset:

  A light logger dataset. Expects a dataframe. If not imported by
  LightLogR, take care to choose sensible variables for the
  Datetime.colname and Variable.colname.

- Variable.colname:

  Column name that contains the variable for which to assess sufficient
  datapoints. Expects a symbol. Needs to be part of the dataset. Default
  is `Datetime`, which makes only sense in the presence of single data
  point groups that need to be removed.

- threshold.missing:

  either

  - percentage of missing data, before that group gets removed. Expects
    a numeric scalar.

  - duration of missing data, before that group gets removed. Expects
    either a
    [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
    or a character that can be converted to one, e.g., "30 mins". If
    negative duration is specified (e.g., "-20 hours"), this will be
    taken as a minimum duration of available data.

- by.date:

  Logical. Should the data be (additionally) grouped by day? Defaults to
  `FALSE`. Additional grouping is not persitant beyond the function
  call.

- Datetime.colname:

  Column name that contains the datetime. Defaults to "Datetime" which
  is automatically correct for data imported with LightLogR. Expects a
  symbol. Needs to be part of the dataset. Must be of type POSIXct.

- show.result:

  Logical, whether the output of the function is summary of the data
  (TRUE), or the reduced dataset (FALSE, the default)

- handle.gaps:

  Logical, whether the data shall be treated with
  [`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md).
  Is set to `FALSE` by default. If `TRUE`, it will be used with the
  argument `full.days = TRUE`.

## Value

if `show.result = FALSE`(default), a reduced dataframe without the
groups that did not have sufficient data

## Details

If instead of missing data, the goal is to only leave a minimum length,
use a `-`(dash) when specifying duration (e.g., `"-20 hours"`) or supply
a negative duration (e.g. `lubridate::dhours(-20)`). That will only
leave groups with at least 20 hours of data. This is useful if the total
duration per group is not fixed (e.g., by date).

## Examples

``` r
#create sample data with gaps
gapped_data <-
  sample.data.environment |>
  dplyr::filter(MEDI < 30000)

#check their status, based on the MEDI variable
gapped_data |> remove_partial_data(MEDI, handle.gaps = TRUE, show.result = TRUE)
#> # A tibble: 2 × 8
#> # Groups:   Id [2]
#>   marked.for.removal Id          duration             missing               
#>   <lgl>              <fct>       <Duration>           <Duration>            
#> 1 TRUE               Environment 403920s (~4.68 days) 114480s (~1.32 days)  
#> 2 FALSE              Participant 516350s (~5.98 days) 2050s (~34.17 minutes)
#> # ℹ 4 more variables: total <Duration>, missing_pct <dbl>, threshold <dbl>,
#> #   interval <Duration>

#the function will produce a warning if implicit gaps are present
gapped_data |> remove_partial_data(MEDI, show.result = TRUE)
#> This dataset has implicit gaps. Please make sure to convert them to explicit gaps or that you really know what you are doing
#> # A tibble: 2 × 8
#> # Groups:   Id [2]
#>   marked.for.removal Id        duration             missing total               
#>   <lgl>              <fct>     <Duration>           <Durat> <Duration>          
#> 1 FALSE              Environm… 403920s (~4.68 days) 0s      403920s (~4.68 days)
#> 2 FALSE              Particip… 516350s (~5.98 days) 0s      516350s (~5.98 days)
#> # ℹ 3 more variables: missing_pct <dbl>, threshold <dbl>, interval <Duration>

#one group (Environment) does not make the cut of 20% missing data
gapped_data |> remove_partial_data(MEDI, handle.gaps = TRUE) |> dplyr::count(Id)
#> # A tibble: 1 × 2
#> # Groups:   Id [1]
#>   Id              n
#>   <fct>       <int>
#> 1 Participant 51635
#for comparison
gapped_data |> dplyr::count(Id)
#> # A tibble: 2 × 2
#> # Groups:   Id [2]
#>   Id              n
#>   <fct>       <int>
#> 1 Environment 13464
#> 2 Participant 51635
#If the threshold is set differently, e.g., to 2 days allowed missing, results vary
gapped_data |>
  remove_partial_data(MEDI, handle.gaps = TRUE, threshold.missing = "2 days") |>
  dplyr::count(Id)
#> # A tibble: 2 × 2
#> # Groups:   Id [2]
#>   Id              n
#>   <fct>       <int>
#> 1 Environment 13464
#> 2 Participant 51635

#The removal can be automatically switched to daily detections within groups
gapped_data |>
 remove_partial_data(MEDI, handle.gaps = TRUE, by.date = TRUE, show.result = TRUE) |>
 head()
#> # A tibble: 6 × 9
#> # Groups:   Id, .date [6]
#>   marked.for.removal Id    .date      duration              missing             
#>   <lgl>              <fct> <date>     <Duration>            <Duration>          
#> 1 FALSE              Envi… 2023-08-29 85020s (~23.62 hours) 1380s (~23 minutes) 
#> 2 FALSE              Envi… 2023-08-30 69150s (~19.21 hours) 17250s (~4.79 hours)
#> 3 TRUE               Envi… 2023-08-31 63870s (~17.74 hours) 22530s (~6.26 hours)
#> 4 TRUE               Envi… 2023-09-01 65820s (~18.28 hours) 20580s (~5.72 hours)
#> 5 TRUE               Envi… 2023-09-02 56880s (~15.8 hours)  29520s (~8.2 hours) 
#> 6 TRUE               Envi… 2023-09-03 63180s (~17.55 hours) 23220s (~6.45 hours)
#> # ℹ 4 more variables: total <Duration>, missing_pct <dbl>, threshold <dbl>,
#> #   interval <Duration>
```

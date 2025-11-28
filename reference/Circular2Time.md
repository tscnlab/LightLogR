# Convert circular time columns to hms

Convert circular time columns to hms

## Usage

``` r
Circular2Time(
  dataset,
  cols = dplyr::where(circular::is.circular),
  silent = FALSE
)
```

## Arguments

- dataset:

  A data.frame with `circular` columns representing time of day.

- cols:

  The column names to convert. Expects a `symbol`. The default will
  convert all `circular` columns. If uncertain whether columns exist in
  the dataset, use
  [`dplyr::any_of()`](https://dplyr.tidyverse.org/reference/reexports.html).

- silent:

  Logical on whether no message shall be shown if input and output are
  identical. Defaults to `FALSE` (i.e., a message is shown).

## Value

The input dataset with converted circular columns as time (hms) columns.
With the default settings, if no circular column exists, input and
output will be identical.

## Examples

``` r
times <- lubridate::as_datetime("2023-01-01 10:00:00") + lubridate::hours(0:2)
times
#> [1] "2023-01-01 10:00:00 UTC" "2023-01-01 11:00:00 UTC"
#> [3] "2023-01-01 12:00:00 UTC"
circular_times <- Datetime2Time(tibble::tibble(Timestamp = times), circular = TRUE)
circular_times
#> # A tibble: 3 × 1
#>   Timestamp 
#>   <circular>
#> 1 2.617994  
#> 2 2.879793  
#> 3 3.141593  
Circular2Time(circular_times)
#> # A tibble: 3 × 1
#>   Timestamp
#>   <time>   
#> 1 10:00    
#> 2 11:00    
#> 3 12:00    

#if times are not circular, then an averaging can be problematic across midnight:
selected_times <- 
sample.data.environment |> 
sample_groups() |> 
dplyr::slice(c(40:43, 51838:51840))
selected_times
#> # A tibble: 7 × 3
#> # Groups:   Id [1]
#>   Id          Datetime             MEDI
#>   <fct>       <dttm>              <dbl>
#> 1 Participant 2023-08-29 00:06:34     0
#> 2 Participant 2023-08-29 00:06:44     0
#> 3 Participant 2023-08-29 00:06:54     0
#> 4 Participant 2023-08-29 00:07:04     0
#> 5 Participant 2023-09-03 23:59:34     0
#> 6 Participant 2023-09-03 23:59:44     0
#> 7 Participant 2023-09-03 23:59:54     0

#a simple averaging will lead to a nonsensical value, e.g. if this should 
#calculate average sleep timing: ~10:00 in the morning
selected_times |> summarize_numeric()
#> # A tibble: 1 × 4
#>   Id          mean_Datetime mean_MEDI episodes
#>   <fct>       <time>            <dbl>    <int>
#> 1 Participant 10:20:55              0        7

#by converting it to a circular beforehand, averaging works as expected: 
#~3 minutes after midnight
selected_times |> 
summarize_numeric(Datetime2Time.circular = TRUE) |> 
Circular2Time()
#> # A tibble: 1 × 4
#>   Id          mean_Datetime mean_MEDI episodes
#>   <fct>       <time>            <dbl>    <int>
#> 1 Participant 03'46.859494"         0        7
```

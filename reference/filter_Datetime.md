# Filter Datetimes in a dataset.

Filtering a dataset based on Dates or Datetimes may often be necessary
prior to calcuation or visualization. The functions allow for a
filtering based on simple `strings` or `Datetime` scalars, or by
specifying a length. They also support prior dplyr grouping, which is
useful, e.g., when you only want to filter the first two days of
measurement data for every participant, regardless of the actual date.
If you want to filter based on times of the day, look to
[`filter_Time()`](https://tscnlab.github.io/LightLogR/reference/filter_Time.md).

## Usage

``` r
filter_Datetime(
  dataset,
  length = NULL,
  start = NULL,
  end = NULL,
  length_from_start = TRUE,
  full.day = FALSE,
  only_Id = NULL,
  filter.expr = NULL,
  Datetime.colname = Datetime,
  tz = NULL
)

filter_Date(..., start = NULL, end = NULL)
```

## Arguments

- dataset:

  A light logger dataset. Expects a `dataframe`. If not imported by
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md),
  take care to choose a sensible variable for the `Datetime.colname`.

- length:

  Either a Period or Duration from lubridate. E.g.,
  `days(2) + hours(12)` will give a period of 2.5 days, whereas
  `ddays(2) + dhours(12)` will give a duration. For the difference
  between periods and durations look at the documentation from
  lubridate. Basically, periods model clocktimes, whereas durations
  model physical processes. This matters on several occasions, like leap
  years, or daylight savings. You can also provide a `character` scalar
  in the form of e.g. "1 day", which will be converted into a period.

- start, end:

  For `filter_Datetime()` a `POSIXct` or `character` scalar in the form
  of `"yyyy-mm-dd hh-mm-ss"` giving the respective start and end time
  positions for the filtered dataframe. If you only want to provide
  `dates` in the form of `"yyyy-mm-dd"`, use the wrapper function
  `filter_Date()`.

  - If one of start/end are not provided, the times will be taken from
    the respective extreme values of the `dataset`.

  - If `length` is provided and one of start/end is not, the other will
    be calculated based on the given value.

  - If `length` is provided and both of start/end are NULL, the time
    from the respective start (within each group) is taken.

- length_from_start:

  A `logical` indicating whether the `length` argument should be applied
  to the start (default, TRUE) or the end of the data (FALSE). Only
  relevant if neither the `start` nor the `end` arguments are provided.

- full.day:

  A `logical` indicating whether the `start` param should be rounded to
  a full day, when only the `length` argument is provided (Default is
  FALSE). This is useful, e.g., when the first observation in the
  dataset is slightly after midnight. If TRUE, it will count the length
  from midnight on to avoid empty days in plotting with
  [`gg_day()`](https://tscnlab.github.io/LightLogR/reference/gg_day.md).

- only_Id:

  An expression of `ids` where the filtering should be applied to. If
  `NULL` (the default), the filtering will be applied to all `ids`.
  Based on the this expression, the dataset will be split in two and
  only where the given expression evaluates to `TRUE`, will the
  filtering take place. Afterwards both sets are recombined and sorted
  by `Datetime`.

- filter.expr:

  Advanced filtering conditions. If not `NULL` (default) and given an
  `expression`, this is used to
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
  the results. This can be useful to filter, e.g. for group-specific
  conditions, like starting after the first two days of measurement (see
  examples).

- Datetime.colname:

  column name that contains the datetime. Defaults to `"Datetime"` which
  is automatically correct for data imported with
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md).
  Expects a `symbol`. Needs to be part of the `dataset`. Must be of type
  `POSIXct`.

- tz:

  Timezone of the start/end times. If `NULL` (the default), it will take
  the timezone from the `Datetime.colname` column.

- ...:

  Parameter handed over to
  [`lubridate::round_date()`](https://lubridate.tidyverse.org/reference/round_date.html)
  and siblings

## Value

a `data.frame` object identical to `dataset` but with only the specified
Dates/Times.

## See also

Other filter:
[`filter_Time()`](https://tscnlab.github.io/LightLogR/reference/filter_Time.md)

Other filter:
[`filter_Time()`](https://tscnlab.github.io/LightLogR/reference/filter_Time.md)

## Examples

``` r
library(lubridate)
library(dplyr)
#baseline
range.unfiltered <- sample.data.environment$Datetime %>% range()
range.unfiltered
#> [1] "2023-08-29 00:00:04 CEST" "2023-09-03 23:59:54 CEST"

#setting the start of a dataset
sample.data.environment %>%
filter_Datetime(start = "2023-08-31 12:00:00") %>%
pull(Datetime) %>%
range()
#> [1] "2023-08-31 12:00:04 CEST" "2023-09-03 23:59:44 CEST"

#setting the end of a dataset
sample.data.environment %>%
filter_Datetime(end = "2023-08-31 12:00:00") %>% pull(Datetime) %>% range()
#> [1] "2023-08-29 00:00:04 CEST" "2023-08-31 11:59:54 CEST"

#setting a period of a dataset
sample.data.environment %>%
filter_Datetime(end = "2023-08-31 12:00:00", length = days(2)) %>%
pull(Datetime) %>% range()
#> [1] "2023-08-29 12:00:04 CEST" "2023-08-31 11:59:54 CEST"

#setting only the period of a dataset
sample.data.environment %>%
filter_Datetime(length = days(2)) %>%
pull(Datetime) %>% range()
#> [1] "2023-08-29 00:00:04 CEST" "2023-08-30 23:59:54 CEST"

#advanced filtering based on grouping (second day of each group)
sample.data.environment %>%
#shift the "Environment" group by one day
mutate(
Datetime = ifelse(Id == "Environment", Datetime + ddays(1), Datetime) %>%
as_datetime()) -> sample
sample %>% summarize(Daterange = paste(min(Datetime), max(Datetime), sep = " - "))
#> # A tibble: 2 × 2
#>   Id          Daterange                                
#>   <fct>       <chr>                                    
#> 1 Environment 2023-08-29 22:00:08 - 2023-09-04 21:59:38
#> 2 Participant 2023-08-28 22:00:04 - 2023-09-03 21:59:54
#now we can use the `filter.expr` argument to filter from the second day of each group
sample %>%
filter_Datetime(filter.expr = Datetime > Datetime[1] + days(1)) %>%
summarize(Daterange = paste(min(Datetime), max(Datetime), sep = " - "))
#> # A tibble: 2 × 2
#>   Id          Daterange                                
#>   <fct>       <chr>                                    
#> 1 Environment 2023-08-30 22:00:38 - 2023-09-04 21:59:08
#> 2 Participant 2023-08-29 22:00:14 - 2023-09-03 21:59:54

sample.data.environment %>% filter_Date(end = "2023-08-31")
#> # A tibble: 34,560 × 3
#> # Groups:   Id [2]
#>    Id          Datetime             MEDI
#>    <fct>       <dttm>              <dbl>
#>  1 Participant 2023-08-29 00:00:04     0
#>  2 Participant 2023-08-29 00:00:14     0
#>  3 Participant 2023-08-29 00:00:24     0
#>  4 Participant 2023-08-29 00:00:34     0
#>  5 Participant 2023-08-29 00:00:44     0
#>  6 Participant 2023-08-29 00:00:54     0
#>  7 Participant 2023-08-29 00:01:04     0
#>  8 Participant 2023-08-29 00:01:14     0
#>  9 Participant 2023-08-29 00:01:24     0
#> 10 Participant 2023-08-29 00:01:34     0
#> # ℹ 34,550 more rows
```

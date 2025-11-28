# Find or set sensible limits for Datetime axis

Take a vector of `Datetimes` and return the start of the first and end
of the last day of data. The `start` and the `length` can be adjusted by
`durations`, like
[`lubridate::ddays()`](https://lubridate.tidyverse.org/reference/duration.html).
It is used in the
[`gg_days()`](https://tscnlab.github.io/LightLogR/reference/gg_days.md)
function to return a sensible x-axis. This function is a thin wrapper
around
[`lubridate::floor_date()`](https://lubridate.tidyverse.org/reference/round_date.html)
and
[`lubridate::ceiling_date()`](https://lubridate.tidyverse.org/reference/round_date.html).

## Usage

``` r
Datetime_limits(
  x,
  start = NULL,
  length = NULL,
  unit = "1 day",
  midnight.rollover = FALSE,
  ...
)
```

## Arguments

- x:

  a vector of `Datetimes`

- start:

  optional `duration` object, e.g. `lubridate::ddays(1)` that shifts the
  start of the `Datetime` vector by this amount.

- length:

  optional `duration` object, e.g. `lubridate::ddays(7)` that shifts the
  end of the `Datetime` vector by this amount from the (adjusted) start.
  Depending on the data, you might have to subtract one day from the
  desired length to get the correct axis-scaling if you start at
  midnight.

- unit:

  a `character` scalar giving the unit of rounding in
  [`lubridate::floor_date()`](https://lubridate.tidyverse.org/reference/round_date.html)
  and
  [`lubridate::ceiling_date()`](https://lubridate.tidyverse.org/reference/round_date.html)

- midnight.rollover:

  a `logical` scalar indicating whether to rollover in cases of exact
  matches of rounded values and input values. Helpful if some cases fall
  exactly on the rounded values and others don\`t.

- ...:

  other arguments passed to
  [`lubridate::floor_date()`](https://lubridate.tidyverse.org/reference/round_date.html)
  and
  [`lubridate::ceiling_date()`](https://lubridate.tidyverse.org/reference/round_date.html)

## Value

a 2 item `vector` of `Datetimes` with the (adjusted) start and end of
the input vector.

## Examples

``` r
dataset <- c("2023-08-15", "2023-08-20")
breaks <- Datetime_breaks(dataset)
Datetime_limits(breaks)
#> [1] "2023-08-15 UTC" "2023-08-20 UTC"
Datetime_limits(breaks, start = lubridate::ddays(1))
#> [1] "2023-08-16 UTC" "2023-08-20 UTC"
Datetime_limits(breaks, length = lubridate::ddays(2))
#> [1] "2023-08-15 UTC" "2023-08-18 UTC"
```

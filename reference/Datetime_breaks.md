# Create a (shifted) sequence of Datetimes for axis breaks

Take a vector of Datetimes and create a sequence of Datetimes with a
given shift and interval. This is a helper function to create breaks for
plotting, e.g. in
[`gg_days()`](https://tscnlab.github.io/LightLogR/reference/gg_days.md),
and is best used in conjunction with
[`Datetime_limits()`](https://tscnlab.github.io/LightLogR/reference/Datetime_limits.md).
The function is a thin wrapper around
[`seq()`](https://rdrr.io/r/base/seq.html).

## Usage

``` r
Datetime_breaks(x, shift = lubridate::duration(12, "hours"), by = "1 day")
```

## Arguments

- x:

  a vector of `Datetimes`

- shift:

  a `numeric` giving the number of `duration` object, e.g.
  `lubridate::duration(12, "hours")`

- by:

  a `character` scalar giving the unit of the interval in
  [`base::seq()`](https://rdrr.io/r/base/seq.html)

## Value

a `vector` of `Datetimes`

## Examples

``` r
dataset <- c("2023-08-15", "2023-08-20")
Datetime_breaks(dataset)
#> [1] "2023-08-15 12:00:00 UTC" "2023-08-16 12:00:00 UTC"
#> [3] "2023-08-17 12:00:00 UTC" "2023-08-18 12:00:00 UTC"
#> [5] "2023-08-19 12:00:00 UTC"
Datetime_breaks(dataset, shift = 0)
#> [1] "2023-08-15 UTC" "2023-08-16 UTC" "2023-08-17 UTC" "2023-08-18 UTC"
#> [5] "2023-08-19 UTC" "2023-08-20 UTC"
Datetime_breaks(dataset, by = "12 hours")
#>  [1] "2023-08-15 12:00:00 UTC" "2023-08-16 00:00:00 UTC"
#>  [3] "2023-08-16 12:00:00 UTC" "2023-08-17 00:00:00 UTC"
#>  [5] "2023-08-17 12:00:00 UTC" "2023-08-18 00:00:00 UTC"
#>  [7] "2023-08-18 12:00:00 UTC" "2023-08-19 00:00:00 UTC"
#>  [9] "2023-08-19 12:00:00 UTC" "2023-08-20 00:00:00 UTC"
```

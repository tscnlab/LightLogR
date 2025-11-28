# Style (date)times as times

This function takes a numeric input vector, converts them to an `hms`
(using [`hms::as_hms()`](https://hms.tidyverse.org/reference/hms.html)),
then to a `POSIXlt` (using
[`base::as.POSIXlt()`](https://rdrr.io/r/base/as.POSIXlt.html)), and
then formats is according to the `format` argument.

## Usage

``` r
style_time(x, format = "%H:%M")
```

## Arguments

- x:

  a `numeric` vector to be styled

- format:

  output format. Defaults to "%H:%M", which results in, e.g., "03:45".
  Look to [`base::strptime()`](https://rdrr.io/r/base/strptime.html) for
  formatting options.

## Value

a `character` vector of length(x)

## Examples

``` r
#collect some time info
time <- 
sample.data.irregular |> 
dplyr::slice(300:305) |> 
dplyr::pull(Datetime)

#Output is of type POSIXct
time
#> [1] "2023-06-21 01:15:06 UTC" "2023-06-21 01:15:21 UTC"
#> [3] "2023-06-21 01:15:36 UTC" "2023-06-21 01:15:51 UTC"
#> [5] "2023-06-21 01:16:06 UTC" "2023-06-21 01:16:21 UTC"

time |> style_time()
#> [1] "01:15" "01:15" "01:15" "01:15" "01:16" "01:16"
```

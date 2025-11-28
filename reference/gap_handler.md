# Fill implicit gaps in a light logger dataset

Datasets from light loggers often have implicit gaps. These gaps are
implicit in the sense that consecutive timestamps (`Datetimes`) might
not follow a regular epoch/interval. This function fills these implicit
gaps by creating a gapless sequence of `Datetimes` and joining it to the
dataset. The gapless sequence is determined by the minimum and maximum
`Datetime` in the dataset (per group) and an epoch. The epoch can either
be guessed from the dataset or specified by the user. A sequence of
gapless `Datetimes` can be created with the
[`gapless_Datetimes()`](https://tscnlab.github.io/LightLogR/reference/gapless_Datetimes.md)
function, whereas the dominant epoch in the data can be checked with the
[`dominant_epoch()`](https://tscnlab.github.io/LightLogR/reference/dominant_epoch.md)
function. The `behaviour` argument specifies how the data is combined.
By default, the data is joined with a full join, which means that all
rows from the gapless sequence are kept, even if there is no matching
row in the dataset.

## Usage

``` r
gap_handler(
  dataset,
  Datetime.colname = Datetime,
  epoch = "dominant.epoch",
  behavior = c("full_sequence", "regulars", "irregulars", "gaps"),
  full.days = FALSE
)
```

## Arguments

- dataset:

  A light logger dataset. Needs to be a dataframe.

- Datetime.colname:

  The column that contains the datetime. Needs to be a `POSIXct` and
  part of the dataset.

- epoch:

  The epoch to use for the gapless sequence. Can be either a
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
  or a string. If it is a string, it needs to be either
  '"dominant.epoch"' (the default) for a guess based on the data or a
  valid
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

- behavior:

  The behavior of the join of the `dataset` with the `gapless` sequence.
  Can be one of `"full_sequence"` (the default), `"regulars"`,
  `"irregulars"`, or `"gaps"`. See @return for details.

- full.days:

  If `TRUE`, the gapless sequence will include the whole first and last
  day where there is data.

## Value

A modified `tibble` similar to `dataset` but with handling of implicit
gaps, depending on the `behavior` argument:

- `"full_sequence"` adds timestamps to the `dataset` that are missing
  based on a full sequence of `Datetimes` (i.e., the gapless sequence).
  The `dataset` is this equal (no gaps) or greater in the number of rows
  than the input. One column is added. `is.implicit` indicates whether
  the row was added (`TRUE`) or not (`FALSE`). This helps
  differentiating measurement values from values that might be imputed
  later on.

- `"regulars"` keeps only rows from the gapless sequence that have a
  matching row in the dataset. This can be interpreted as a row-reduced
  `dataset` with only regular timestamps according to the `epoch`. In
  case of no gaps this tibble has the same number of rows as the input.

- `"irregulars"` keeps only rows from the `dataset` that do not follow
  the regular sequence of `Datetimes` according to the `epoch`. In case
  of no gaps this tibble has 0 rows.

- `"gaps"` returns a `tibble` of all implicit gaps in the dataset. In
  case of no gaps this tibble has 0 rows.

## See also

Other regularize:
[`dominant_epoch()`](https://tscnlab.github.io/LightLogR/reference/dominant_epoch.md),
[`extract_gaps()`](https://tscnlab.github.io/LightLogR/reference/extract_gaps.md),
[`gap_finder()`](https://tscnlab.github.io/LightLogR/reference/gap_finder.md),
[`gapless_Datetimes()`](https://tscnlab.github.io/LightLogR/reference/gapless_Datetimes.md),
[`has_gaps()`](https://tscnlab.github.io/LightLogR/reference/has_gaps.md),
[`has_irregulars()`](https://tscnlab.github.io/LightLogR/reference/has_irregulars.md)

## Examples

``` r
dataset <-
tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
              Datetime = lubridate::as_datetime(1) +
                         lubridate::days(c(0:2, 4, 6, 8)) +
                         lubridate::hours(c(0,12,rep(0,4)))) %>% 
dplyr::group_by(Id)
dataset
#> # A tibble: 6 × 2
#> # Groups:   Id [2]
#>   Id    Datetime           
#>   <chr> <dttm>             
#> 1 A     1970-01-01 00:00:01
#> 2 A     1970-01-02 12:00:01
#> 3 A     1970-01-03 00:00:01
#> 4 B     1970-01-05 00:00:01
#> 5 B     1970-01-07 00:00:01
#> 6 B     1970-01-09 00:00:01
#assuming the epoch is 1 day, we can add implicit data to our dataset
dataset %>% gap_handler(epoch = "1 day")
#> # A tibble: 9 × 3
#> # Groups:   Id [2]
#>   Id    Datetime            is.implicit
#>   <chr> <dttm>              <lgl>      
#> 1 A     1970-01-01 00:00:01 FALSE      
#> 2 A     1970-01-02 00:00:01 TRUE       
#> 3 A     1970-01-02 12:00:01 FALSE      
#> 4 A     1970-01-03 00:00:01 FALSE      
#> 5 B     1970-01-05 00:00:01 FALSE      
#> 6 B     1970-01-06 00:00:01 TRUE       
#> 7 B     1970-01-07 00:00:01 FALSE      
#> 8 B     1970-01-08 00:00:01 TRUE       
#> 9 B     1970-01-09 00:00:01 FALSE      

#we can also check whether there are irregular Datetimes in our dataset
dataset %>% gap_handler(epoch = "1 day", behavior = "irregulars")
#> # A tibble: 1 × 3
#> # Groups:   Id [1]
#>   Id    Datetime            is.implicit
#>   <chr> <dttm>              <lgl>      
#> 1 A     1970-01-02 12:00:01 FALSE      

#to get to the gaps, we can use the "gaps" behavior
dataset %>% gap_handler(epoch = "1 day", behavior = "gaps")
#> # A tibble: 3 × 2
#> # Groups:   Id [2]
#>   Id    Datetime           
#>   <chr> <dttm>             
#> 1 A     1970-01-02 00:00:01
#> 2 B     1970-01-06 00:00:01
#> 3 B     1970-01-08 00:00:01
 
#finally, we can also get just the regular Datetimes
dataset %>% gap_handler(epoch = "1 day", behavior = "regulars")
#> # A tibble: 5 × 3
#> # Groups:   Id [2]
#>   Id    Datetime            is.implicit
#>   <chr> <dttm>              <lgl>      
#> 1 A     1970-01-01 00:00:01 FALSE      
#> 2 A     1970-01-03 00:00:01 FALSE      
#> 3 B     1970-01-05 00:00:01 FALSE      
#> 4 B     1970-01-07 00:00:01 FALSE      
#> 5 B     1970-01-09 00:00:01 FALSE      
```

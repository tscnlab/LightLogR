# Counts the Time differences (epochs) per group (in a grouped dataset)

Counts the Time differences (epochs) per group (in a grouped dataset)

## Usage

``` r
count_difftime(dataset, Datetime.colname = Datetime)
```

## Arguments

- dataset:

  A light logger dataset. Expects a `dataframe`. If not imported by
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md),
  take care to choose a sensible variable for the `Datetime.colname`.

- Datetime.colname:

  column name that contains the datetime. Defaults to `"Datetime"` which
  is automatically correct for data imported with
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md).
  Expects a `symbol`. Needs to be part of the `dataset`. Must be of type
  `POSIXct`.

## Value

a `tibble` with the number of occurences of each time difference per
group

## Examples

``` r
#count_difftime returns the number of occurences of each time difference
#and is more comprehensive in terms of a summary than `gap_finder` or 
#`dominant_epoch`
count_difftime(sample.data.irregular)
#> # A tibble: 4 × 4
#> # Groups:   Id [1]
#>   Id    difftime       n group.indices
#>   <chr> <Duration> <int>         <int>
#> 1 P1    15s        10015             1
#> 2 P1    16s         1367             1
#> 3 P1    17s           23             1
#> 4 P1    18s           16             1
dominant_epoch(sample.data.irregular)
#> # A tibble: 1 × 3
#>   Id    dominant.epoch group.indices
#>   <chr> <Duration>             <int>
#> 1 P1    15s                        1
gap_finder(sample.data.irregular)
#> Found 10758 gaps. 761 Datetimes fall into the regular sequence.

#irregular data can be regularized with `aggregate_Datetime`
sample.data.irregular |> 
 aggregate_Datetime(unit = "15 secs") |> 
 count_difftime()
#> # A tibble: 2 × 4
#> # Groups:   Id [1]
#>   Id    difftime       n group.indices
#>   <chr> <Duration> <int>         <int>
#> 1 P1    15s        11324             1
#> 2 P1    30s           97             1
```

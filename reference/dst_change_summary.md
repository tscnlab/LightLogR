# Get a summary of groups where a daylight saving time change occurs.

Get a summary of groups where a daylight saving time change occurs.

## Usage

``` r
dst_change_summary(dataset, Datetime.colname = Datetime)
```

## Arguments

- dataset:

  dataset to be summarized, must be a `dataframe`

- Datetime.colname:

  name of the column that contains the Datetime data, expects a `symbol`

## Value

a `tibble` with the groups where a dst change occurs. The column
`dst_start` is a boolean that indicates whether the start of this group
occurs during daylight savings.

## See also

Other DST:
[`dst_change_handler()`](https://tscnlab.github.io/LightLogR/reference/dst_change_handler.md)

## Examples

``` r
sample.data.environment %>% 
  dplyr::mutate(Datetime = 
  Datetime + lubridate::dweeks(8)) %>%
  dst_change_summary()
#> # A tibble: 2 × 2
#>   Id          dst_start
#>   <fct>       <lgl>    
#> 1 Environment TRUE     
#> 2 Participant TRUE     
```

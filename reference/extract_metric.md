# Add metrics to extracted sSummary

This helper function adds metric values to an extract, like from
[`extract_states()`](https://tscnlab.github.io/LightLogR/reference/extract_states.md)
or
[`extract_clusters()`](https://tscnlab.github.io/LightLogR/reference/extract_clusters.md).
E.g., the average value of a variable during a cluster or state instance
might be of interest. The metrics must be specified by the user using
the `...` argument.

## Usage

``` r
extract_metric(
  extracted_data,
  data,
  identifying.colname = state.count,
  Datetime.colname = Datetime,
  ...
)
```

## Arguments

- extracted_data:

  A dataframe containing cluster or state summaries, typically from
  [`extract_clusters()`](https://tscnlab.github.io/LightLogR/reference/extract_clusters.md)
  or
  [`extract_states()`](https://tscnlab.github.io/LightLogR/reference/extract_states.md).

- data:

  The original dataset that produced `extracted_data`.

- identifying.colname:

  Name of the column in `extracted_data` that uniquely identifies each
  row (in addition to the groups. Expects a symbol. Defaults to
  `state.count`

- Datetime.colname:

  Column name that contains the datetime in `data`. Defaults to
  "Datetime" which is automatically correct for data imported with
  LightLogR. Expects a symbol. This argument is only necessary if `data`
  does not contain the `cluster.colname`.

- ...:

  Arguments specifying the metrics to add summary. For example:
  `"mean_lux" = mean(lux)`.

## Value

A dataframe containing the extracted data with added metrics.

## Details

The original `data` does not have to have the cluster/state information,
but it will be computationally faster if it does.

## Examples

``` r
# Extract clusters and add mean MEDI value
sample.data.environment |>
filter_Date(length = "2 days") |> 
extract_clusters(MEDI > 1000) |>
extract_metric(
  sample.data.environment,
  "mean_medi" = mean(MEDI, na.rm = TRUE)
) |>
dplyr::select(Id, state.count, duration, mean_medi)
#> # A tibble: 2 × 4
#> # Groups:   Id [1]
#>   Id          state.count duration              mean_medi
#>   <fct>       <chr>       <Duration>                <dbl>
#> 1 Environment 1           46050s (~12.79 hours)    11906.
#> 2 Environment 2           47370s (~13.16 hours)    25446.

# Extract states and add mean MEDI value
dataset <-
sample.data.environment |>
filter_Date(length = "2 days") |> 
 add_photoperiod(c(48.5, 9))

dataset |>
  extract_states(photoperiod.state) |>
  extract_metric(dataset, mean_lux = mean(MEDI)) |>
  dplyr::select(state.count, duration, mean_lux)
#> Adding missing grouping variables: `Id`, `photoperiod.state`
#> # A tibble: 10 × 5
#> # Groups:   Id, photoperiod.state [4]
#>    Id          photoperiod.state state.count duration                mean_lux
#>    <fct>       <chr>             <chr>       <Duration>                 <dbl>
#>  1 Environment day               day 1       52920s (~14.7 hours)  10388.    
#>  2 Environment day               day 2       52710s (~14.64 hours) 22893.    
#>  3 Environment night             night 1     21810s (~6.06 hours)      0     
#>  4 Environment night             night 2     33570s (~9.32 hours)      0.0142
#>  5 Environment night             night 3     11790s (~3.28 hours)      0.0433
#>  6 Participant day               day 1       52910s (~14.7 hours)    145.    
#>  7 Participant day               day 2       52690s (~14.64 hours)   145.    
#>  8 Participant night             night 1     21820s (~6.06 hours)      0     
#>  9 Participant night             night 2     33580s (~9.33 hours)      6.29  
#> 10 Participant night             night 3     11800s (~3.28 hours)     38.6   
```

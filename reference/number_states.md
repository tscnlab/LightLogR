# Number non-consecutive state occurrences

`number_states()` creates a new column in a dataset that takes a state
column and assigns a count value to each state, rising every time a
state is replaced by another state. E.g., a column with the states "day"
and "night" will produce a column indicating whether this is "day 1",
"day 2", and so forth, as will the "night" state with "night 1", "night
2", etc. Grouping within the input dataset is respected, i.e., the count
will reset for each group.

## Usage

``` r
number_states(
  dataset,
  state.colname,
  colname.extension = ".count",
  use.original.state = TRUE
)
```

## Arguments

- dataset:

  A `data.frame` with a state column.

- state.colname:

  Column name that contains the state. Expects a `symbol`. Needs to be
  part of the `dataset`. Can be of any type, but `character` and
  `factor` make the most sense.

- colname.extension:

  The extension that is added to the state name to create the new
  column. Defaults to `".count"`.

- use.original.state:

  Logical, whether the original state should be part of the output
  column.

## Value

The input `dataset` with an additional column that counts the
occurrences of each state. The new column will of type `character` if
`use.original.state = TRUE` and `integer` otherwise.

## Details

The state column is not limited to two states, but can have as many
states as needed. Also, it does not matter in which time frames these
states change, so they do not necessarily conform to a 24-hour day. `NA`
values will be treated as their own state.

Gaps in the data can lead to non-sensible outcomes, e.g. if there is no
in-between state/observation between a day state at "18:00:00" and a day
state at "6:00:00" - this would be counted as `day 1` still. In these
cases, the
[`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md)
function can be useful to a priori add observations.

## Examples

``` r
dataset <- tibble::tibble(
 state =
 c("day", "day", "day", "night", "night", "day", "day", "night",
 "night", "night", "day", "night")
 )
number_states(dataset, state)
#> # A tibble: 12 × 2
#>    state state.count
#>    <chr> <chr>      
#>  1 day   day 1      
#>  2 day   day 1      
#>  3 day   day 1      
#>  4 night night 1    
#>  5 night night 1    
#>  6 day   day 2      
#>  7 day   day 2      
#>  8 night night 2    
#>  9 night night 2    
#> 10 night night 2    
#> 11 day   day 3      
#> 12 night night 3    
number_states(dataset, state, use.original.state = FALSE)
#> # A tibble: 12 × 2
#>    state state.count
#>    <chr>       <int>
#>  1 day             1
#>  2 day             1
#>  3 day             1
#>  4 night           1
#>  5 night           1
#>  6 day             2
#>  7 day             2
#>  8 night           2
#>  9 night           2
#> 10 night           2
#> 11 day             3
#> 12 night           3

#example with photoperiods, calculating the mean values for each day and night
coordinates <- c(48.52, 9.06)
sample.data.environment |>
  add_photoperiod(coordinates) |>
  number_states(photoperiod.state) |>
  dplyr::group_by(photoperiod.state.count, .add = TRUE) |>
  dplyr::summarize(mean_MEDI = mean(MEDI)) |>
  tail(13)
#> `summarise()` has grouped output by 'Id'. You can override using the `.groups`
#> argument.
#> # A tibble: 13 × 3
#> # Groups:   Id [1]
#>    Id          photoperiod.state.count mean_MEDI
#>    <fct>       <chr>                       <dbl>
#>  1 Participant day 1                     145.   
#>  2 Participant day 2                     145.   
#>  3 Participant day 3                     291.   
#>  4 Participant day 4                    1232.   
#>  5 Participant day 5                    2723.   
#>  6 Participant day 6                    2851.   
#>  7 Participant night 1                     0    
#>  8 Participant night 2                     6.29 
#>  9 Participant night 3                    13.5  
#> 10 Participant night 4                    28.0  
#> 11 Participant night 5                     5.89 
#> 12 Participant night 6                     0.866
#> 13 Participant night 7                    12.1  
```

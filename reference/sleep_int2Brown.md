# Recode Sleep/Wake intervals to Brown state intervals

Takes a dataset with sleep/wake intervals and recodes them to Brown
state intervals. Specifically, it recodes the `sleep` intervals to
`night`, reduces `wake` intervals by a specified `evening.length` and
recodes them to `evening` and `day` intervals. The `evening.length` is
the time between `day` and `night`. The result can be used as input for
[`interval2state()`](https://tscnlab.github.io/LightLogR/reference/interval2state.md)
and might be used subsequently with
[`Brown2reference()`](https://tscnlab.github.io/LightLogR/reference/Brown2reference.md).

## Usage

``` r
sleep_int2Brown(
  dataset,
  Interval.colname = Interval,
  Sleep.colname = State,
  wake.state = "wake",
  sleep.state = "sleep",
  Brown.day = "day",
  Brown.evening = "evening",
  Brown.night = "night",
  evening.length = lubridate::dhours(3),
  Brown.state.colname = State.Brown,
  output.dataset = TRUE
)
```

## Arguments

- dataset:

  A dataset with sleep/wake intervals.

- Interval.colname:

  The name of the column with the intervals. Defaults to `Interval`.

- Sleep.colname:

  The name of the column with the sleep/wake states. Defaults to
  `State`.

- wake.state, sleep.state:

  The names of the wake and sleep states in the `Sleep.colname`. Default
  to `"wake"` and `"sleep"`. Expected to be a `character` scalar and
  must be an exact match.

- Brown.day, Brown.evening, Brown.night:

  The names of the Brown states that will be used. Defaults to `"day"`,
  `"evening"` and `"night"`.

- evening.length:

  The length of the evening interval in seconds. Can also use lubridate
  duration or period objects. Defaults to 3 hours.

- Brown.state.colname:

  The name of the column with the newly created Brown states. Works as a
  simple renaming of the `Sleep.colname`.

- output.dataset:

  Whether to return the whole `dataset` or a `vector` with the Brown
  states.

## Value

A dataset with the Brown states or a vector with the Brown states. The
Brown states are created in a new column with the name specified in
`Brown.state.colname`. The dataset will have more rows than the original
dataset, because the `wake` intervals are split into `day` and `evening`
intervals.

## Details

The function will filter out any non-sleep intervals that are shorter
than the specified `evening.length`. This prevents problematic behaviour
when the `evening.length` is longer than the `wake` intervals or, e.g.,
when the first state is sleep after midnight and there is a prior `NA`
interval from midnight till sleep. This behavior might, however, result
in problematic results for specialized experimental setups with ultra
short wake/sleep cycles. The `sleep_int2Brown()` function would not be
applicable in those cases anyways. Note that any column in the `dataset`
outside of `Interval.colname` and `Sleep.colname` will be filled up from
previous states (respecting groups).

## References

https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001571

## See also

Other Brown:
[`Brown2reference()`](https://tscnlab.github.io/LightLogR/reference/Brown2reference.md),
[`Brown_check()`](https://tscnlab.github.io/LightLogR/reference/Brown_check.md),
[`Brown_cut()`](https://tscnlab.github.io/LightLogR/reference/Brown_cut.md),
[`Brown_rec()`](https://tscnlab.github.io/LightLogR/reference/Brown_rec.md)

## Examples

``` r
#create a sample dataset
sample <- tibble::tibble(Datetime = c("2023-08-15 6:00:00",
                                         "2023-08-15 23:00:00",
                                         "2023-08-16 6:00:00",
                                         "2023-08-16 22:00:00",
                                         "2023-08-17 6:30:00",
                                         "2023-08-18 1:00:00"),
                         State = rep(c("wake", "sleep"), 3),
                         Id = "Participant")
#intervals from sample
sc2interval(sample)
#> # A tibble: 7 × 3
#>   State Id          Interval                                        
#>   <chr> <chr>       <Interval>                                      
#> 1 NA    NA          2023-08-15 00:00:00 UTC--2023-08-15 06:00:00 UTC
#> 2 wake  Participant 2023-08-15 06:00:00 UTC--2023-08-15 23:00:00 UTC
#> 3 sleep Participant 2023-08-15 23:00:00 UTC--2023-08-16 06:00:00 UTC
#> 4 wake  Participant 2023-08-16 06:00:00 UTC--2023-08-16 22:00:00 UTC
#> 5 sleep Participant 2023-08-16 22:00:00 UTC--2023-08-17 06:30:00 UTC
#> 6 wake  Participant 2023-08-17 06:30:00 UTC--2023-08-18 01:00:00 UTC
#> 7 sleep Participant 2023-08-18 01:00:00 UTC--2023-08-19 00:00:00 UTC
#recoded intervals
sc2interval(sample) %>% sleep_int2Brown()
#> # A tibble: 10 × 3
#>    State.Brown Id          Interval                                        
#>    <chr>       <chr>       <Interval>                                      
#>  1 NA          NA          2023-08-15 00:00:00 UTC--2023-08-15 06:00:00 UTC
#>  2 day         Participant 2023-08-15 06:00:00 UTC--2023-08-15 20:00:00 UTC
#>  3 evening     Participant 2023-08-15 20:00:00 UTC--2023-08-15 23:00:00 UTC
#>  4 night       Participant 2023-08-15 23:00:00 UTC--2023-08-16 06:00:00 UTC
#>  5 day         Participant 2023-08-16 06:00:00 UTC--2023-08-16 19:00:00 UTC
#>  6 evening     Participant 2023-08-16 19:00:00 UTC--2023-08-16 22:00:00 UTC
#>  7 night       Participant 2023-08-16 22:00:00 UTC--2023-08-17 06:30:00 UTC
#>  8 day         Participant 2023-08-17 06:30:00 UTC--2023-08-17 22:00:00 UTC
#>  9 evening     Participant 2023-08-17 22:00:00 UTC--2023-08-18 01:00:00 UTC
#> 10 night       Participant 2023-08-18 01:00:00 UTC--NA                     
                                         
```

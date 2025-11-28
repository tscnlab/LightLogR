# Extract summaries of states

Extracts a state from a dataset and provides their start and end times,
as well as duration and epoch. The state does not have to exist in the
dataset, but can be dynamically created. Extracted states can have
group-dropping disabled, meaning that summaries based on the extracted
states show empty groups as well.

## Usage

``` r
extract_states(
  data,
  State.colname,
  State.expression = NULL,
  Datetime.colname = Datetime,
  handle.gaps = FALSE,
  epoch = "dominant.epoch",
  drop.empty.groups = TRUE,
  group.by.state = TRUE
)
```

## Arguments

- data:

  A light logger dataset. Expects a dataframe.

- State.colname:

  The variable or condition to be evaluated for state exctration.
  Expects a symbol. If it is not part of the data, a `State.expression`
  is required.

- State.expression:

  If `State.colname` is not part of the `data`, this expression will be
  evaluated to generate the state. The result of this expression will be
  used for grouping, so it is recommended to be factor-like. If
  `State.colname` **is** part of the `data`, this argument will be
  ignored

- Datetime.colname:

  Column name that contains the datetime. Defaults to "Datetime" which
  is automatically correct for data imported with LightLogR. Expects a
  symbol.

- handle.gaps:

  Logical whether the data shall be treated with
  [`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md).
  Is set to `FALSE` by default, due to computational costs.

- epoch:

  The epoch to use for the gapless sequence. Can be either a
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
  or a string. If it is a string, it needs to be either
  '"dominant.epoch"' (the default) for a guess based on the data or a
  valid
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

- drop.empty.groups:

  Logical. Should empty groups be dropped? Only works if `.drop = FALSE`
  has not been used with the current grouping prior to calling the
  function. Default to `TRUE`. If set to `FALSE` can lead to an error if
  factors are present in the grouping that have more levels than actual
  data. Can, however, be useful and necessary when summarizing the
  groups further, e.g. through
  [`summarize_numeric()`](https://tscnlab.github.io/LightLogR/reference/summarize_numeric.md) -
  having an empty group present is important when averaging numbers.

- group.by.state:

  Logical. Should the output be automatically be grouped by the new
  state?

## Value

a dataframe with one row per state instance. Each row will consist of
the original dataset grouping, the state column. A state.count column,
start and end Datetimes, as well as a duration of the state

## Examples

``` r
#summarizing states "photoperiod"
states <-
sample.data.environment |>
  add_photoperiod(c(48.52, 9.06)) |>
  extract_states(photoperiod.state)
states |> head(2)
#> # A tibble: 2 × 7
#> # Groups:   Id, photoperiod.state [1]
#>   Id          photoperiod.state state.count epoch start              
#>   <fct>       <chr>             <chr>       <dbl> <dttm>             
#> 1 Environment day               day 1          30 2023-08-29 06:03:23
#> 2 Environment day               day 2          30 2023-08-30 06:04:53
#> # ℹ 2 more variables: end <dttm>, duration <Duration>
states |> tail(2)
#> # A tibble: 2 × 7
#> # Groups:   Id, photoperiod.state [1]
#>   Id          photoperiod.state state.count epoch start              
#>   <fct>       <chr>             <chr>       <dbl> <dttm>             
#> 1 Participant night             night 6        10 2023-09-02 20:36:49
#> 2 Participant night             night 7        10 2023-09-03 20:34:39
#> # ℹ 2 more variables: end <dttm>, duration <Duration>
states |> summarize_numeric(c("state.count", "epoch"))
#> # A tibble: 4 × 7
#> # Groups:   Id [2]
#>   Id          photoperiod.state mean_start mean_end mean_duration        
#>   <fct>       <chr>             <time>     <time>   <Duration>           
#> 1 Environment day               06:07:08   20:39:58 52370s (~14.55 hours)
#> 2 Environment night             21:08:32   08:40:23 29169s (~8.1 hours)  
#> 3 Participant day               06:07:04   20:39:56 52372s (~14.55 hours)
#> 4 Participant night             21:08:30   08:40:20 29167s (~8.1 hours)  
#> # ℹ 2 more variables: total_duration <Duration>, episodes <int>
```

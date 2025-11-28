# Sample groups from a grouped dataset

This helper selects a subset of groups from a grouped dataset. Groups
can be drawn randomly, by ordering groups from the top or bottom
according to a summary expression, or by filtering with a custom
condition. The function is designed to work with datasets that were
grouped using
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Usage

``` r
sample_groups(
  dataset,
  n = 1,
  sample = c("top", "bottom", "random"),
  order.by = dplyr::cur_group_id(),
  condition = NULL
)
```

## Arguments

- dataset:

  A grouped dataset. Expects a data frame grouped with
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

- n:

  Number of groups to return. Defaults to 1. Ignored when `condition` is
  supplied and `n` is `NULL`.

- sample:

  Sampling strategy. Must be one of `"random"`, `"top"` (the default),
  or `"bottom"`. Alternatively, a numeric vector can be provided to
  select group positions (using bottom ordering); when numeric, `n` is
  ignored. When `condition` is provided, the `sample` value is ignored
  and conditional filtering is applied instead.

- order.by:

  Expression used to order groups when `sample` is set to `"top"` or
  `"bottom"`. Evaluated in a one-row summary for each group. Defaults to
  [`dplyr::cur_group_id()`](https://dplyr.tidyverse.org/reference/context.html),
  i.e., the group number.

- condition:

  Logical expression used to filter the summarised groups. Evaluated in
  a one-row summary for each group, which includes an `.order_value`
  column derived from `order.by`.

## Value

A grouped tibble containing only the sampled groups.

## Examples

``` r
#gives one last group (highest group id)
sample.data.environment |>
  sample_groups() |>
  dplyr::group_keys()
#> # A tibble: 1 × 1
#>   Id         
#>   <fct>      
#> 1 Participant

#gives one random group (highest group id)
sample.data.environment |>
  sample_groups(sample = "random") |>
  dplyr::group_keys()
#> # A tibble: 1 × 1
#>   Id         
#>   <fct>      
#> 1 Participant

#gives the group with the highest average melanopic EDI
sample.data.environment |>
  sample_groups(order.by = mean(MEDI)) |>
  dplyr::group_keys()
#> # A tibble: 1 × 1
#>   Id         
#>   <fct>      
#> 1 Environment

#gives the group with the lowest average melanopic EDI
sample.data.environment |>
  sample_groups(sample = "bottom", order.by = mean(MEDI)) |>
  dplyr::group_keys()
#> # A tibble: 1 × 1
#>   Id         
#>   <fct>      
#> 1 Participant

# give only groups that have a median melanopic EDI > 1000 lx
sample.data.environment |>
  sample_groups(condition = median(MEDI, na.rm = TRUE) > 1000) |>
  dplyr::group_keys()
#> # A tibble: 1 × 1
#>   Id         
#>   <fct>      
#> 1 Environment

# return only days with time above 250 lx mel EDI > 7 hours
sample.data.environment |>
  add_Date_col(group.by = TRUE) |>
  sample_groups(order.by = duration_above_threshold(MEDI, Datetime, threshold = 250),
                condition = .order_value > 7*60*60) |>
  dplyr::group_keys()
#> # A tibble: 8 × 2
#>   Id          Date      
#>   <fct>       <date>    
#> 1 Environment 2023-08-29
#> 2 Environment 2023-08-30
#> 3 Environment 2023-08-31
#> 4 Environment 2023-09-01
#> 5 Environment 2023-09-02
#> 6 Environment 2023-09-03
#> 7 Participant 2023-09-02
#> 8 Participant 2023-09-03
  
# return the 5 days with the highest time above 250 lx mel EDI
sample.data.environment |>
  add_Date_col(group.by = TRUE) |>
  sample_groups(
    n = 5,
    order.by = duration_above_threshold(MEDI, Datetime, threshold = 250),
    ) |>
  dplyr::group_keys()
#> # A tibble: 5 × 2
#>   Id          Date      
#>   <fct>       <date>    
#> 1 Environment 2023-08-30
#> 2 Environment 2023-08-31
#> 3 Environment 2023-09-01
#> 4 Environment 2023-09-02
#> 5 Environment 2023-09-03

# gives the first group
sample.data.environment |>
  sample_groups(sample = 1) |>
  dplyr::group_keys()
#> # A tibble: 1 × 1
#>   Id         
#>   <fct>      
#> 1 Environment

# gives the second group
sample.data.environment |>
  sample_groups(sample = 2) |>
  dplyr::group_keys()
#> # A tibble: 1 × 1
#>   Id         
#>   <fct>      
#> 1 Participant
  
```

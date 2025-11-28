# Plot an overview of dataset intervals with implicit missing data

Plot an overview of dataset intervals with implicit missing data

## Usage

``` r
gg_overview(
  dataset,
  Datetime.colname = Datetime,
  Id.colname = Id,
  gap.data = NULL,
  ...,
  interactive = FALSE
)
```

## Arguments

- dataset:

  A light logger dataset. Expects a `dataframe`. If not imported by
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md),
  take care to choose a sensible variable for the `x.axis.`.

- Datetime.colname:

  column name that contains the datetime. Defaults to `"Datetime"` which
  is automatically correct for data imported with
  [LightLogR](https://tscnlab.github.io/LightLogR/reference/LightLogR-package.md).
  Expects a `symbol`. Needs to be part of the `dataset`.

- Id.colname:

  The column name of the Id column (default is `Id`), needs to be in the
  `dataset`. This is also used as the y-axis variable and is the minimum
  grouping variable.

- gap.data:

  Optionally provide a `tibble` with `start` and `end` `Datetimes` of
  gaps per group. If not provided, the function uses
  [`gap_finder()`](https://tscnlab.github.io/LightLogR/reference/gap_finder.md)
  to calculate implicit missing data. This might be computationally
  intensive for large datasets and many missing data. In these cases it
  can make sense to calculate those gaps beforehand and provide them to
  the function. If an empty `tibble`
  ([`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html))
  is provided, the function will just plot the start and end dates of
  the dataset, which is computationally very fast at the cost of
  additional info.

- ...:

  Additional arguments given to the main
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html)
  used for styling depending on data within the `dataset`

- interactive:

  Should the plot be interactive? Expects a `logical`. Defaults to
  `FALSE`.

## Value

A `ggplot` object

## Examples

``` r
sample.data.environment %>% gg_overview()
```

# Filter multiple times based on a list of arguments.

`filter_Datetime_multiple()` is a wrapper around
[`filter_Datetime()`](https://tscnlab.github.io/LightLogR/reference/filter_Datetime.md)
or
[`filter_Date()`](https://tscnlab.github.io/LightLogR/reference/filter_Datetime.md)
that allows the cumulative filtering of `Datetimes` based on varying
filter conditions. It is most useful in conjunction with the `only_Id`
argument, e.g., to selectively cut off dates depending on participants
(see examples)

## Usage

``` r
filter_Datetime_multiple(
  dataset,
  arguments,
  filter_function = filter_Datetime,
  ...
)
```

## Arguments

- dataset:

  A light logger dataset

- arguments:

  A list of arguments to be passed to
  [`filter_Datetime()`](https://tscnlab.github.io/LightLogR/reference/filter_Datetime.md)
  or
  [`filter_Date()`](https://tscnlab.github.io/LightLogR/reference/filter_Datetime.md).
  each list entry must itself be a list of arguments, e.g,
  `list(start = "2021-01-01", only_Id = quote(Id == 216))`. Expressions
  have to be quoted with
  [`quote()`](https://rdrr.io/r/base/substitute.html) or
  [`rlang::expr()`](https://rlang.r-lib.org/reference/expr.html).

- filter_function:

  The function to be used for filtering, either `filter_Datetime` (the
  default) or `filter_Date`

- ...:

  Additional arguments passed to the filter function. If the `length`
  argument is provided here instead of the `argument`, it has to be
  written as a string, e.g., `length = "1 day"`, instead of
  `length = lubridate::days(1)`.

## Value

A dataframe with the filtered data

## Examples

``` r
arguments <- list(
 list(start = "2023-08-31", only_Id = quote(Id == "Participant")),
 list(end = "2023-08-31", only_Id = quote(Id == "Environment")))
 #compare the unfiltered dataset
 sample.data.environment %>% gg_overview(Id.colname = Id)

 #compare the unfiltered dataset
 sample.data.environment %>%
 filter_Datetime_multiple(arguments = arguments, filter_Date) %>%
 gg_overview(Id.colname = Id)
```

# Light exposure summary table helpers

These helpers create a publication-ready summary table for light logger
datasets. Users can either calculate the metrics, generate overview
counts, or render the complete
[gt](https://gt.rstudio.com/reference/gt.html) table.

This function creates a tibble that gives some high level information
about a dataset: How many participants are in there, the number of
participant days, how many participant days are complete above a given
threshold, how much data is missing, and (if provided) what the
photoperiod is.

## Usage

``` r
summary_overview(
  dataset,
  Variable.colname = MEDI,
  coordinates = NULL,
  location = NULL,
  site = NULL,
  Datetime.colname = Datetime,
  Id.colname = Id,
  threshold.missing = 0.2,
  programmatic.use = FALSE,
  handle.gaps = TRUE
)

summary_metrics(
  dataset,
  Variable.colname = MEDI,
  Datetime.colname = Datetime,
  Id.colname = Id,
  threshold.missing = 0.2,
  programmatic.use = FALSE,
  handle.gaps = TRUE
)

summary_table(
  dataset,
  coordinates = NULL,
  location = NULL,
  site = NULL,
  color = "grey",
  Variable.colname = MEDI,
  Datetime.colname = Datetime,
  Id.colname = Id,
  threshold.missing = 0.2,
  Variable.label = "melanopic EDI (lx)",
  histograms = TRUE
)
```

## Arguments

- dataset:

  A data frame containing light logger data.

- Variable.colname:

  Column containing light exposure values. Expects a symbol; defaults to
  `MEDI` for compatibility with the built-in datasets.

- coordinates:

  Optional numeric vector of length two containing latitude and
  longitude (in that order). If supplied, photoperiod information is
  calculated when the dataset does not already contain a `photoperiod`
  column.

- location:

  Optional location description (e.g. city name).

- site:

  Optional site description (e.g. country or study site).

- Datetime.colname:

  Column containing the timestamp information. Expects a symbol;
  defaults to `Datetime`.

- Id.colname:

  Column containing the participant identifier. Expects a symbol;
  defaults to `Id`.

- threshold.missing:

  Proportion of missing data (per participant-day) tolerated before a
  day is considered incomplete.

- programmatic.use:

  Whether the function is used by another function. This determines the
  number of columns to be output. Default is `FALSE`

- handle.gaps:

  Whether gaps in the data should be handled. Sets the argument in
  [`remove_partial_data()`](https://tscnlab.github.io/LightLogR/reference/remove_partial_data.md).
  Default is `TRUE`.

- color:

  Color used for histogram accents in the metrics section.

- Variable.label:

  Label used in the table footnote to describe the light variable.

- histograms:

  Logical indicating whether histogram spark lines should be added for
  metrics where applicable.

## Value

A tibble with overview metrics (`type`, `name`, `mean`, `SD`, `min`,
`max`, `plot`). A `location_string` attribute is attached to the result
for use in `summary_table()`. If `programmatic.use = FALSE`, `type`,
`SD` and `plot` are removed.

A tibble with summarized metrics across participant-days and
participant-level stability measures. Columns are compatible with
`summary_table()`.

A [gt](https://gt.rstudio.com/reference/gt.html) table.

## Details

The function is used within `summary_table()`.

## Examples

``` r
sample.data.environment |> summary_overview()
#> # A tibble: 4 × 4
#>   name                mean   min   max
#>   <chr>              <dbl> <dbl> <dbl>
#> 1 Participants           2    NA    NA
#> 2 Participant-days      12     6     6
#> 3 Days ≥80% complete    12     6     6
#> 4 Missing/Irregular      0     0     0
sample.data.irregular |> summary_overview()
#> # A tibble: 4 × 4
#>   name                mean   min   max
#>   <chr>              <dbl> <dbl> <dbl>
#> 1 Participants        1    NA    NA   
#> 2 Participant-days    2     2     2   
#> 3 Days ≥80% complete  0     0     0   
#> 4 Missing/Irregular   0.49  0.49  0.49
# \donttest{
sample.data.environment |> 
filter_Date(length = "3 days") |> 
summary_metrics()
#> # A tibble: 15 × 4
#>    name                         mean       min        max
#>    <chr>                       <dbl>     <dbl>      <dbl>
#>  1 brightest_10h_mean      12468.       58.9    34659.   
#>  2 brightest_10h_midpoint  51523.    48278      54084    
#>  3 darkest_5h_mean             0         0          0    
#>  4 darkest_5h_midpoint      8986      8978       8994    
#>  5 dose                   152858.     2197.    420306.   
#>  6 duration_above_1000     24635       590      47370    
#>  7 duration_above_250      29623.     5810      49350    
#>  8 duration_below_1        32978.    27990      39260    
#>  9 duration_within_1-10     6338.     1140      17530    
#> 10 first_timing_above_250  29791     23648      36184    
#> 11 last_timing_above_250   77921     72128      84264    
#> 12 mean_timing_above_250   50922.    47993      54354    
#> 13 period_above_250        25093.     1230      49350    
#> 14 interdaily_stability        0.684     0.598      0.770
#> 15 intradaily_variability      0.614     0.209      1.02 
  # }

#sample.data.environment |> summary_table(coordinates = c(47,9))
```

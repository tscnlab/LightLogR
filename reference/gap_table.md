# Tabular summary of data and gaps in all groups

`gap_table()` creates a
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html) with one row per
group, summarizing key gap and gap-related information about the
dataset. These include the available data, total duration, number of
gaps, missing implicit and explicit data, and, optionally, irregular
data.

## Usage

``` r
gap_table(
  dataset,
  Variable.colname = MEDI,
  Variable.label = "melanopic EDI",
  title = "Summary of available and missing data",
  Datetime.colname = Datetime,
  epoch = "dominant.epoch",
  full.days = TRUE,
  include.implicit.gaps = TRUE,
  check.irregular = TRUE,
  get.df = FALSE
)
```

## Arguments

- dataset:

  A light logger dataset. Needs to be a dataframe.

- Variable.colname:

  Column name of the variable to check for NA values. Expects a symbol.

- Variable.label:

  Clear name of the variable. Expects a string

- title:

  Title string for the table

- Datetime.colname:

  The column that contains the datetime. Needs to be a `POSIXct` and
  part of the dataset.

- epoch:

  The epoch to use for the gapless sequence. Can be either a
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
  or a string. If it is a string, it needs to be either
  '"dominant.epoch"' (the default) for a guess based on the data or a
  valid
  [`lubridate::duration()`](https://lubridate.tidyverse.org/reference/duration.html)
  string, e.g., `"1 day"` or `"10 sec"`.

- full.days:

  If `TRUE`, the gapless sequence will include the whole first and last
  day where there is data.

- include.implicit.gaps:

  Logical. Whether to expand the datetime sequence and search for
  implicit gaps, or not. Default is `TRUE`. If no `Variable.colname` is
  provided, this argument will be ignored. **If there are implicit gaps,
  gap calculation can be incorrect whenever there are missing explicit
  gaps flanking implicit gaps!**

- check.irregular:

  Logical on whether to include irregular data in the summary, i.e. data
  points that do not fall on the regular sequence.

- get.df:

  Logical whether the dataframe should be returned instead of a
  [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table

## Value

A gt table about data and gaps in the dataset

## Examples

``` r
sample.data.environment |> dplyr::filter(MEDI <= 50000) |> gap_table()


  


Summary of available and missing data
```

Variable: melanopic EDI

Data

Missing

Regular

Irregular

Range

Interval

Gaps

Implicit

Explicit

Time

%

n¹

n^(1,2)

Time

n¹

Time

N

ø

øn¹

Time

%

n¹

Time

%

n¹

Time

%

n¹

**Overall**

1w 4d 6h 42m 50s

94.0%³

67,003

0

1w 5d

69,120

30; 10

200

7m 8s

15

17h 17m 10s

6.0%³

2,117

17h 17m 10s

6.0%³

2,117

0s

0.0%³

0

Environment

5d 6h 53m 30s

88.1%

15,227

0

6d

17,280

30s

148

6m 56s

14

17h 6m 30s

11.9%

2,053

17h 6m 30s

11.9%

2,053

0s

0.0%

0

Participant

5d 23h 49m 20s

99.9%

51,776

0

6d

51,840

10s

52

12s

1

10m 40s

0.1%

64

10m 40s

0.1%

64

0s

0.0%

0

¹ Number of (missing or actual) observations

² If n \> 0: it is possible that the other summary statistics are
affected, as they are calculated based on the most prominent interval.

³ Based on times, not necessarily number of observations

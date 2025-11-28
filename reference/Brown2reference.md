# Add Brown et al. (2022) reference illuminance to a dataset

Adds several columns to a light logger dataset. It requires a column
that contains the Brown states, e.g. "daytime", "evening", and "night".
From that the function will add a column with the recommended
illuminance, a column that checks if the illuminance of the dataset is
within the recommended illuminance levels, and a column that gives a
label to the reference.

## Usage

``` r
Brown2reference(
  dataset,
  MEDI.colname = MEDI,
  Brown.state.colname = State.Brown,
  Brown.rec.colname = Reference,
  Reference.label = "Brown et al. (2022)",
  overwrite = FALSE,
  ...
)
```

## Arguments

- dataset:

  A dataframe that contains a column with the Brown states

- MEDI.colname:

  The name of the column that contains the MEDI values which are used
  for checks against the Brown reference illuminance. Must be part of
  the dataset.

- Brown.state.colname:

  The name of the column that contains the Brown states. Must be part of
  the dataset.

- Brown.rec.colname:

  The name of the column that will contain the recommended illuminance.
  Must not be part of the dataset, otherwise it will throw an error.

- Reference.label:

  The label that will be used for the reference. Expects a `character`
  scalar.

- overwrite:

  If `TRUE` (defaults to `FALSE`), the function will overwrite the
  `Brown.rec.colname` column if it already exists.

- ...:

  Additional arguments that will be passed to
  [`Brown_rec()`](https://tscnlab.github.io/LightLogR/reference/Brown_rec.md)
  and
  [`Brown_check()`](https://tscnlab.github.io/LightLogR/reference/Brown_check.md).
  This is only relevant to correct the names of the daytime states or
  the thresholds used within these states. See the documentation of
  these functions for more information.

## Value

A dataframe on the basis of the `dataset` that contains the added
columns.

## Details

On a lower level, the function uses
[`Brown_rec()`](https://tscnlab.github.io/LightLogR/reference/Brown_rec.md)
and
[`Brown_check()`](https://tscnlab.github.io/LightLogR/reference/Brown_check.md)
to create the required information.

## References

https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001571

## See also

Other Brown:
[`Brown_check()`](https://tscnlab.github.io/LightLogR/reference/Brown_check.md),
[`Brown_cut()`](https://tscnlab.github.io/LightLogR/reference/Brown_cut.md),
[`Brown_rec()`](https://tscnlab.github.io/LightLogR/reference/Brown_rec.md),
[`sleep_int2Brown()`](https://tscnlab.github.io/LightLogR/reference/sleep_int2Brown.md)

## Examples

``` r
#add Brown reference illuminance to some sample data
testdata <- tibble::tibble(MEDI = c(100, 10, 1, 300),
                  State.Brown = c("day", "evening", "night", "day"))
Brown2reference(testdata)
#> # A tibble: 4 × 6
#>    MEDI State.Brown Reference Reference.check Reference.difference
#>   <dbl> <chr>           <dbl> <lgl>                          <dbl>
#> 1   100 day               250 FALSE                           -150
#> 2    10 evening            10 TRUE                               0
#> 3     1 night               1 TRUE                               0
#> 4   300 day               250 TRUE                              50
#> # ℹ 1 more variable: Reference.label <chr>
```

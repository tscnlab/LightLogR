# Check whether a value is within the recommended illuminance/MEDI levels by Brown et al. (2022)

This is a lower level function. It checks a given value against a
threshold for the states given by Brown et al. (2022). The function is
vectorized. For `day` the threshold is a lower limit, for `evening` and
`night` the threshold is an upper limit.

## Usage

``` r
Brown_check(
  value,
  state,
  Brown.day = "day",
  Brown.evening = "evening",
  Brown.night = "night",
  Brown.day.th = 250,
  Brown.evening.th = 10,
  Brown.night.th = 1
)
```

## Arguments

- value:

  Illuminance value to check against the recommendation. needs to be
  numeric, can be a vector.

- state:

  The state from Brown et al. (2022). Needs to be a character vector (or
  factor vector) with the same length as `value`.

- Brown.day, Brown.evening, Brown.night:

  The names of the states from Brown et al. (2022). These are the
  default values (`"day"`, `"evening"`, `"night"`), but can be changed
  if the names in `state` are different. Needs to be a character scalar.

- Brown.day.th, Brown.evening.th, Brown.night.th:

  The thresholds for the states from Brown et al. (2022). These are the
  default values (`250`, `10`, `1`), but can be changed if the
  thresholds should be different. Needs to be a numeric scalar.

## Value

A logical vector with the same length as `value` that indicates whether
the value is within the recommended illuminance levels.

## References

https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001571

## See also

Other Brown:
[`Brown2reference()`](https://tscnlab.github.io/LightLogR/reference/Brown2reference.md),
[`Brown_cut()`](https://tscnlab.github.io/LightLogR/reference/Brown_cut.md),
[`Brown_rec()`](https://tscnlab.github.io/LightLogR/reference/Brown_rec.md),
[`sleep_int2Brown()`](https://tscnlab.github.io/LightLogR/reference/sleep_int2Brown.md)

## Examples

``` r
states <- c("day", "evening", "night", "day")
values <- c(100, 10, 1, 300)
Brown_check(values, states)
#> [1] FALSE  TRUE  TRUE  TRUE
Brown_check(values, states, Brown.day.th = 100)
#> [1] TRUE TRUE TRUE TRUE
```

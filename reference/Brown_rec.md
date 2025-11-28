# Set the recommended illuminance/MEDI levels by Brown et al. (2022)

This is a lower level function. It sets the recommended illuminance/MEDI
levels by Brown et al. (2022) for a given state. The function is
vectorized.

## Usage

``` r
Brown_rec(
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

- state:

  The state from Brown et al. (2022). Needs to be a character vector.

- Brown.day, Brown.evening, Brown.night:

  The names of the states from Brown et al. (2022). These are the
  default values (`"day"`, `"evening"`, `"night"`), but can be changed
  if the names in `state` are different. Needs to be a character scalar.

- Brown.day.th, Brown.evening.th, Brown.night.th:

  The thresholds for the states from Brown et al. (2022). These are the
  default values (`250`, `10`, `1`), but can be changed if the
  thresholds should be different. Needs to be a numeric scalar.

## Value

A dataframe with the same length as `state` that contains the
recommended illuminance/MEDI levels.

## References

https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001571

## See also

Other Brown:
[`Brown2reference()`](https://tscnlab.github.io/LightLogR/reference/Brown2reference.md),
[`Brown_check()`](https://tscnlab.github.io/LightLogR/reference/Brown_check.md),
[`Brown_cut()`](https://tscnlab.github.io/LightLogR/reference/Brown_cut.md),
[`sleep_int2Brown()`](https://tscnlab.github.io/LightLogR/reference/sleep_int2Brown.md)

## Examples

``` r
states <- c("day", "evening", "night")
Brown_rec(states)
#> [1] 250  10   1
Brown_rec(states, Brown.day.th = 100)
#> [1] 100  10   1
```

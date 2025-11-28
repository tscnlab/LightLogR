# Create a state column that cuts light levels into sections by Brown et al. (2022)

This is a convenience wrapper arount
[`cut()`](https://rdrr.io/r/base/cut.html) and
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).
It creates a state column dividing a light column into recommended
levels by Brown et al. (2022). Cuts can be adjusted or extended with
`vector_cuts` and `vector_labels`

## Usage

``` r
Brown_cut(
  dataset,
  MEDI.colname = MEDI,
  New.state.colname = state,
  vector_cuts = c(-Inf, 1, 10, 250, Inf),
  vector_labels = "default",
  overwrite = TRUE
)
```

## Arguments

- dataset:

  A light exposure dataframe

- MEDI.colname:

  The colname containing melanopic EDI values (or, alternatively,
  Illuminance). Defaults to `MEDI`. Expects a symbol.

- New.state.colname:

  Name of the new column that will contain the cut data. Expects a
  symbol.

- vector_cuts:

  Numeric vector of breaks for the cuts.

- vector_labels:

  Vector of labels for the cuts. Must be one entry shorter than
  `vector_cuts`. `"default"` will produce nice labels for the default
  setting of `vector_cuts` (and throw an error otherwise).

- overwrite:

  Logical. Should the `New.state.colname` overwrite a preexisting column
  in the dataset

## Value

The input dataset with an additional (or overwritten) column containing
a cut light vector

## References

https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001571

## See also

Other Brown:
[`Brown2reference()`](https://tscnlab.github.io/LightLogR/reference/Brown2reference.md),
[`Brown_check()`](https://tscnlab.github.io/LightLogR/reference/Brown_check.md),
[`Brown_rec()`](https://tscnlab.github.io/LightLogR/reference/Brown_rec.md),
[`sleep_int2Brown()`](https://tscnlab.github.io/LightLogR/reference/sleep_int2Brown.md)

## Examples

``` r
sample.data.environment |>
Brown_cut(vector_labels = c("0-1lx", "1-10lx", "10-250lx", "250lx-Inf")) |>
dplyr::count(state)
#> # A tibble: 8 × 3
#> # Groups:   Id [2]
#>   Id          state         n
#>   <fct>       <fct>     <int>
#> 1 Environment 0-1lx      6768
#> 2 Environment 1-10lx      247
#> 3 Environment 10-250lx    523
#> 4 Environment 250lx-Inf  9742
#> 5 Participant 0-1lx     20315
#> 6 Participant 1-10lx     5781
#> 7 Participant 10-250lx  15892
#> 8 Participant 250lx-Inf  9852
```

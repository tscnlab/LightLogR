# Normalize counts between sensor outputs

This is a niche helper function to normalize counts. Some sensors
provide raw counts and gain levels as part of their output. In some
cases it is desirable to compare counts between sensors, e.g., to gauge
daylight outside by comparing UV counts to photopic counts (a high ratio
of UV/Pho indicates outside daylight). Or to gauge daylight inside by
comparing IR counts to photopic counts (a high ratio of IR/Pho with a
low ratio of UV/Pho indicates daylight in the context of LED or
fluorescent lighting). The user can provide their own gain ratiotable,
or use a table provided for a sensor in the `gain.ratio.table` dataset
from `LightLogR`.

## Usage

``` r
normalize_counts(dataset, gain.columns, count.columns, gain.ratio.table)
```

## Arguments

- dataset:

  a `data.table` containing gain and count columns.

- gain.columns:

  a `character` vector of columns in the `dataset` containing a gain
  setting. Columns must not repeat.

- count.columns:

  a `character` vector of columns in the `dataset` containing raw count
  data. Must be of the same length as `gain.columns`, and the order must
  conform to the order in `gain.columns`.

- gain.ratio.table:

  a two-column tibble containing `gain` and `gain.ratio` information.
  Can be provided by the user or use the `gain.ratio.table` dataset.

## Value

an extended dataset with new columns containing normalized counts

## See also

Other Spectrum:
[`spectral_integration()`](https://tscnlab.github.io/LightLogR/reference/spectral_integration.md),
[`spectral_reconstruction()`](https://tscnlab.github.io/LightLogR/reference/spectral_reconstruction.md)

## Examples

``` r
example.table <-
tibble::tibble(
uvGain = c(4096, 1024, 2),
visGain = c(4096, 4096, 4096),
irGain = c(2,2,2),
uvValue = c(692, 709, 658),
visValue = c(128369, 129657, 128609),
irValue = c(122193, 127113, 124837))

gain.columns = c("uvGain", "visGain", "irGain")
count.columns = c("uvValue", "visValue", "irValue")

example.table |>
normalize_counts(gain.columns, count.columns, gain.ratio.tables$TSL2585)
#> # A tibble: 3 × 9
#>   uvGain visGain irGain uvValue visValue irValue uvValue.normalized
#>    <dbl>   <dbl>  <dbl>   <dbl>    <dbl>   <dbl>              <dbl>
#> 1   4096    4096      2     692   128369  122193               27.3
#> 2   1024    4096      2     709   129657  127113               95.6
#> 3      2    4096      2     658   128609  124837            41433. 
#> # ℹ 2 more variables: visValue.normalized <dbl>, irValue.normalized <dbl>
```

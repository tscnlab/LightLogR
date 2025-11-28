# Adjust device imports or make your own

Adjust device imports or make your own

## Usage

``` r
import_adjustment(import_expr)
```

## Arguments

- import_expr:

  A named list of import expressions. The basis for `LightLogR`'s import
  functions is the included dataset
  [`ll_import_expr()`](https://tscnlab.github.io/LightLogR/reference/ll_import_expr.md).
  If this function were to be given that exact dataset, and bound to a
  variable called `import`, it would be identical to the `import`
  function. See `details`.

## Value

A list of import functions

## Details

This function should only be used with some knowledge of how expressions
work in R. The minimal required output for an expression to work as
expected, it must lead to a data frame containing a `Datetime` column
with the correct time zone. It has access to all arguments defined in
the description of
[`import_Dataset()`](https://tscnlab.github.io/LightLogR/reference/import_Dataset.md).
The `...` argument should be passed to whatever csv reader function is
used, so that it works as expected. Look at `ll_import_expr()$ActLumus`
for a quite minimal example.

## Examples

``` r
#create a new import function for the ActLumus device, same as the old
new_import <- import_adjustment(ll_import_expr())
#the new one is identical to the old one in terms of the function body
identical(body(import$ActLumus), body(new_import$ActLumus))
#> [1] TRUE

#change the import expression for the ActLumus device to add a message at the top
new_import_expr <- ll_import_expr()
new_import_expr$ActLumus[[6]] <-
rlang::expr({ cat("**This is a new import function**\n")
data
})
new_import <- import_adjustment(new_import_expr)
filepath <- 
system.file("extdata/205_actlumus_Log_1020_20230904101707532.txt.zip", 
            package = "LightLogR")
#Now, a message is printed when the import function is called
data <- new_import$ActLumus(filepath, auto.plot = FALSE)
#> Multiple files in zip: reading '205_actlumus_Log_1020_20230904101707532.txt'
#> **This is a new import function**
#> 
#> Successfully read in 61'016 observations across 1 Ids from 1 ActLumus-file(s).
#> Timezone set is UTC.
#> 
#> First Observation: 2023-08-28 08:47:54
#> Last Observation: 2023-09-04 10:17:04
#> Timespan: 7.1 days
#> 
#> Observation intervals: 
#>   Id                                          interval.time     n pct  
#> 1 205_actlumus_Log_1020_20230904101707532.txt 10s           61015 100% 
```

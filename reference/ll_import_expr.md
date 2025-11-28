# Get the import expression for a device

Returns the import expression for all device in LightLogR.

## Usage

``` r
ll_import_expr()
```

## Value

A list of import expressions for all supported devices

## Details

These expressions are used to import and prepare data from specific
devices. The list is made explicit, so that a user, requiring slight
changes to the import functions, (e.g., because a timestamp is formatted
differently) can modify or add to the list. The list can be turned into
a fully functional import function through
[`import_adjustment()`](https://tscnlab.github.io/LightLogR/reference/import_adjustment.md).

## See also

[import_Dataset](https://tscnlab.github.io/LightLogR/reference/import_Dataset.md),
[import_Dataset](https://tscnlab.github.io/LightLogR/reference/import_Dataset.md)

## Examples

``` r
ll_import_expr()[1]
#> $ActLumus
#> {
#>     first_file <- filename[1]
#>     rows_to_skip <- detect_starting_row(first_file, locale = locale, 
#>         column_names = "DATE/TIME", n_max = 250)
#>     data <- suppressMessages(readr::read_delim(filename, skip = rows_to_skip, 
#>         delim = ";", n_max = n_max, id = "file.name", locale = locale, 
#>         name_repair = "universal", ...))
#>     data <- data %>% dplyr::rename(Datetime = DATE.TIME, MEDI = MELANOPIC.EDI) %>% 
#>         dplyr::mutate(Datetime = Datetime %>% lubridate::dmy_hms(tz = tz))
#> }
#> 
```

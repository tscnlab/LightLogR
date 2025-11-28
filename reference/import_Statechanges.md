# Import data that contain `Datetimes` of `Statechanges`

Auxiliary data greatly enhances data analysis. This function allows the
import of files that contain `Statechanges`, i.e., specific time points
of when a `State` (like `sleep` or `wake`) starts.

## Usage

``` r
import_Statechanges(
  filename,
  path = NULL,
  sep = ",",
  dec = ".",
  structure = c("wide", "long"),
  Datetime.format = "ymdHMS",
  tz = "UTC",
  State.colnames,
  State.encoding = State.colnames,
  Datetime.column = Datetime,
  Id.colname,
  State.newname = State,
  Id.newname = Id,
  keep.all = FALSE,
  silent = FALSE
)
```

## Arguments

- filename:

  Filename(s) for the Dataset. Can also contain the filepath, but `path`
  must then be `NULL`. Expects a `character`. If the vector is longer
  than `1`, multiple files will be read in into one Tibble.

- path:

  Optional path for the dataset(s). `NULL` is the default. Expects a
  `character`.

- sep:

  String that separates columns in the import file. Defaults to `","`.

- dec:

  String that indicates a decimal separator in the import file. Defaults
  to `"."`.

- structure:

  String that specifies whether the import file is in the `long` or
  `wide` format. Defaults to `"wide"`.

- Datetime.format:

  String that specifies the format of the `Datetimes` in the file. The
  default `"ymdHMS"` specifies a format like "2023-07-10 10:00:00". In
  the function,
  [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html)
  does the actual conversion - the documentation can be searched for
  valid inputs.

- tz:

  Timezone of the data. `"UTC"` is the default. Expects a `character`.
  You can look up the supported timezones with
  [`OlsonNames()`](https://rdrr.io/r/base/timezones.html).

- State.colnames:

  Column name or vector of column names (the latter only in the `wide`
  format). Expects a `character`.

  - In the `wide` format, the column names indicate the `State` and must
    contain `Datetimes`. The columns will be pivoted to the columns
    specified in `Datetime.column` and `State.newname`.

  - In the `long` format, the column contains the `State`

- State.encoding:

  In the `wide` format, this enables recoding the column names to state
  names, if there are any differences. The default uses the
  `State.colnames` argument. Expects a `character` (vector) with the
  same length as `State.colnames`.

- Datetime.column:

  Symbol of the `Datetime` column (which is also the default).

  - In the `wide` format, this is the newly created column from the
    `Datetimes` in the `State.colnames`.

  - In the `long` format, this is the existing column that contains the
    `Datetimes`.

- Id.colname:

  Symbol of the column that contains the `ID` of the subject.

- State.newname:

  Symbol of the column that will contain the `State` of the subject. In
  the `wide` format, this is the newly created column from the
  `State.colnames`. In the `long` format, this argument is used to
  rename the `State` column.

- Id.newname:

  Column name used for renaming the `Id.colname` column.

- keep.all:

  Logical that specifies whether all columns should be kept in the
  output. Defaults to `FALSE`.

- silent:

  Logical that specifies whether a summary of the imported data should
  be shown. Defaults to `FALSE`.

## Value

a dataset with the `ID`, `State`, and `Datetime` columns. May contain
additional columns if `keep.all` is `TRUE`.

## Details

Data can be present in the long or wide format.

- In the `wide` format, multiple `Datetime` columns indicate the state
  through the column name. These get pivoted to the `long` format and
  can be recoded through the `State.encoding` argument.

- In the `long` format, one column indicates the `State`, while the
  other gives the `Datetime`.

## Examples

``` r
#get the example file from within the package
path <- system.file("extdata/",
package = "LightLogR")
file.sleep <- "205_sleepdiary_all_20230904.csv"

#import Data in the wide format (sleep/wake times)
import_Statechanges(file.sleep, path,
Datetime.format = "dmyHM",
State.colnames = c("sleep", "offset"),
State.encoding = c("sleep", "wake"),
Id.colname = record_id,
sep = ";",
dec = ",")
#> 
#> Successfully read in 14 observations across 1 Ids from 1 Statechanges-file(s).
#> Timezone set is UTC.
#> 
#> First Observation: 2023-08-28 23:20:00
#> Last Observation: 2023-09-04 07:25:00
#> Timespan: 6.3 days
#> 
#> Observation intervals: 
#>    Id    interval.time             n pct  
#>  1 205   34860s (~9.68 hours)      1 8%   
#>  2 205   35520s (~9.87 hours)      1 8%   
#>  3 205   35700s (~9.92 hours)      1 8%   
#>  4 205   36000s (~10 hours)        1 8%   
#>  5 205   36900s (~10.25 hours)     1 8%   
#>  6 205   37020s (~10.28 hours)     1 8%   
#>  7 205   37920s (~10.53 hours)     1 8%   
#>  8 205   45780s (~12.72 hours)     1 8%   
#>  9 205   48480s (~13.47 hours)     1 8%   
#> 10 205   49200s (~13.67 hours)     1 8%   
#> # ℹ 3 more rows
#> # A tibble: 14 × 3
#> # Groups:   Id [1]
#>    Id    State Datetime           
#>    <fct> <chr> <dttm>             
#>  1 205   sleep 2023-08-28 23:20:00
#>  2 205   wake  2023-08-29 09:37:00
#>  3 205   sleep 2023-08-29 23:40:00
#>  4 205   wake  2023-08-30 09:21:00
#>  5 205   sleep 2023-08-30 23:15:00
#>  6 205   wake  2023-08-31 09:47:00
#>  7 205   sleep 2023-08-31 23:15:00
#>  8 205   wake  2023-09-01 09:30:00
#>  9 205   sleep 2023-09-01 23:10:00
#> 10 205   wake  2023-09-02 09:10:00
#> 11 205   sleep 2023-09-02 22:55:00
#> 12 205   wake  2023-09-03 08:47:00
#> 13 205   sleep 2023-09-03 21:30:00
#> 14 205   wake  2023-09-04 07:25:00

#import in the long format (Comments on sleep)
import_Statechanges(file.sleep, path,
                   Datetime.format = "dmyHM",
                   State.colnames = "comments",
                   Datetime.column = sleep,
                   Id.colname = record_id,
                   sep = ";",
                   dec = ",", structure = "long")
#> 
#> Successfully read in 7 observations across 1 Ids from 1 Statechanges-file(s).
#> Timezone set is UTC.
#> 
#> First Observation: 2023-08-28 23:20:00
#> Last Observation: 2023-09-03 21:30:00
#> Timespan: 5.9 days
#> 
#> Observation intervals: 
#>   Id    interval.time             n pct  
#> 1 205   81300s (~22.58 hours)     1 17%  
#> 2 205   84900s (~23.58 hours)     1 17%  
#> 3 205   85500s (~23.75 hours)     1 17%  
#> 4 205   86100s (~23.92 hours)     1 17%  
#> 5 205   86400s (~1 days)          1 17%  
#> 6 205   87600s (~1.01 days)       1 17%  
#> # A tibble: 7 × 3
#> # Groups:   Id [1]
#>   Id    State                                                Datetime           
#>   <fct> <chr>                                                <dttm>             
#> 1 205   Slept longer than usual since my kids are on Summer… 2023-08-28 23:20:00
#> 2 205   no                                                   2023-08-29 23:40:00
#> 3 205   Kids slept in my bed                                 2023-08-30 23:15:00
#> 4 205   none                                                 2023-08-31 23:15:00
#> 5 205   woke Up and could Not Fall asleep. went to the dini… 2023-09-01 23:10:00
#> 6 205   none                                                 2023-09-02 22:55:00
#> 7 205   no                                                   2023-09-03 21:30:00
```

# LightLogR 0.10.0 **High noon**

## General improvements and new functions

* `summary_table()`, `summary_overview()`, and `summary_metrics()` are a great set of **new functions** that give quick summaries across the whole dataset. Care has to be taken with these functions, as they are high level and will not perform well with a higly fragmented/irregular dataset.

* `sample_groups()` is a **new function** that makes it easy to reduce the number of groups either by random sampling, flexible ordering, or based on a condition.

* `style_time()` is a **new convenience function** that takes datetimes, times, or numeric input and outputs a clean time format (e.g., "03:45"). This is primarily used to style times in tables or plots.

* `format_coordinates()` is a **new convenience function** that takes a coordinates and formats them nicely for plots and tables.

* `Datetime2Time()` now allows for `circular` time through a sine-conversion of time of day - this is especially useful for averaging of times that (can) cross midnight, such as bed-times.

* `Circular2Time()` is a **new function** that back-converts `circular` time columns to `hms` time. See the package `{circular}` for details on how `circular` columns work.

* `remove_partial_data()` allows to specify a minimum duration of available data. Simply supply a negative duration to the `threshold.missing` argument. E.g., `"-20 hours"` will only keep groups with at least 20 hours of data. While this was easy before in case of groups of known total duration, 24 hours (e.g., simply set the threshold to 4 hours to get to 20 hours of data), it was not possible for groups of unknown total duration.

* `import` functions now support a `version` argument. If there are multiple known formats for one supported device, the version can be changed. As of now, this is the case with the `VEET` device, which changed its format slightly with `v2.1.17`. This argument is also used for the `Actiwatch Spectrum`, which requires several adjustments for a German locale (beyond simple adjustment of locale settings). #65

* `supported_versions()` is a **new function** that provides an overview which device versions are supported.

* `add_Date_col()` gained the `as.count` argument. If set to true, it will output the number of days since start. The basis are calendar days.

* `add_states()` now 
  - allows for `start` and `end` variables to be of class `Interval` - this makes the function ready to work with output from `sc2interal()` or `sleep_int2Brown()`.
  - works as expected when `force.tz = TRUE`, i.e., the timestamp in the states dataset is forced to the timezone of the receiving time-series dataset. That is useful, e.g., when you know that the timestamp is correct, but was imported ad `UTC` by default.

* Added many new resources to the documentation webpage (accessible through the nav menu), including an interactive online course for `LightLogR`.

* `filter_Datetime()` & `filter_Date()`:
  - When only `length` is provided, but not `start` nor `end`, the functions now respect grouping, i.e., the length will be taken from the first record (or last in the case of `from_start = FALSE`) within each group.
  - the order of arguments now makes more sense in a typical use case, with `length` being the first argument taken, then `start` and `end`

* `gg_states()` now
  - allows for an individual height of the state indicators through `ymin` and `ymax` arguments
  - allows to calculate arbitrary metrics from the dataset to be used in the plot through the `extract.metrics` argument.
  - combining the two above allows for powerful visualizations, where, e.g., the height of a status indicator is determined by a summary metric, like the median
  - allows for a non-standard `Datetime` column through the `Datetime.colname` argument.

* `gg_photoperiod()` now 
  - allows for an individual height of the state indicators through `ymin` and `ymax` arguments
  - allows for a non-standard `Datetime` column through the `Datetime.colname` argument.

## Bug fixes and small stuff

* The `plotly` package was moved to suggested dependency, as it only covers an edge case.

* The `janitor` package is no longer a dependency, as a simpler version to find duplicates during import was implemented.

* `VEET` devices import much faster now, thanks to an efficient way to construct the data table. Thanks to @ThomasKraft for raising this issue! #66

* `gg_states()` replaced the function `gg_state()` for more consistent naming with other `states` functions.

* `gg_day()` and `gg_days()` now have the `y.axis` variable and the `geom` as first two arguments, putting the most often used arguments to the front.

* New device import: `MiEye` from Circadian Health Innovations. There are two known datetime formats for the device: `ymd HMS`, and `dmy HMS`. Both are parsed.

* More flexible import for `ActLumus` devices: Data can start at any line. For computational efficiency, it will determine the correct starting row in the first file provided (`filenames[1]`) and use it for all files provided for import.

* `log_zero_inflated()` and `exp_zero_inflated()` have an updated reference.

* `Brown_check()` now also takes factor vectors for `state`. This affects the `Brown` upstream functions that use `Brown_check()`.

* `gg_heatmap()` has gained a `facetting` variable to remove facetting altogether. Default is `TRUE`.

* Standard `y.axis.label` of visualization functions is now `Melanopic EDI (lx)`. Affects `gg_day()`, `gg_days()`, and `gg_heatmap()`.

* `gg_day()`: standard `x.axis.label` is `Local time (HH:MM)`.

* `aggregate_Date()` and `aggregate_Datetime()` now contain a warning for `...` about partial matching of argument names.

* `sleep_int2Brown()` will sensibly fill in values for columns in the `evening`, should the state dataset contain more than `Interval` and `Sleep` columns.

# LightLogR 0.9.3

* added a Newsletter section to the package page

* added a test-coverage workflow and badge

* `extract_metric()` throws no error if the original data is already grouped by the `identifying.colname`

* `gap_table()` no longer throws an error if the package `gt` is not installed. `gt` is now added as a dependency.

# LightLogR 0.9.2

* minor tweaks without end-user relevance for CRAN release.

# LightLogR 0.9.1

* `dose()` is a new metric function to calculate light dose (or any other kind of dosage)

* `add_Date_col()` is a new convenience function to add a Date column to the dataset, optionally showing the weekday.

* `Datetime2Time()` is a new convenience function that is used in other functions that average over datetimes, which is often more sensible over times.

* reworked the README file to reflect some of the features `LightLogR` has gained over time

* `summarize_numeric()` now calculates `total_duration` correctly, even when the prefix is removed.

* added the `sample.data.irregular` internal dataset

* removed `LYS` wearable sample file, due to package size limitations

* `add_Time_col()` replaces `create_Time_data()`, also, the new column is called `Time` by default instead of `Time.data`

* `extract_clusters()` has the option to show the cluster condition in the output with `add.label = TRUE`, e.g., `MEDI>500|d≥30min|i≤5min` for clusters of melanopic EDI larger than 500, at least 30 minutes long (`d`), allowing interruptions of up to 5 minutes at a time (`i`).

* `add_clusters()` now drops empty groups, which has led to warnings before

* Added many more unit tests - **888** and counting!

* Removed a nasty bug in the internal functions that could lead to a shift in how dominant epochs are assigned to durations when groups are dropped due to singular observations

* `gapless_datetimes()`: fixed a bug that prohibited the use of durations. Had downstream effects on basically all gap functions

* `extract_gaps()` now warns correctly if there are implicit gaps with a non-default epoch 

* `import` has the option to remove data before a specified date. Default is `not.before = 2001-01-01`. Some devices fall back to a time stamp in the year 2000 after the battery drained completely. This makes the import problematic in terms of gap searching.

* `extract_states()` has the option not to group by the extracted state.

* `extract_clusters()` and `extract_states()` do not drop empty groups, which is important for summaries. `extract_clusters()` does it by default, `extract_states()` does not.

* `summarize_numeric()` has the option to show zero-instances of groups. Helpful to make certain groups with zero instances are not dropped, especially in a chain with `mean_daily()`

* `mean_daily()`: Automatic conversion to weekdays from dates. Further it has the option to replace NA with zeros before calculating mean daily values

# LightLogR 0.9.0 `Sunrise`

This is a huge update for `LightLogR`, bringing many new features and twenty-two new functions

## New functions & datasets

### Light spectrum

* `spectral_reconstruction()`:reconstruct a spectral power distribution (SPD) from sensor channels that provide (normalized) counts, and a calibration matrix. Examples for such devices include the the *ActLumus from Condor instruments*, or the *VEET from Meta Reality labs* (after normalization of counts, e.g., through `normalize_counts()`)

* `alphaopic.action.spectra`: New dataset containing alphaopic action spectra (CIE S026) plus the photopic action spectrum in 1-nm wavelength steps.

* `spectral_integration()`: integrate over all or just parts of the spectrum, including the option to weigh the spectrum with an action spectrum (e.g., from `alphaopic.action.spectra`)

### Missing data

* `remove_partial_data()`: remove groups with a below than user-specified amount of data

* `extract_gaps()`: provides a start and end times, as well as durations for all gaps in the dataset. 

* `has_gaps()`, `has_irregulars()`; provide a logical feedback on whether a dataset has (implicit) gaps or irregular data. 

* `gap_table()` provides a comprehensive summary of available and missing data. 

### Metrics and summaries

* `durations()`: calculate the groupwise duration of a dataset, based on datapoints, the dominant interval, and missing data

* `mean_daily()` and `mean_daily_metric()`: give a three-row summary of weekday, weekend, and mean daily (numeric) values. `mean_daily_metric()` skips the prior metric calculation for duration-based metrics, and directly calculates the mean daily value.

* `extract_clusters()`, `add_clusters()`: find clusters of a user-specified condition and either summarize them or add them to a dataset.

* `extract_states()`, `add_states()`: provides a summary of every state in the dataset or add them to a dataset.

* `extract_metric()`: add a calculation to extracted data, such as from `extract_state()` or `extract_clusters()`.

* `summarize_numeric()`/`summarise_numeric()`: calculate means across numeric values, ideal to summarize results from `extract_state()`, `extract_gaps()`, or `extract_clusters`.

* `Brown_cut()`: divide light exposure variables into sections ≤1lx, ≤10lx, and ≥250lx according to Brown et al. 2022

* `log_zero_inflated()` and `exp_zero_inflated()`: apply or reverse a logarithmic transformation after adding a small value to a vector so as to provide zero values in logarithmic transformation, which is especially important for light exposure.

### Visualizations

* `gg_gaps()`: visualize gaps and shows instances of irregular data.

* `gg_states()`: is an addon-function to `gg_day()` or `gg_days()`, which adds a state or cluster indicator to the plot

* `gg_heatmap()`: visualize a condensed version of time series patterns, optionally as double plots.

## Misc and Housekeeping

* Import support for the `Clouclip` device.

* `number_states()` added the option to just output a count number without the original state

* `gg_days()`: `jco_color = TRUE` is now the default

## Bug fixes

* `import_Dataset()` no longer changes a pre-existing `Id` column (if it is not called `Id`). The function is also more informative for the daylight savings time handling in files with more than one Id.

* `gg_photoperiod()` does no longer throw an error when the main plots `y.axis` is not based on a `MEDI` column.

* `gapless_Datetimes()` now ignores groups with only a single measurement, instead of throwing an error. This function is the basis for all calculations regarding gaps.


# LightLogR 0.5.4

* changed the behavior of all metric functions that calculate an in-between value (such as the duration of light between 250 and 1000 lux). Up until now, the function would use inclusive bounds on both sides, i.e. the value of 250 lux and 1000 lux would be included in the calculation. This is now changed to right exclusive bounds, i.e. the value of 250 lux will still be included in the calculation, whereas 1000 lux will not. While of little practical difference in a realistic dataset (where exact values matching the threshold are likely not present), it is relevant when calculating, e.g., the time spent in various levels of light or any other variable. The sum of those times should always add up to the total time. With inclusive bounds on both sides, the sum could theoretically be larger, with right exclusive bounds it cannot.

* Metrics `intradaily_variability()` and `interdaily_stability()` now use the population variance (divide by N), instead of the sample variance (divide by N-1). The legacy behavior can still be accessed by setting the argument `use.samplevar = TRUE`. #55

* Metric IS now correctly uses the `overall mean` in relation to the variance, instead of the `mean of hourly averages across days`. #56

# LightLogR 0.5.3

* small cleanup changes for CRAN submission

# LightLogR 0.5.2

* passed 300 unit tests for `LightLogR` 🎉

* `normalize_counts()` was added as a low-level helper function  and the accompanying dataset `gain.ratio.tables` to facilitate calculating normalized sensor values when comparing across different sensors, e.g. to assess daylighting conditions based on UV, IR, and photopic sensing ranges. See documentation for more infos.

* Update to the import function of `GENEActiv` devices, based on input from the author of the GGIR package. The timezone `tz` argument in LightLogR now is just set on the timestamp provided by the GGIR export, instead of shifting the datetime. This requires the correct setting of `desiredtz`/`configtz` arguments in GGIR during preprocessing.

# LightLogR 0.5.1

* refinement and cross-referencing in tutorials on photoperiod

* fixing a bug in the *photoperiod* family of functions when using a timezone with a large offset to the coordinates where photoperiod is calculated and crosses a date.

# LightLogR 0.5.0 "Civil dawn"

* added a suite of functions to deal with photoperiod: `photoperiod()` and `solar_noon()` to calculate dusk, dawn, and noon times of the day. `extract_photoperiod()` and `add_photoperiod()` utilize datasets imported with `LightLogR` to calculate and deal with photoperiods in the context of your own datasets. `gg_photoperiod()` brings this functionality to the visualization tools of `LightLogR` in an easy and powerful way.

* added the function `number_states()` that relabels states based on their non-consecutive appearance. This is especially useful when labelling photoperiod states, as the function will allow for an easy classifier of "day 1", "day 2", ..., and "night 1", "night 2", .... These can be used to, e.g., calculate metrics for individual photoperiod sections throughout the observed time frame.

* added a tutorial on the new functions [`Photoperiod`](https://tscnlab.github.io/LightLogR/articles/photoperiod.html). This also details how to calculate metrics based on photoperiod (#39).

* implemented further changes in the paper.md based on the [JOSS Reviews](https://github.com/openjournals/joss-reviews/issues/7601)

# LightLogR 0.4.3

* implemented changes based on the [JOSS Reviews](https://github.com/openjournals/joss-reviews/issues/7601)

* added a [Code of Conduct](https://tscnlab.github.io/LightLogR/CODE_OF_CONDUCT.html) and a [Contributing](https://tscnlab.github.io/LightLogR/CONTRIBUTING.html) file for the project

# LightLogR 0.4.2

* updated the license to MIT: LightLogR is now permissively licensed

* `import` functions will now give a warning message about identical observations in the provided data files, stop the import process and return a tibble with the duplicate rows. Through the `remove_duplicates` parameter, the user can decide to automatically remove these duplicates during import. **Note: identical observations refers to identical rows when disregarding the filename.**

# LightLogR 0.4.1

* added support for OcuWEAR devices

* added support for MotionWatch 8 devices #32

* added support for LIMO devices

* added support for GENEActiv devices, when data was preprocessed with the [`GGIR`]( https://cran.r-project.org/package=GGIR) package. The function `import$GENEActiv_GGIR()` takes the `GGIR` output and imports it with LightLogR naming schemes. #27

# LightLogR 0.4.0 "Nautical dawn"

* release on CRAN!

* changed the `supported.devices` list to a function `supported_devices()` instead, so the documentation automatically updates with the list of supported devices. Similarly, `ll_import_expr` is now `ll_import_expr()`.

* added support for the Meta `VEET` device for visual experience measurements

* added support for the `Kronowise` device

* added support for the MPI `melanopiQ Circadian Eye` (Prototype)

* rewrote the import function for `Actiwatch_Spectrum`, as the sample file the original was based off, had specific formatting to German standards. Now, the German version can still be called through `Actiwatch_Spectrum_de`, wheras the main function refers to the english/international format.

* updated the landing page for the website with a list of supported devices and a table of metrics

* small changes to documentation

# LightLogR 0.3.8

* Submission to CRAN

# LightLogR 0.3.7 "Astronomical dawn"

* Changes to the tutorial articles on the website

* Integration of a community survey on the website and Github Readme.

# LightLogR 0.3.6 

* `bright_dark_period()` now maintains the date when looping the data.

* Added articles on `Import & Cleaning`, `Metrics`, and `Visualizations` to the website.

* Added the option for more print rows of observation intervals during `import`.

* Added the option to set a length for the dataset starting from the end in `filter_Datetime()` and family.

# LightLogR 0.3.5

* Added the function `aggregate_Date()` to aggregate long datasets to one day per group.

* New function `gg_doubleplot()` for ... well, double plots.

# LightLogR 0.3.4

* Backup to Zenodo, DOI and all

# LightLogR 0.3.3

* New and updated metric functions. LightLogR now contains 16 metric families with 60 sub-metrics.

# LightLogR 0.3.2

* added import functions for `nanoLambda`and `LightWatcher` devices

* new Logo!

# LightLogR 0.3.1

* fixed bug in `interval2state()` that would dismiss the first state if it starts before the actual data

* fixed a bug in `interval2state()` that would add other columns then the `State` column present in the interval dataset to the output dataset, but leave them empty. Added an example that shows how to add multiple columns to the output dataset correctly.

* in `aggregate_Datetime()`, added the option to set the `dominant.epoch`, i.e., the most common interval, as the `unit` parameter, to effectively deal with irregular data. 

# LightLogR 0.3.0

* Added the functions `dst_change_summary()` and `dst_change_handler()` to detect and deal with Daylight Savings. The functionality is also integrated into the import functions, so that a user can automatically apply it during the import process.

* Added **Steffen Hartmeyer** as a collaborator, who added a number of light metrics from the `lightdosimetry` package.

* Added the `import_adjustment()` function for more flexibility when importing light logger data that does not conform to the standard format. This goes hand in hand with the `ll_import_expr` list that contains specific expressions for all supported devices.

* lots of bug fixes and improvements

# LightLogR 0.2.2

* Bugfix for `LiDo` import

* Added import support for new devices: `LiDo`, `DeLux`, and `Speccy`

* Removed minor inconsistencies in naming conventions. Also, all imported columns will have syntactic naming now

* Added an option to all `gap` functions, to extend the gapless Datetime range to full days.

# LightLogR 0.2.1

* Exports the up to now internal function `count_difftime()` that is the basis for `dominant_epoch()`. But whereas the latter gets only the most common epoch, `count_difftime()` returns a table with the counts of all epochs. This is useful in conjunction with `gap_finder()`, to check the distribution of data intervals.

* Added the `gg_days()` function to visualize multiple days of data in a single plot. Alongside come two helper functions, `Datetime_limits()` and `Datetime_breaks()`, to set the limits and breaks of the x-axis. 

* Added the `filter_Datetime_multiple()` function to filter for multiple Datetime ranges depending on certain conditions, e.g. different filter cutoffs for different participants. It wraps around `filter_Datetime()` or `filter_Date()`.

* Reworked the internals of the light logger data import functions. They now use a more straightforward function factory approach. For users the only visible change it that device specific functions now have the form `import$device()` instead of the old `import.device()`.

* Added the `symlog_trans()` function from a [post on stack overflow](https://stackoverflow.com/a/14674703). This function leads to a better visualization of light logger data, as a logarithmic transformation is necessary, but values of 0 are common. The function was integrated as a default for `gg_day()` and will likely be the basis of upcoming visualization functions.

* Added the `aggregate_Datetime()` function to aggregate data to a given time interval.

* Added the `gg_overview()` function to get a sense for the timeframe of measurement data.

* Added the family of `regularize` functions to find and deal with implicit missing data. These functions include `dominant_epoch()`, `gapless_Datetimes()`, `gap_handler()`, and `gap_finder()`. 

* A ton of updates to documentation, unit tests, and bug fixes.

# LightLogR 0.2.0

* Added Unit tests and documentation for all new functions.

* To `filter_Datetime()` and `filter_Date()` added the option to filter for group specific dates.

* Added the family of functions around `States` and `Reference` to import, process, and add states to light logger data, like sleep/wake times, wear times, or other data. This family includes `import_Statechanges()`, `sc2interval()`, `ìnterval2state()`, `data2reference()`, `sleep_int2Brown()`, `Brown_check()`, `Brown_rec()`, and `Brown2reference()`. 

* Added the Article/Vignette "What´s in a Day" to demonstrate the LightLogR workflow.

* Added the convenience function `create_Timedata()` to create a Time-of-Day column in datasets.

* Added the family of `filter_Datetime()`, `filter_Date()` and `filter_Time()` functions to easily filter datasets.

* Added unit tests for the first functions.

* Added several helper functions to work with states like sleep or wear times.

* Added an automatic ID creation at import and streamlined the `import` functions.

* Added the function `join_datasets` to combine imported datasets with sensible constraints.

# LightLogR 0.1.1

## `gg_day()`:
* Added `major grid marks` for the y-axis.

* Added a `message` when using start or end dates to make it clear, that only the Date portion of the input will be used.

* Changed the behavior, when there is already a Day.data column present in the data. It will only create a new column if none is present, otherwise it will use the existing column for faceting (after factorization)

* Added the option to create an `interactive` plot by feeding the plot to the [plotly] package.

# LightLogR 0.1.0

* Added a `NEWS.md` file to track changes to the package.


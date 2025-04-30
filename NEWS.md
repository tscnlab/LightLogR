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


# Package index

## Import

Wearable light logger data can be imported from a variety of sources,
i.e., exports from measurement devices or online databases. This section
also includes functions to import auxiliary data, such as sleep/wake
data. The family of import functions works on a variety of
device-specific files through `import_*()`.

- [`import_adjustment()`](https://tscnlab.github.io/LightLogR/reference/import_adjustment.md)
  : Adjust device imports or make your own

- [`import_Dataset()`](https://tscnlab.github.io/LightLogR/reference/import_Dataset.md)
  [`import`](https://tscnlab.github.io/LightLogR/reference/import_Dataset.md)
  : Import a light logger dataset or related data

- [`import_Statechanges()`](https://tscnlab.github.io/LightLogR/reference/import_Statechanges.md)
  :

  Import data that contain `Datetimes` of `Statechanges`

## Insight

Functions to gain more insight into the data. Functions in this section
will not return a version of the input dataset, but rather information
based on it.

- [`count_difftime()`](https://tscnlab.github.io/LightLogR/reference/count_difftime.md)
  : Counts the Time differences (epochs) per group (in a grouped
  dataset)
- [`dominant_epoch()`](https://tscnlab.github.io/LightLogR/reference/dominant_epoch.md)
  : Determine the dominant epoch/interval of a dataset
- [`dst_change_summary()`](https://tscnlab.github.io/LightLogR/reference/dst_change_summary.md)
  : Get a summary of groups where a daylight saving time change occurs.
- [`durations()`](https://tscnlab.github.io/LightLogR/reference/durations.md)
  : Calculate duration of data in each group
- [`photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  [`extract_photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  [`add_photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  [`solar_noon()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  : Calculate photoperiod and boundary times
- [`extract_clusters()`](https://tscnlab.github.io/LightLogR/reference/extract_clusters.md)
  [`add_clusters()`](https://tscnlab.github.io/LightLogR/reference/extract_clusters.md)
  : Find and extract clusters from a dataset
- [`extract_states()`](https://tscnlab.github.io/LightLogR/reference/extract_states.md)
  : Extract summaries of states
- [`extract_metric()`](https://tscnlab.github.io/LightLogR/reference/extract_metric.md)
  : Add metrics to extracted sSummary
- [`extract_gaps()`](https://tscnlab.github.io/LightLogR/reference/extract_gaps.md)
  : Extract gap episodes from the data
- [`gapless_Datetimes()`](https://tscnlab.github.io/LightLogR/reference/gapless_Datetimes.md)
  : Create a gapless sequence of Datetimes
- [`gap_finder()`](https://tscnlab.github.io/LightLogR/reference/gap_finder.md)
  : Check for and output gaps in a dataset
- [`gap_table()`](https://tscnlab.github.io/LightLogR/reference/gap_table.md)
  : Tabular summary of data and gaps in all groups
- [`has_gaps()`](https://tscnlab.github.io/LightLogR/reference/has_gaps.md)
  : Does a dataset have implicit gaps
- [`has_irregulars()`](https://tscnlab.github.io/LightLogR/reference/has_irregulars.md)
  : Does a dataset have irregular data
- [`summary_overview()`](https://tscnlab.github.io/LightLogR/reference/summary_table.md)
  [`summary_metrics()`](https://tscnlab.github.io/LightLogR/reference/summary_table.md)
  [`summary_table()`](https://tscnlab.github.io/LightLogR/reference/summary_table.md)
  : Light exposure summary table helpers

## Process

Functions to process light logger data, e.g., to validate and clean
data, to filter, cut or aggreagate data, or to join datasets. All of
these functions will return a version of the input dataset.

- [`add_Time_col()`](https://tscnlab.github.io/LightLogR/reference/add_Time_col.md)
  : Create a Time-of-Day column in the dataset
- [`add_Date_col()`](https://tscnlab.github.io/LightLogR/reference/add_Date_col.md)
  : Create a Date column in the dataset
- [`aggregate_Date()`](https://tscnlab.github.io/LightLogR/reference/aggregate_Date.md)
  : Aggregate dates to a single day
- [`aggregate_Datetime()`](https://tscnlab.github.io/LightLogR/reference/aggregate_Datetime.md)
  : Aggregate Datetime data
- [`create_Timedata()`](https://tscnlab.github.io/LightLogR/reference/create_Timedata.md)
  : create_Timedata
- [`cut_Datetime()`](https://tscnlab.github.io/LightLogR/reference/cut_Datetime.md)
  : Create Datetime bins for visualization and calculation
- [`dst_change_handler()`](https://tscnlab.github.io/LightLogR/reference/dst_change_handler.md)
  : Handle jumps in Daylight Savings (DST) that are missing in the data
- [`filter_Datetime()`](https://tscnlab.github.io/LightLogR/reference/filter_Datetime.md)
  [`filter_Date()`](https://tscnlab.github.io/LightLogR/reference/filter_Datetime.md)
  : Filter Datetimes in a dataset.
- [`filter_Datetime_multiple()`](https://tscnlab.github.io/LightLogR/reference/filter_Datetime_multiple.md)
  : Filter multiple times based on a list of arguments.
- [`filter_Time()`](https://tscnlab.github.io/LightLogR/reference/filter_Time.md)
  : Filter Times in a dataset.
- [`gap_handler()`](https://tscnlab.github.io/LightLogR/reference/gap_handler.md)
  : Fill implicit gaps in a light logger dataset
- [`join_datasets()`](https://tscnlab.github.io/LightLogR/reference/join_datasets.md)
  : Join similar Datasets
- [`remove_partial_data()`](https://tscnlab.github.io/LightLogR/reference/remove_partial_data.md)
  : Remove groups that have too few data points

## Expand

Expanding light logger data through auxiliary data and/or reference data
allows for a more comprehensive analysis.

- [`photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  [`extract_photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  [`add_photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  [`solar_noon()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  : Calculate photoperiod and boundary times
- [`extract_clusters()`](https://tscnlab.github.io/LightLogR/reference/extract_clusters.md)
  [`add_clusters()`](https://tscnlab.github.io/LightLogR/reference/extract_clusters.md)
  : Find and extract clusters from a dataset
- [`add_states()`](https://tscnlab.github.io/LightLogR/reference/add_states.md)
  : Add states to a dataset based on groups and start/end times
- [`Brown2reference()`](https://tscnlab.github.io/LightLogR/reference/Brown2reference.md)
  : Add Brown et al. (2022) reference illuminance to a dataset
- [`Brown_check()`](https://tscnlab.github.io/LightLogR/reference/Brown_check.md)
  : Check whether a value is within the recommended illuminance/MEDI
  levels by Brown et al. (2022)
- [`Brown_cut()`](https://tscnlab.github.io/LightLogR/reference/Brown_cut.md)
  : Create a state column that cuts light levels into sections by Brown
  et al. (2022)
- [`Brown_rec()`](https://tscnlab.github.io/LightLogR/reference/Brown_rec.md)
  : Set the recommended illuminance/MEDI levels by Brown et al. (2022)
- [`data2reference()`](https://tscnlab.github.io/LightLogR/reference/data2reference.md)
  : Create reference data from other data
- [`interval2state()`](https://tscnlab.github.io/LightLogR/reference/interval2state.md)
  : Adds a state column to a dataset from interval data
- [`number_states()`](https://tscnlab.github.io/LightLogR/reference/number_states.md)
  : Number non-consecutive state occurrences
- [`sc2interval()`](https://tscnlab.github.io/LightLogR/reference/sc2interval.md)
  : Statechange (sc) Timestamps to Intervals
- [`sleep_int2Brown()`](https://tscnlab.github.io/LightLogR/reference/sleep_int2Brown.md)
  : Recode Sleep/Wake intervals to Brown state intervals
- [`spectral_reconstruction()`](https://tscnlab.github.io/LightLogR/reference/spectral_reconstruction.md)
  : Reconstruct spectral irradiance from sensor counts

## Visualize

Functions to visualize light logger data, e.g., to plot light exposure
or to plot sleep/wake data.

- [`gg_day()`](https://tscnlab.github.io/LightLogR/reference/gg_day.md)
  : Create a simple Time-of-Day plot of light logger data, faceted by
  Date
- [`gg_days()`](https://tscnlab.github.io/LightLogR/reference/gg_days.md)
  : Create a simple datetime plot of light logger data, faceted by group
- [`gg_doubleplot()`](https://tscnlab.github.io/LightLogR/reference/gg_doubleplot.md)
  : Double Plots
- [`gg_gaps()`](https://tscnlab.github.io/LightLogR/reference/gg_gaps.md)
  : Visualize gaps and irregular data
- [`gg_heatmap()`](https://tscnlab.github.io/LightLogR/reference/gg_heatmap.md)
  : Plot a heatmap across days and times of day
- [`gg_overview()`](https://tscnlab.github.io/LightLogR/reference/gg_overview.md)
  : Plot an overview of dataset intervals with implicit missing data
- [`gg_photoperiod()`](https://tscnlab.github.io/LightLogR/reference/gg_photoperiod.md)
  : Add photoperiods to gg_day() or gg_days() plots
- [`gg_state()`](https://tscnlab.github.io/LightLogR/reference/gg_state.md)
  **\[deprecated\]** : Add states to gg_day() or gg_days() plots
- [`gg_states()`](https://tscnlab.github.io/LightLogR/reference/gg_states.md)
  : Add states to gg_day() or gg_days() plots

## Metrics

Functions to calculate light exposure metrics.

- [`barroso_lighting_metrics()`](https://tscnlab.github.io/LightLogR/reference/barroso_lighting_metrics.md)
  : Circadian lighting metrics from Barroso et al. (2014)
- [`bright_dark_period()`](https://tscnlab.github.io/LightLogR/reference/bright_dark_period.md)
  : Brightest or darkest continuous period
- [`centroidLE()`](https://tscnlab.github.io/LightLogR/reference/centroidLE.md)
  : Centroid of light exposure
- [`disparity_index()`](https://tscnlab.github.io/LightLogR/reference/disparity_index.md)
  : Disparity index
- [`dose()`](https://tscnlab.github.io/LightLogR/reference/dose.md) :
  Calculate the dose (value·hours)
- [`duration_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/duration_above_threshold.md)
  : Duration above/below threshold or within threshold range
- [`exponential_moving_average()`](https://tscnlab.github.io/LightLogR/reference/exponential_moving_average.md)
  : Exponential moving average filter (EMA)
- [`frequency_crossing_threshold()`](https://tscnlab.github.io/LightLogR/reference/frequency_crossing_threshold.md)
  : Frequency of crossing light threshold
- [`intradaily_variability()`](https://tscnlab.github.io/LightLogR/reference/intradaily_variability.md)
  : Intradaily variability (IV)
- [`interdaily_stability()`](https://tscnlab.github.io/LightLogR/reference/interdaily_stability.md)
  : Interdaily stability (IS)
- [`midpointCE()`](https://tscnlab.github.io/LightLogR/reference/midpointCE.md)
  : Midpoint of cumulative light exposure.
- [`nvRC()`](https://tscnlab.github.io/LightLogR/reference/nvRC.md) :
  Non-visual circadian response
- [`nvRC_circadianDisturbance()`](https://tscnlab.github.io/LightLogR/reference/nvRC_metrics.md)
  [`nvRC_circadianBias()`](https://tscnlab.github.io/LightLogR/reference/nvRC_metrics.md)
  [`nvRC_relativeAmplitudeError()`](https://tscnlab.github.io/LightLogR/reference/nvRC_metrics.md)
  : Performance metrics for circadian response
- [`nvRD()`](https://tscnlab.github.io/LightLogR/reference/nvRD.md) :
  Non-visual direct response
- [`nvRD_cumulative_response()`](https://tscnlab.github.io/LightLogR/reference/nvRD_cumulative_response.md)
  : Cumulative non-visual direct response
- [`period_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/period_above_threshold.md)
  : Length of longest continuous period above/below threshold
- [`pulses_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/pulses_above_threshold.md)
  : Pulses above threshold
- [`spectral_integration()`](https://tscnlab.github.io/LightLogR/reference/spectral_integration.md)
  : Integrate spectral irradiance with optional weighting
- [`threshold_for_duration()`](https://tscnlab.github.io/LightLogR/reference/threshold_for_duration.md)
  : Find threshold for given duration
- [`timing_above_threshold()`](https://tscnlab.github.io/LightLogR/reference/timing_above_threshold.md)
  : Mean/first/last timing above/below threshold.

## Photoperiod

Functions that deal with photoperiod aspects of a dataset

- [`photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  [`extract_photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  [`add_photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  [`solar_noon()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  : Calculate photoperiod and boundary times
- [`gg_photoperiod()`](https://tscnlab.github.io/LightLogR/reference/gg_photoperiod.md)
  : Add photoperiods to gg_day() or gg_days() plots

## Helpers

Helper functions that are used in the other sections.

- [`Circular2Time()`](https://tscnlab.github.io/LightLogR/reference/Circular2Time.md)
  : Convert circular time columns to hms
- [`Datetime2Time()`](https://tscnlab.github.io/LightLogR/reference/Datetime2Time.md)
  : Convert Datetime columns to Time columns
- [`Datetime_breaks()`](https://tscnlab.github.io/LightLogR/reference/Datetime_breaks.md)
  : Create a (shifted) sequence of Datetimes for axis breaks
- [`Datetime_limits()`](https://tscnlab.github.io/LightLogR/reference/Datetime_limits.md)
  : Find or set sensible limits for Datetime axis
- [`log_zero_inflated()`](https://tscnlab.github.io/LightLogR/reference/log_zero_inflated.md)
  [`exp_zero_inflated()`](https://tscnlab.github.io/LightLogR/reference/log_zero_inflated.md)
  : Add a defined number to a numeric and log transform it
- [`format_coordinates()`](https://tscnlab.github.io/LightLogR/reference/format_coordinates.md)
  : Format coordinates as a readable string
- [`mean_daily()`](https://tscnlab.github.io/LightLogR/reference/mean_daily.md)
  : Calculate mean daily metrics from daily summary
- [`mean_daily_metric()`](https://tscnlab.github.io/LightLogR/reference/mean_daily_metric.md)
  : Calculate mean daily metrics from Time Series
- [`normalize_counts()`](https://tscnlab.github.io/LightLogR/reference/normalize_counts.md)
  : Normalize counts between sensor outputs
- [`photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  [`extract_photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  [`add_photoperiod()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  [`solar_noon()`](https://tscnlab.github.io/LightLogR/reference/photoperiod.md)
  : Calculate photoperiod and boundary times
- [`reverse2_trans()`](https://tscnlab.github.io/LightLogR/reference/reverse2_trans.md)
  : Create a reverse transformation function specifically for date
  scales
- [`sample_groups()`](https://tscnlab.github.io/LightLogR/reference/sample_groups.md)
  : Sample groups from a grouped dataset
- [`spectral_reconstruction()`](https://tscnlab.github.io/LightLogR/reference/spectral_reconstruction.md)
  : Reconstruct spectral irradiance from sensor counts
- [`style_time()`](https://tscnlab.github.io/LightLogR/reference/style_time.md)
  : Style (date)times as times
- [`summarize_numeric()`](https://tscnlab.github.io/LightLogR/reference/summarize_numeric.md)
  [`summarise_numeric()`](https://tscnlab.github.io/LightLogR/reference/summarize_numeric.md)
  : Summarize numeric columns in dataframes to means
- [`symlog_trans()`](https://tscnlab.github.io/LightLogR/reference/symlog_trans.md)
  : Scale positive and negative values on a log scale

## Datasets

Datasets that are used in LightLogR and in examples.

- [`alphaopic.action.spectra`](https://tscnlab.github.io/LightLogR/reference/alphaopic.action.spectra.md)
  : Alphaopic (+ photopic) action spectra
- [`gain.ratio.tables`](https://tscnlab.github.io/LightLogR/reference/gain.ratio.tables.md)
  : Gain / Gain-ratio tables to normalize counts
- [`ll_import_expr()`](https://tscnlab.github.io/LightLogR/reference/ll_import_expr.md)
  : Get the import expression for a device
- [`sample.data.environment`](https://tscnlab.github.io/LightLogR/reference/sample.data.environment.md)
  : Sample of wearable data combined with environmental data
- [`sample.data.irregular`](https://tscnlab.github.io/LightLogR/reference/sample.data.irregular.md)
  : Sample of highly irregular wearable data
- [`supported_devices()`](https://tscnlab.github.io/LightLogR/reference/supported_devices.md)
  : Get all the supported devices in LightLogR
- [`supported_versions()`](https://tscnlab.github.io/LightLogR/reference/supported_versions.md)
  : Get all the supported device-formats in LightLogR

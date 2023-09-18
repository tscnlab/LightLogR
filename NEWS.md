# LightLogR (development version)

# LightLogR 0.1.1.9000

* Added the convenience function [create_Time.data()] to create a Time-of-Day column in datasets.

* Added the family of [filter_Datetime()], [filter_Date()] and [filter_Time()] functions to easily filter datasets.

* Added unit tests for the first functions.

## [gg_day()]:
* Added `major grid marks` for the y-axis.

* Added a `message` when using start or end dates to make it clear, that only the Date portion of the input will be used.

* Changed the behavior, when there is already a Day.data column present in the data. It will only create a new column if none is present, otherwise it will use the existing column for faceting (after factorization)

* Added the option to create an `interactive` plot by feeding the plot to the [plotly] package.

# LightLogR 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.


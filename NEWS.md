# LightLogR 0.2.0.9000

* Added the Article/Vignette "What´s in a Day" to demonstrate the LightLogR workflow.

* Added the convenience function `create_Time.data()` to create a Time-of-Day column in datasets.

* Added the family of `filter_Datetime()`, `filter_Date()` and `filter_Time()` functions to easily filter datasets.

* Added unit tests for the first functions.

* Added several helper functions to work with states like sleep or wear times.

* Added an automatic ID creation at import and streamlined the `import` functions.

* Added the function `join.datasets` to combine imported datasets with sensible constraints.

# LightLogR 0.1.1.9000

## `gg_day()`:
* Added `major grid marks` for the y-axis.

* Added a `message` when using start or end dates to make it clear, that only the Date portion of the input will be used.

* Changed the behavior, when there is already a Day.data column present in the data. It will only create a new column if none is present, otherwise it will use the existing column for faceting (after factorization)

* Added the option to create an `interactive` plot by feeding the plot to the [plotly] package.

# LightLogR 0.1.0.9000

* Added a `NEWS.md` file to track changes to the package.

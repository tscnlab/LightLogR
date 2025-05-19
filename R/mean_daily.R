#' Calculate mean daily metrics from daily summary
#'
#' `mean_daily` calculates a three-row summary of metrics showing average
#' weekday, weekend, and mean daily values of all non-grouping numeric columns.
#' The basis is a dataframe that contains metrics per weekday, or per date (with
#' `calculate.from.Date = Datetime`). The function requires a column specifying
#' the day of the week as a factor (with Monday as the weekstart), or it can
#' calculate this from a date column if provided.
#'
#' @param data A dataframe containing the metrics to summarize
#' @param Weekend.type A column in the dataframe that specifies the day of the
#'   week as a factor, where weekstart is Monday (so weekends are 6 and 7 in
#'   numeric representation)
#' @param na.rm Logical, whether to remove NA values when calculating means.
#'   Default is TRUE.
#' @param calculate.from.Date Optional. A column in the dataframe containing
#'   dates from which to calculate the Weekend.type. If provided, Weekend.type
#'   will be generated from this column.
#' @param prefix String that is the prefix on summarized values
#'
#' @return A dataframe with three rows representing average weekday, weekend,
#'   and mean daily values of all numeric columns
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   Day = factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
#'                levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
#'   lux = c(250, 300, 275, 280, 290, 350, 320),
#'   duration = lubridate::as.duration(c(120, 130, 125, 135, 140, 180, 160))
#' )
#'
#' # Calculate mean daily metrics
#' mean_daily(sample_data)
#'
#' # With a Date column
#' sample_data_with_date <- data.frame(
#'   Date = seq(as.Date("2023-05-01"), as.Date("2023-05-07"), by = "day"),
#'   lux = c(250, 300, 275, 280, 290, 350, 320),
#'   duration = lubridate::as.duration(c(120, 130, 125, 135, 140, 180, 160))
#' )
#'
#' mean_daily(sample_data_with_date, calculate.from.Date = Date)
#'
#' @export
mean_daily <- function(data,
                       Weekend.type = Day,
                       na.rm = TRUE,
                       calculate.from.Date = NULL,
                       prefix = "average_") {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a dataframe")
  }
  
  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("'na.rm' must be a logical value (TRUE or FALSE)")
  }
  
  Date_quo <- rlang::enexpr(calculate.from.Date)
  # Date_name <- rlang::as_name(Date_quo)
  
  # Calculate Weekend.type from Date if provided
  if(!is.null(Date_quo)) {
    data <-
      data |>
      dplyr::mutate({{ Weekend.type }} := lubridate::wday({{ calculate.from.Date }}, 
                                                          label = TRUE, 
                                                          week_start = 1))
  }
  
  # Ensure Weekend.type exists in the data
  w_type_quo <- rlang::enquo(Weekend.type)
  w_type_name <- rlang::as_name(w_type_quo)
  
  if (!(w_type_name %in% names(data))) {
    stop(sprintf("Column '%s' not found in the input data", w_type_name))
  }
  
  # Calculate weekday/weekend averages
  weekday_type <-
    data |>
    dplyr::group_by({{ Weekend.type }} := dplyr::if_else(as.numeric({{ Weekend.type }}) > 5,
                                                         "Weekend",
                                                         "Weekday") |> factor(levels = c("Weekday", "Weekend")), 
                    .add = TRUE, .drop = FALSE) |>
    dplyr::summarize(dplyr::across(dplyr::where(is.numeric),
                                   \(x) if(inherits(x, "Duration")) {
                                     lubridate::duration(mean(x, na.rm = na.rm)) |> 
                                       round(0)
                                   } else {
                                     mean(x, na.rm = na.rm)
                                   },
                                   .names = "{prefix}{.col}"),
                     .groups = "drop_last")
  
  if(nrow(weekday_type) == 0) {
    stop(call. = FALSE, "No weekday or weekend information present")
  } else if (!"Weekday" %in% (weekday_type |> dplyr::pull({{ Weekend.type }}))) {
    warning(call. = FALSE, "No weekday information present. Mean daily calculation skipped")
  } else if (!"Weekend" %in% (weekday_type |> dplyr::pull({{ Weekend.type }}))) {
    warning(call. = FALSE, "No weekend information present. Mean daily calculation skipped")
  }
  
  if (nrow(weekday_type) == 1) {
    return(weekday_type)
  }
  
  # Calculate mean daily values
  mean_daily <-
    weekday_type |>
    dplyr::mutate(
      dplyr::across(-{{ Weekend.type }},
                    \(x) dplyr::case_when({{ Weekend.type }} == "Weekday" ~ x*5,
                                          {{ Weekend.type }} == "Weekend" ~ x*2))) |>
    dplyr::summarize(
      dplyr::across(-{{ Weekend.type }},
                    \(x) if(inherits(x, "Duration")) {
                      lubridate::duration(sum(x)/7) |> round(0)
                    } else {
                      sum(x)/7
                    }),
      .groups = "keep"
    ) |>
    dplyr::mutate({{ Weekend.type }} := "Mean daily", .before = -1)
  
  # Combine results
  weekday_type |> 
    dplyr::bind_rows(mean_daily) |> 
    dplyr::arrange({{ Weekend.type }}, .by_group = TRUE)
}

#' Calculate mean daily metrics from Time Series
#'
#' `mean_daily_metric` is a convenience wrapper around `mean_daily` that
#' summarizes data imported with LightLogR per weekday and calculates mean daily
#' values for a specific metric. Examples include [duration_above_threshold()]
#' (the default), or [durations()].
#'
#' @inheritParams mean_daily
#' @param data A dataframe containing light logger data imported with LightLogR
#' @param metric The name of the metric column to create in the output. Expects
#'   a `character`
#' @param variable The variable column to analyze. Expects a `symbol`. Needs to
#'   be part of the dataset.
#' @param Weekend.type A (new) column in the dataframe that specifies the day of
#'   the week as a factor
#' @param Datetime.colname Column name containing datetime values. Defaults to
#'   `Datetime`
#' @param metric_type The metric function to apply, default is
#'   [duration_above_threshold()]
#' @param ... Additional arguments passed to the metric function
#'
#' @return A dataframe with three rows representing average weekday, weekend,
#'   and mean daily values for the specified metric
#'
#' @examples
#' # Import sample data
#' filepath <- system.file("extdata/sample_data_LYS.csv", package = "LightLogR")
#' dataset <- import$LYS(filepath, silent = TRUE)
#'
#' # Calculate mean daily duration above threshold. As the data only contains
#' # data for two days, Weekend and Mean daily will throw errors
#' dataset |> 
#' mean_daily_metric(
#'   metric = "duration_above_100lux",
#'   variable = lux,
#'   threshold = 100
#' )
#' 
#' # again with another dataset
#' sample.data.environment |> 
#'   mean_daily_metric(
#'   metric = "duration_above_250lux",
#'   variable = MEDI,
#'   threshold = 250)
#'
#' @export
mean_daily_metric <- function(data,
                              metric,
                              variable,
                              Weekend.type = Day,
                              Datetime.colname = Datetime,
                              metric_type = duration_above_threshold,
                              prefix = "average_",
                              ...) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a dataframe")
  }
  
  dt_quo <- rlang::enquo(Datetime.colname)
  dt_name <- rlang::as_name(dt_quo)
  
  var_quo <- rlang::enquo(variable)
  var_name <- rlang::as_name(var_quo)
  
  if (!(dt_name %in% names(data))) {
    stop(sprintf("Column '%s' not found in the input data", dt_name))
  }
  
  if (!(var_name %in% names(data))) {
    stop(sprintf("Column '%s' not found in the input data", var_name))
  }
  
  # Check if metric_type is a function
  if (!is.function(metric_type)) {
    stop("'metric_type' must be a function")
  }
  
  # Calculate metric per weekday
  weekday <-
    data |>
    dplyr::group_by({{ Weekend.type }} := 
                      lubridate::wday({{ Datetime.colname }}, 
                                      label = TRUE, 
                                      week_start = 1),
                    .add = TRUE) |>
    dplyr::summarize(
      {{ metric }} := 
        (!!metric_type)({{ variable }},
                        Time.vector = {{ Datetime.colname }},
                        ...),
      .groups = "drop_last"
    )
  
  # Calculate mean daily metrics
  mean_daily(
    weekday,
    Weekend.type = {{ Weekend.type }},
    prefix = prefix
  )
}

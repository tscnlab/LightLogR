#' Calculate mean daily metrics from daily summary
#'
#' `mean_daily` calculates a three-row summary of metrics showing average
#' weekday, weekend, and mean daily values of all non-grouping numeric columns.
#' The basis is a dataframe that contains metrics per weekday, or per date (with
#' `calculate.from.Date = Datetime`). The function requires a column specifying
#' the day of the week as a factor (with Monday as the weekstart), or it can
#' calculate this from a date column if provided.
#'
#' Summary values for type `POSIXct` are calculated as the mean, which can be
#' nonsensical at times (e.g., the mean of Day1 18:00 and Day2 18:00, is Day2
#' 6:00, which can be the desired result, but if the focus is on time, rather
#' then on datetime, it is recommended that values are converted to times via
#' [hms::as_hms()] before applying the function (the mean of 18:00 and 18:00 is
#' still 18:00, not 6:00).
#'
#' @param data A dataframe containing the metrics to summarize
#' @param Weekend.type A column in the dataframe that specifies the day of the
#'   week as a factor, where weekstart is Monday (so weekends are 6 and 7 in
#'   numeric representation). If it is a date, it will be converted to this
#'   factor
#' @param na.rm Logical, whether to remove NA values when calculating means.
#'   Default is TRUE.
#' @param calculate.from.Date Optional. A column in the dataframe containing
#'   dates from which to calculate the Weekend.type. If provided, Weekend.type
#'   will be generated from this column.
#' @param prefix String that is the prefix on summarized values
#' @param filter.empty Filter out empty rows. Default is FALSE
#' @param sub.zero Logical. Should missing values be replaced by zero? Defaults
#'   to `FALSE`. Will throw an error, if it happens on a type other than
#'   `double`.
#' @param Datetime2Time Logical of whether POSIXct columns should be transformed
#'   into hms(time) columns, which is usually sensible for averaging (default is
#'   `TRUE`). Calls [Datetime2Time()] with default settings (all POSIXct are
#'   affected).
#' @param Datetime2Time.circular Logical of whether Time should be circular.
#'   Will be ignored if `Datetime2Time = FALSE`. Default is `FALSE`.
#'
#' @return A dataframe with three rows representing average weekday, weekend,
#'   and mean daily values of all numeric columns
#'
#' @examples
#' # Create sample data
#' sample_data <- data.frame(
#'   Date = factor(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
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
#' mean_daily(sample_data_with_date)
#'
#' @export
mean_daily <- function(data,
                       Weekend.type = Date,
                       na.rm = TRUE,
                       calculate.from.Date = NULL,
                       prefix = "average_",
                       filter.empty = FALSE,
                       sub.zero = FALSE,
                       Datetime2Time = TRUE,
                       Datetime2Time.circular = FALSE
                       ) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a dataframe")
  }
  
  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("'na.rm' must be a logical value (TRUE or FALSE)")
  }
  
  if(Datetime2Time) {
    data <-
      data |>
      Datetime2Time(silent = TRUE, circular = Datetime2Time.circular)
  }
  
  Date_quo <- rlang::enexpr(calculate.from.Date)
  Weekend_str <- colname.defused({{ Weekend.type }})
  # Date_name <- rlang::as_name(Date_quo)
  
  #Calculate Weekend.type from Date if provided
  if(!is.null(Date_quo)) {
    data <-
      data |>
      dplyr::mutate({{ Weekend.type }} := lubridate::wday({{ calculate.from.Date }},
                                                          label = TRUE,
                                                          week_start = 1))
  }
  
  # Calculate Weekend.type from Date if is type date
  if(lubridate::is.Date(data[[Weekend_str]])) {
    data <-
      data |>
      dplyr::mutate({{ Weekend.type }} := lubridate::wday({{ Weekend.type }}, 
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
    dplyr::summarize(dplyr::across(dplyr::where(\(x) is.double(x) | is.numeric(x)),
                                   \(x) if(inherits(x, "Duration")) {
                                     lubridate::duration(mean(x, na.rm = na.rm)) |> 
                                       round(0)
                                   } else if (inherits(x, "hms")) {
                                     hms::as_hms(mean(x, na.rm = na.rm) |> round(0))
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
  
  #replace na with zeros
  if(sub.zero){
    weekday_type <- 
    weekday_type |> 
      dplyr::mutate(
        dplyr::across(dplyr::where(\(x) is.double(x) | is.numeric(x)),
                      \(x) ifelse(is.na(x), 0, x)
        )
      )
  }
  
  # Calculate mean daily values
  mean_daily <-
    weekday_type |>
    dplyr::mutate(
      # dplyr::across(-{{ Weekend.type }},
      #               \(x) dplyr::case_when({{ Weekend.type }} == "Weekday" ~ x*5,
      #                                     {{ Weekend.type }} == "Weekend" ~ x*2))
      .weight = dplyr::case_when(
        {{ Weekend.type }} == "Weekday" ~ 5,
        {{ Weekend.type }} == "Weekend" ~ 2
      )
      )|>
    dplyr::summarize(
      dplyr::across(-c({{ Weekend.type }}, dplyr::where(lubridate::is.Date)),
                    \(x) if(inherits(x, "Duration")) {
                      lubridate::duration(sum(x*.weight)/7) |> round(0)
                    } else if (inherits(x, "hms")) {
                      hms::as_hms((sum(x*.weight)/7) |> round(0)) 
                    } else {
                      sum(x*.weight)/7
                    }),
      .groups = "keep"
    ) |>
    dplyr::select(-.weight) |>
    dplyr::mutate({{ Weekend.type }} := "Mean daily", .before = -1)
  
  # Combine results
  weekday_type <- 
  weekday_type |> 
    dplyr::bind_rows(mean_daily) |> 
    dplyr::arrange({{ Weekend.type }}, .by_group = TRUE)
  
  #filter out empty rows
  if(filter.empty){
    weekday_type |>
      dplyr::filter(
        !dplyr::if_all(-{{ Weekend.type }}, is.na)
      )
  } else {
    # Return the result
    return(weekday_type)
  }
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
#' @param Variable The variable column to analyze. Expects a `symbol`. Needs to
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
#'
#' # Calculate mean daily duration above threshold. As the data only contains
#' # data for two days, Weekend and Mean daily will throw NA
#' sample.data.irregular |> 
#' aggregate_Datetime(unit = "1 min") |> 
#' mean_daily_metric(
#'   Variable = lux,
#'   threshold = 100
#' )
#' 
#' # again with another dataset
#' sample.data.environment |> 
#'   mean_daily_metric(
#'   Variable = MEDI,
#'   threshold = 250)
#'
#' # by default, datetime columns are converted to time
#' sample.data.environment |> 
#'   mean_daily_metric(
#'   Variable = MEDI,
#'   metric_type = timing_above_threshold,
#'   threshold = 250)
#'
#' @export
mean_daily_metric <- function(data,
                              Variable,
                              Weekend.type = Date,
                              Datetime.colname = Datetime,
                              metric_type = duration_above_threshold,
                              prefix = "average_",
                              filter.empty = FALSE,
                              Datetime2Time = TRUE,
                              ...) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a dataframe")
  }
  
  dt_quo <- rlang::enquo(Datetime.colname)
  dt_name <- rlang::as_name(dt_quo)
  
  var_quo <- rlang::enquo(Variable)
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
        (!!metric_type)({{ Variable }},
                        Time.vector = {{ Datetime.colname }},
                        as.df = TRUE,
                        ...),
      .groups = "drop_last"
    )
  
  # Calculate mean daily metrics
  mean_daily(
    weekday,
    Weekend.type = {{ Weekend.type }},
    prefix = prefix,
    filter.empty = filter.empty,
    Datetime2Time = Datetime2Time
  )
}

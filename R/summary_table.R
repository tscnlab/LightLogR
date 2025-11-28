#' Light exposure summary table helpers
#'
#' These helpers create a publication-ready summary table for light logger
#' datasets. Users can either calculate the metrics, generate overview counts,
#' or render the complete [gt][gt::gt()] table.
#'
#' @name summary_table
NULL

#' Calculate overview statistics for light logger datasets
#'
#' This function creates a tibble that gives some high level information about a
#' dataset: How many participants are in there, the number of participant days,
#' how many participant days are complete above a given threshold, how much data
#' is missing, and (if provided) what the photoperiod is.
#' 
#' The function is used within [summary_table()].
#'
#' @param dataset A data frame containing light logger data.
#' @param coordinates Optional numeric vector of length two containing latitude
#'   and longitude (in that order). If supplied, photoperiod information is
#'   calculated when the dataset does not already contain a `photoperiod`
#'   column.
#' @param location Optional location description (e.g. city name).
#' @param site Optional site description (e.g. country or study site).
#' @param Variable.colname Column containing light exposure values. Expects a
#'   symbol; defaults to `MEDI` for compatibility with the built-in datasets.
#' @param Datetime.colname Column containing the timestamp information. Expects
#'   a symbol; defaults to `Datetime`.
#' @param Id.colname Column containing the participant identifier. Expects a
#'   symbol; defaults to `Id`.
#' @param threshold.missing Proportion of missing data (per participant-day)
#'   tolerated before a day is considered incomplete.
#' @param programmatic.use Whether the function is used by another function.
#'   This determines the number of columns to be output. Default is `FALSE`
#' @param handle.gaps Whether gaps in the data should be handled. Sets the
#'   argument in [remove_partial_data()]. Default is `TRUE`.
#'
#' @return A tibble with overview metrics (`type`, `name`, `mean`, `SD`, `min`,
#'   `max`, `plot`). A `location_string` attribute is attached to the result for
#'   use in [summary_table()]. If `programmatic.use = FALSE`, `type`, `SD` and
#'   `plot` are removed.
#'
#' @rdname summary_table
#' @export
#' 
#' @examples
#' sample.data.environment |> summary_overview()
#' sample.data.irregular |> summary_overview()
summary_overview <- function(dataset,
                             Variable.colname = MEDI,
                             coordinates = NULL,
                             location = NULL,
                             site = NULL,
                             Datetime.colname = Datetime,
                             Id.colname = Id,
                             threshold.missing = 0.2,
                             programmatic.use = FALSE,
                             handle.gaps = TRUE) {

  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "threshold.missing has to be numeric" = is.numeric(threshold.missing),
    "programmatic.use must be a logical" = is.logical(programmatic.use),
    "handle.gaps must be a logical" = is.logical(handle.gaps)
  )

  # Participant and day counts ------------------------------------------------
  daily_data <-
    dataset |>
    dplyr::group_by({{ Id.colname }}) |>
    add_Date_col(.Date, group.by = TRUE)

  suppressMessages(
  complete_daily_data <-
    daily_data |>
    remove_partial_data({{ Variable.colname }},
                        threshold.missing = threshold.missing,
                        handle.gaps = handle.gaps)
  )

  participant_days <- daily_data |> dplyr::ungroup() |> dplyr::distinct({{ Id.colname }}, .Date)
  complete_participant_days <- complete_daily_data |> dplyr::ungroup() |> dplyr::distinct({{ Id.colname }}, .Date)

  n_participants <- participant_days |> dplyr::distinct({{ Id.colname }}) |> nrow()
  n_participant_days <- complete_participant_days |> nrow()
  n_participant_days_range <- 
    if(nrow(complete_participant_days) == 0) {
      c(0,0)
    } else {
    complete_participant_days |> 
    dplyr::group_by({{ Id.colname }}) |> 
    dplyr::count() |> 
    dplyr::pull(n) |> 
    range()
    }
  total_participant_days <- participant_days |> nrow()
  total_participant_days_range <- 
    participant_days |> 
    dplyr::group_by({{ Id.colname }}) |> 
    dplyr::count() |> 
    dplyr::pull(n) |> 
    range()

  # Missingness ---------------------------------------------------------------
  missingness_row <- missingness_summary(dataset, {{ Variable.colname }})

  # Photoperiod ---------------------------------------------------------------
  photoperiod_row <- photoperiod_summary(daily_data, coordinates, {{ Datetime.colname }}, {{ Id.colname }})

  overview <- tibble::tibble(
    type = "Overview",
    name = c(
      "Participants",
      "Participant-days",
      paste0("Days \u2265", round((1 - threshold.missing) * 100), "% complete")
    ),
    mean = c(n_participants, total_participant_days, n_participant_days),
    SD = NA_real_,
    min = c(NA_real_, total_participant_days_range[1], n_participant_days_range[1]),
    max = c(NA_real_, total_participant_days_range[2], n_participant_days_range[2]),
    plot = list(NA, NA, NA)
  ) |>
    dplyr::bind_rows(missingness_row) |>
    dplyr::bind_rows(photoperiod_row)

  attr(overview, "location_string") <- location_string(dataset, coordinates, location, site, {{ Datetime.colname }})
  if(programmatic.use){
    overview
  } else overview |> dplyr::select(-SD, -plot, -type)
}

#' Calculate daily and participant-level light metrics
#' 
#' @return A tibble with summarized metrics across participant-days and
#'   participant-level stability measures. Columns are compatible with
#'   [summary_table()].
#' 
#' @rdname summary_table
#' @export
#' 
#' @examples
#' \donttest{
#' sample.data.environment |> 
#' filter_Date(length = "3 days") |> 
#' summary_metrics()
#'   }
summary_metrics <- function(dataset,
                            Variable.colname = MEDI,
                            Datetime.colname = Datetime,
                            Id.colname = Id,
                            threshold.missing = 0.2,
                            programmatic.use = FALSE,
                            handle.gaps = TRUE) {

  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset)
  )
  suppressMessages(
  daily_data <-
    dataset |>
    dplyr::group_by({{ Id.colname }}) |>
    add_Date_col(.Date, group.by = TRUE) |>
    remove_partial_data({{ Variable.colname }},
                        threshold.missing = threshold.missing,
                        handle.gaps = handle.gaps)
  )

  if(nrow(daily_data) == 0){stop("No data is left with below `threshold.missing`")} 
  daily_metrics <- summarise_daily_metrics(daily_data, {{ Variable.colname }}, {{ Datetime.colname }})
  participant_metrics <- summarise_participant_metrics(daily_data, {{ Variable.colname }}, {{ Datetime.colname }}, {{ Id.colname }})
  dplyr::bind_rows(daily_metrics, participant_metrics)
  
  if(programmatic.use){
    dplyr::bind_rows(daily_metrics, participant_metrics)
  } else dplyr::bind_rows(daily_metrics, participant_metrics) |> 
          dplyr::select(-SD, -plot, -type)
  
}

#' Create a GT summary table for light logger datasets
#' 
#' @param color Color used for histogram accents in the metrics section.
#' @param Variable.label Label used in the table footnote to describe the light
#'   variable.
#' @param histograms Logical indicating whether histogram spark lines should be
#'   added for metrics where applicable.
#' 
#' @return A [gt][gt::gt()] table.
#' 
#' @rdname summary_table
#' @export
#' @examples
#'
#' #sample.data.environment |> summary_table(coordinates = c(47,9))

summary_table <- function(dataset,
                          coordinates = NULL,
                          location = NULL,
                          site = NULL,
                          color = "grey",
                          Variable.colname = MEDI,
                          Datetime.colname = Datetime,
                          Id.colname = Id,
                          threshold.missing = 0.2,
                          Variable.label = "melanopic EDI (lx)",
                          histograms = TRUE) {

  overview <- summary_overview(dataset,
                               coordinates = coordinates,
                               location = location,
                               site = site,
                               Variable.colname = {{ Variable.colname }},
                               Datetime.colname = {{ Datetime.colname }},
                               Id.colname = {{ Id.colname }},
                               threshold.missing = threshold.missing,
                               programmatic.use = TRUE)

  complete_day_label <- paste0(
    "Days \u2265",
    round((1 - threshold.missing) * 100),
    "% complete"
  )
  participant_day_n <- overview$mean[overview$name == complete_day_label]
  participant_n <- overview$mean[overview$name == "Participants"]

  metrics <- summary_metrics(dataset,
                             Variable.colname = {{ Variable.colname }},
                             Datetime.colname = {{ Datetime.colname }},
                             Id.colname = {{ Id.colname }},
                             threshold.missing = threshold.missing,
                             programmatic.use = TRUE)

  table_data <-
    overview |>
    dplyr::bind_rows(metrics) |>
    order_table_rows()

  labels_vec <- c(
    dose = gt::md("D (lx\u00B7h)"),
    duration_above_250 = gt::md("TAT<sub>250</sub>"),
    `duration_within_1-10` = gt::md("TWT<sub>1-10</sub>"),
    duration_below_1 = gt::md("TBT<sub>1</sub>"),
    period_above_250 = gt::md("PAT<sub>250</sub>"),
    duration_above_1000 = gt::md("TAT<sub>1000</sub>"),
    first_timing_above_250 = gt::md("FLiT<sub>250</sub>"),
    mean_timing_above_250 = gt::md("MLiT<sub>250</sub>"),
    last_timing_above_250 = gt::md("LLiT<sub>250</sub>"),
    brightest_10h_midpoint = gt::md("M10<sub>midpoint</sub>"),
    darkest_5h_midpoint = gt::md("L5<sub>midpoint</sub>"),
    brightest_10h_mean = gt::md("M10<sub>mean</sub> (lx)"),
    darkest_5h_mean = gt::md("L5<sub>mean</sub> (lx)"),
    interdaily_stability = gt::md("IS"),
    intradaily_variability = gt::md("IV")
  )

  table_data <-
    table_data |>
    dplyr::mutate(symbol = dplyr::recode(name, !!!labels_vec))

  row_selection <- format_row_selection(table_data, complete_day_label)

  table_summary <-
    table_data |>
    dplyr::group_by(type) |>
    gt::gt(rowname_col = "name") |>
    gt::fmt_integer(rows = name %in% row_selection$counts) |>
    gt::fmt_percent(rows = name %in% row_selection$percent, decimals = 1) |>
    gt::fmt_duration(
      rows = name %in% row_selection$photoperiod,
      input_units = "hours",
      duration_style = "narrow",
      max_output_units = 2
    ) |>
    gt::fmt_duration(
      rows = name %in% row_selection$durations,
      input_units = "seconds",
      max_output_units = 2
    ) |>
    gt::fmt(
      columns = 3:6,
      rows = name %in% row_selection$time_of_day,
      fns = style_time
    ) |>
    gt::fmt_number(rows = name %in% row_selection$stability, decimals = 3) |>
    gt::fmt_number(rows = name %in% row_selection$brightness, decimals = 1) |>
    gt::fmt_markdown(columns = symbol) |>
    gt::cols_merge(
      columns = 3:6,
      pattern = "<strong>{1}</strong><< \u00B1{2}>> <<({3} - {4})>>"
    ) |>
    gt::sub_missing(columns = "symbol", missing_text = "") |>
    gt::cols_label(mean = "", symbol = "", plot = "") |>
    gt::fmt(columns = name, fns = \(x) {
      x |>
        stringr::str_to_sentence() |>
        stringr::str_replace_all("_", " ") |>
        stringr::str_replace("0$", "0 lx") |>
        stringr::str_replace("1$", "1 lx")
    }) |>
    gt::tab_header(
      "Summary table",
      subtitle = attr(overview, "location_string")
    ) |>
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = list(
        gt::cells_column_labels(columns = gt::everything()),
        gt::cells_row_groups()
      )
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = "left"),
      locations = list(
        gt::cells_body(columns = 3)
      )
    ) |>
    gt::tab_footnote(gt::md(
      paste0(
        "values show: **mean** \u00B1sd (min - max) and are all based on measurements of ",
        Variable.label
      )
    )) |>
    gt::tab_footnote(
      gt::md(
        "Values were log 10 transformed prior to averaging, with an offset of 0.1, and backtransformed afterwards"
      ),
      locations = gt::cells_stub(rows = c(17, 18))
    ) |>
    gt::tab_footnote(
      paste0(
        "Metrics are calculated on a by-participant-day basis (n=",
        participant_day_n,
        ") with the exception of IV and IS,\nwhich are calculated on a by-participant basis (n=",
        participant_n,
        ")."
      ),
      locations = gt::cells_row_groups("Metrics")
    ) |>
    gt::cols_move_to_start(symbol) |>
    gt::tab_options(
      column_labels.hidden = TRUE
    ) |>
    gt::tab_style(
      style = gt::cell_text(align = "right"),
      locations = list(
        gt::cells_body(columns = symbol)
      )
    )

  if (histograms) {
    table_summary <-
      table_summary |>
      gtExtras::gt_plt_dist(plot, type = "histogram", fill_color = color, line_color = NA, bw = 0.1) |>
      gt::cols_add(footnote = " ") |>
      gt::tab_footnote(
        "Histogram limits are set from 00:00 to 24:00",
        locations = gt::cells_body(footnote, rows = name %in% row_selection$histograms),
        placement = "left"
      )
  } else table_summary <- table_summary |> gt::cols_hide(plot)

  table_summary
}

# Helpers --------------------------------------------------------------------

missingness_summary <- function(dataset, Variable.colname) {
  dataset |>
    gap_handler(full.days = TRUE) |>
    durations({{ Variable.colname }}, show.missing = TRUE) |>
    dplyr::ungroup() |>
    dplyr::mutate(missingness = missing / total) |>
    dplyr::summarize(
      type = "Overview",
      name = "Missing/Irregular",
      missing = sum(missing, na.rm = TRUE),
      total = sum(total, na.rm = TRUE),
      min = min(missingness, na.rm = TRUE),
      max = max(missingness, na.rm = TRUE),
      mean = missing / total,
      SD = NA_real_,
      plot = list(missingness)
    ) |>
    dplyr::select(type, name, mean, SD, min, max, plot) |> 
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) round(x, 2)))
}

photoperiod_summary <- function(daily_data, coordinates, Datetime.colname, Id.colname) {
  if (!"photoperiod" %in% names(daily_data)) {
    if (is.null(coordinates)) {
      return(tibble::tibble())
    }

    Datetime.colname <-
      rlang::enexpr(Datetime.colname)
    
    daily_data <- 
      add_photoperiod(daily_data, 
                      coordinates, 
                      Datetime.colname = !!Datetime.colname, 
                      overwrite = TRUE)
  }

  daily_data |>
    dplyr::ungroup() |>
    dplyr::distinct({{ Id.colname }}, .Date, .keep_all = TRUE) |>
    dplyr::summarize(
      type = "Overview",
      name = "Photoperiod",
      mean = mean(photoperiod, na.rm = TRUE),
      SD = NA_real_,
      min = min(photoperiod, na.rm = TRUE),
      max = max(photoperiod, na.rm = TRUE),
      plot = list(photoperiod / 24)
    ) |>
    dplyr::mutate(dplyr::across(c(mean, min, max, SD), as.numeric))
}

summarise_daily_metrics <- function(dataset, Variable.colname, Datetime.colname) {
  dataset |>
    dplyr::summarize(
      dose({{ Variable.colname }}, {{ Datetime.colname }}, na.rm = TRUE, as.df = TRUE),
      duration_above_threshold(
        {{ Variable.colname }}, {{ Datetime.colname }},
        "above",
        250,
        na.rm = TRUE,
        as.df = TRUE
      ),
      duration_above_threshold(
        {{ Variable.colname }}, {{ Datetime.colname }},
        threshold = c(1, 10),
        na.rm = TRUE,
        as.df = TRUE
      ),
      duration_above_threshold(
        {{ Variable.colname }}, {{ Datetime.colname }},
        "below",
        1,
        na.rm = TRUE,
        as.df = TRUE
      ),
      period_above_threshold(
        {{ Variable.colname }}, {{ Datetime.colname }},
        "above",
        250,
        na.rm = TRUE,
        as.df = TRUE
      ),
      duration_above_threshold(
        {{ Variable.colname }}, {{ Datetime.colname }},
        "above",
        1000,
        na.rm = TRUE,
        as.df = TRUE
      ),
      timing_above_threshold(
        {{ Variable.colname }}, {{ Datetime.colname }},
        "above",
        250,
        na.rm = TRUE,
        as.df = TRUE
      ),
      bright_dark_period(
        log_zero_inflated({{ Variable.colname }}),
        {{ Datetime.colname }},
        "brightest",
        "10 hours",
        na.rm = TRUE,
        as.df = TRUE
      ),
      bright_dark_period(
        log_zero_inflated({{ Variable.colname }}),
        {{ Datetime.colname }},
        "darkest",
        "5 hours",
        loop = TRUE,
        na.rm = TRUE,
        as.df = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::select(dplyr::matches("dose|duration|period|timing|mean|midpoint")) |>
    Datetime2Time() |>
    dplyr::mutate(dplyr::across(c(brightest_10h_mean, darkest_5h_mean), exp_zero_inflated),
                  dplyr::across(dplyr::everything(), as.numeric)) |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::group_by(name) |>
    dplyr::summarize(
      type = "Metrics",
      dplyr::across(
        value,
        list(
          mean = \(x) mean(x, na.rm = TRUE),
          SD = \(x) if(dplyr::n() <= 2) NA else stats::sd(x, na.rm = TRUE),
          min = \(x) min(x, na.rm = TRUE),
          max = \(x) max(x, na.rm = TRUE),
          plot = \(x) metric_plot_values(unique(name), x)
        ),
        .names = "{.fn}"
      )
    ) 
}

summarise_participant_metrics <- function(dataset, Variable.colname, Datetime.colname, Id.colname) {
  dataset |>
    dplyr::group_by({{ Id.colname }}) |>
    dplyr::summarize(
      interdaily_stability({{ Variable.colname }}, {{ Datetime.colname }},
                           na.rm = TRUE, as.df = TRUE),
      intradaily_variability({{ Variable.colname }}, {{ Datetime.colname }},
                             na.rm = TRUE, as.df = TRUE),
      .groups = "drop"
    ) |>
    dplyr::select(-{{ Id.colname }}) |>
    tidyr::pivot_longer(dplyr::everything()) |>
    dplyr::group_by(name) |>
    dplyr::summarize(
      type = "Metrics",
      dplyr::across(
        value,
        list(
          mean = \(x) mean(x, na.rm = TRUE),
          SD = \(x) if(dplyr::n() <= 2) NA else stats::sd(x, na.rm = TRUE),
          min = \(x) min(x, na.rm = TRUE),
          max = \(x) max(x, na.rm = TRUE),
          plot =  \(x) metric_plot_values(unique(name), x)
          # n = \(x) dplyr::n()
        ),
        .names = "{.fn}"
      )
    )
}

metric_plot_values <- function(name, values) {
  values <- as.numeric(values)

  if (all(is.na(values))) {
    return(list(NA_real_))
  }

  scale_denominator <- max(values, na.rm = TRUE)
  if (is.infinite(scale_denominator) || is.nan(scale_denominator) || scale_denominator == 0) {
    scale_denominator <- 1
  }

  switch(
    name,
    first_timing_above_250 = list(values / (24 * 60 * 60)),
    mean_timing_above_250 = list(values / (24 * 60 * 60)),
    last_timing_above_250 = list(values / (24 * 60 * 60)),
    brightest_10h_midpoint = list(values / (24 * 60 * 60)),
    darkest_5h_midpoint = list(values / (24 * 60 * 60)),
    intradaily_variability = list(values / scale_denominator),
    list(values / scale_denominator)
  )
}

order_table_rows <- function(table_data) {
  metric_order <- c(
    "dose",
    "duration_above_250",
    "duration_within_1-10",
    "duration_below_1",
    "period_above_250",
    "duration_above_1000",
    "first_timing_above_250",
    "mean_timing_above_250",
    "last_timing_above_250",
    "brightest_10h_midpoint",
    "darkest_5h_midpoint",
    "brightest_10h_mean",
    "darkest_5h_mean",
    "interdaily_stability",
    "intradaily_variability"
  )

  overview_order <- c(
    "Participants",
    "Participant-days",
    table_data$name[table_data$type == "Overview"] |> setdiff(c("Participants", "Participant-days", "Missing/Irregular", "Photoperiod")) |> sort(),
    "Missing/Irregular",
    "Photoperiod"
  )

  table_data |>
    dplyr::mutate(
      type = factor(type, levels = c("Overview", "Metrics")),
      name = dplyr::case_when(
        type == "Metrics" ~ factor(name, levels = metric_order),
        TRUE ~ factor(name, levels = overview_order)
      )
    ) |>
    dplyr::arrange(type, name, .by_group = FALSE) |>
    dplyr::mutate(name = as.character(name), type = as.character(type))
}

format_row_selection <- function(table_data, complete_day_label) {
  count_rows <- c("Participants", "Participant-days", complete_day_label, "dose")
  percent_rows <- "Missing/Irregular"
  photoperiod_rows <- "Photoperiod"
  duration_rows <- c(
    "duration_above_250", "duration_within_1-10", "duration_below_1", "period_above_250", "duration_above_1000"
  )
  time_rows <- c(
    "first_timing_above_250", "mean_timing_above_250", "last_timing_above_250",
    "brightest_10h_midpoint", "darkest_5h_midpoint"
  )
  brightness_rows <- c("brightest_10h_mean", "darkest_5h_mean")
  stability_rows <- c("interdaily_stability", "intradaily_variability")

  present_rows <- table_data$name

  list(
    counts = intersect(count_rows, present_rows),
    percent = intersect(percent_rows, present_rows),
    photoperiod = intersect(photoperiod_rows, present_rows),
    durations = intersect(duration_rows, present_rows),
    time_of_day = intersect(time_rows, present_rows),
    brightness = intersect(brightness_rows, present_rows),
    stability = intersect(stability_rows, present_rows),
    histograms = intersect(c(photoperiod_rows, time_rows), 
                           present_rows)
  )
}

location_string <- function(dataset, coordinates, location, site, Datetime.colname) {
  tzone <- dataset |>
    dplyr::pull({{ Datetime.colname }}) |>
    lubridate::tz()

  coordinate_string <-
    if (!is.null(coordinates)) {
      format_coordinates(coordinates)
    } else {
      NULL
    }

  location_bits <- c(location, site)
  location_bits <- location_bits[!purrr::map_lgl(location_bits, is.null)]
  location_bits <- location_bits[location_bits != ""]

  location_part <- paste(location_bits, collapse = ", ")
  combined <- paste(c(location_part, coordinate_string), collapse = ", ") |>
    stringr::str_replace_all("^, |, $", "")

  if (is.null(tzone) || tzone == "") {
    combined
  } else if (combined == "") {
    paste0("TZ: ", tzone)
  } else {
    paste0(combined, ", TZ: ", tzone)
  }
}


#' Tabular summary of data and gaps in all groups
#'
#' [gap_table()] creates a [gt::gt()] with one row per group, summarizing key
#' gap and gap-related information about the dataset. These include the
#' available data, total duration, number of gaps, missing implicit and explicit
#' data, and, optionally, irregular data.
#'
#' @inheritParams extract_gaps
#' @param Variable.colname Column name of the variable to check for NA values.
#'   Expects a symbol.
#' @param check.irregular Logical on whether to include irregular data in the
#'   summary, i.e. data points that do not fall on the regular sequence.
#' @param Variable.label Clear name of the variable. Expects a string
#' @param title Title string for the table
#' @param get.df Logical whether the dataframe should be returned instead of a
#'   [gt::gt()] table
#'
#' @returns A gt table about data and gaps in the dataset
#' @export
#'
#' @examples
#' sample.data.environment |> dplyr::filter(MEDI <= 50000) |> gap_table()
gap_table <- function(dataset,
                      Variable.colname = MEDI,
                      Variable.label = "melanopic EDI",
                      title = "Summary of available and missing data",
                      Datetime.colname = Datetime,
                      epoch = "dominant.epoch",
                      full.days = TRUE,
                      include.implicit.gaps = TRUE,
                      check.irregular = TRUE,
                      get.df = FALSE) {
  dat1 <- 
    dataset |>
    extract_gaps({{ Variable.colname }},
                 {{ Datetime.colname }},
                 epoch = epoch,
                 full.days = full.days,
                 include.implicit.gaps = include.implicit.gaps
    ) |>
    summarize_numeric(gap.id) |>
    dplyr::rename(gaps = episodes,
                  epoch = mean_epoch,
                  mean_gap_duration = mean_duration,
                  total_gap_duration = total_duration)
  
  full_data <- 
    if(include.implicit.gaps) {
      dataset |>
        gap_handler({{ Datetime.colname }},
                    epoch = epoch,
                    full.days = full.days
        )
    } else dataset
  
  dat2 <- 
    full_data |> 
    durations({{ Variable.colname }},
              {{ Datetime.colname }},
              show.interval = TRUE,
              show.missing = TRUE
    )
  
  dat3 <- 
    if(nrow(dat1) != 0) {
    dat2 |> 
    dplyr::left_join(dat1, by = dplyr::group_vars(dat2)) |> 
    dplyr::select(!!!dplyr::groups(dat2), 
                  available_data = duration,
                  missing_data = missing,
                  total_duration = total,
                  interval,
                  number_gaps = gaps,
                  mean_gap_duration)
    } else {
      dat2 |> 
        dplyr::select(!!!dplyr::groups(dat2), 
                  available_data = duration,
                  missing_data = missing,
                  total_duration = total,
                  interval) |> 
        dplyr::mutate(
                  number_gaps = 0,
                  mean_gap_duration = lubridate::duration(0)
                  )
    }
  
  dat4 <- 
    if(include.implicit.gaps) {
      implicit <- 
        full_data |> 
        durations(is.implicit,
                  {{ Datetime.colname }},
                  show.interval = TRUE,
                  show.missing = TRUE, FALSE.as.NA = TRUE) |> 
        dplyr::select(!!!dplyr::groups(full_data), missing_implicit = duration)
      
      dat3 |> dplyr::left_join(implicit, by = dplyr::group_vars(dat3)) |> 
        dplyr::mutate(missing_explicit = missing_data - missing_implicit)
    } else dat3
  
  dat5 <- 
    if(check.irregular) {
      irregulars <- 
        dataset |> gap_handler({{ Datetime.colname }}, 
                               epoch = epoch,
                               behavior = "irregulars", 
                               full.days = full.days) |> 
        dplyr::summarize(number_irregulars = dplyr::n())
      dat4 |> 
        dplyr::left_join(irregulars, by = dplyr::group_vars(dat4)) |> 
        dplyr::mutate(number_irregulars = ifelse(is.na(number_irregulars), 0, number_irregulars))
    } else dat4
  
  dat6 <- 
  dat5 |> dplyr::mutate(
    dplyr::across(
      dplyr::any_of(c("available_data", "missing_data", 
                      "missing_implicit", "missing_explicit"
      )), 
      \(x) x/total_duration, 
      .names = "{.col}_pct") 
  ) |> 
    dplyr::relocate(missing_data, .before = number_gaps) |> 
    dplyr::relocate(interval, .before = available_data) |> 
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c("available_data", "total_duration", "missing_data", "mean_gap_duration", "missing_implicit", "missing_explicit")),
      \(x) x/interval, .names = "{.col}_n"
    )) |> 
    dplyr::mutate(dplyr::across(dplyr::contains("gap"), \(x) ifelse(is.na(x), 0, x)))
    
    if(get.df) return(dat6) else gt_gaps(dat6, Variable = Variable.label, title = title)
}


gt_gaps <- function(
    gap_table,
    Variable = "melanopic EDI",
    title = "Summary of available and missing data"
) {
  
  vertical_lines <- c("number_irregulars", "total_duration", "interval", 
                      "number_gaps", "missing_implicit", "missing_explicit")
  
  gap_table |> 
    gt::gt() |> 
    gt::tab_header(title = title,
                   subtitle = paste0("Variable: ", Variable)) |> 
    gt::tab_spanner(label = "Regular", columns = dplyr::contains("available_data")) |> 
    gt::tab_spanner(label = "Range", columns = dplyr::contains("total_duration")) |> 
    gt::tab_spanner(label = "Implicit", 
                    columns = dplyr::contains("implicit"),
                    id = "implicit_spanner") |> 
    gt::tab_spanner(label = "Explicit", 
                    columns = dplyr::contains("explicit"),
                    id = "explicit_spanner") |> 
    gt::tab_spanner(label = "Gaps", 
                    columns = c(number_gaps, mean_gap_duration, mean_gap_duration_n,
                                dplyr::contains("missing_data")),
                    id = "gap_spanner") |> 
    gt::tab_spanner(label = "Interval", 
                    columns = interval,
                    id = "interval_spanner") |> 
    gt::tab_spanner(label = "Irregular", 
                    columns = dplyr::contains("irregulars"),
                    id = "irregular_spanner") |> 
    gt::tab_spanner(label = "Data", 
                    columns = !c(dplyr::contains("missing"), number_gaps, mean_gap_duration, mean_gap_duration_n), 
                    id = "data_spanner") |> 
    gt::tab_spanner(label = "Missing", 
                    columns = c(dplyr::contains("missing"), number_gaps, mean_gap_duration, mean_gap_duration_n), 
                    id = "missing_spanner") |> 
    gt::cols_label(interval = "Interval",
                   available_data = "Time",
                   interval = "Time",
                   dplyr::contains("_pct") ~ "%",
                   dplyr::contains("_n") ~ "n",
                   mean_gap_duration_n = "\U{00F8}n",
                   missing_data = "Time",
                   number_gaps = "N",
                   dplyr::contains("number_irregulars") ~ "n",
                   mean_gap_duration = "\U{00F8}",
                   dplyr::any_of(c("missing_implicit", "missing_explicit")) ~ "Time",
                   total_duration = "Time") |> 
    gt::fmt_percent(columns = dplyr::contains("pct"), decimals = 1) |> 
    gt::fmt_number(dplyr::contains("_n"), accounting = TRUE, decimals = 0) |> 
    gt::fmt_duration(columns = dplyr::any_of(c("available_data", "total_duration", "missing_data", "missing_implicit", "missing_explicit", "mean_gap_duration")), 
                     input_units = "seconds") |> 
    gt::cols_move(number_irregulars, after = available_data_n) |> 
    gt::cols_move(interval, after = total_duration_n) |> 
    gt::grand_summary_rows(
      fns = list(label = gt::md("**Overall**"), id = "totals", fn = "sum"),
      side = "top",
      fmt = ~ gt::fmt_integer(.),
      columns = c(dplyr::any_of(c("number_gaps", "number_irregulars")), dplyr::contains("_n"))
    ) |> 
    gt::grand_summary_rows(
      fns = list(label = gt::md("**Overall**"), id = "totals", fn = "sum"),
      side = "top",
      fmt = ~ gt::fmt_duration(., input_units = "seconds"),
      columns = c(dplyr::any_of(c("available_data", "total_duration", "mean_gap_duration", "missing_data", "missing_implicit", "missing_explicit")))
    ) |> 
    gt::grand_summary_rows(
      fns = list(label = gt::md("**Overall**"), id = "totals") ~ sum(available_data)/sum(total_duration),
      side = "top",
      fmt = ~ gt::fmt_percent(., decimals = 1),
      columns = dplyr::contains("available_data_pct")
    ) |> 
    gt::grand_summary_rows(
      fns = list(label = gt::md("**Overall**"), id = "totals") ~ sum(missing_data)/sum(total_duration),
      side = "top",
      fmt = ~ gt::fmt_percent(., decimals = 1),
      columns = dplyr::contains("missing_data_pct")
    ) |> 
    gt::grand_summary_rows(
      fns = list(label = gt::md("**Overall**"), id = "totals") ~ sum(missing_implicit)/sum(total_duration),
      side = "top",
      fmt = ~ gt::fmt_percent(., decimals = 1),
      columns = dplyr::contains("missing_implicit_pct")
    ) |> 
    gt::grand_summary_rows(
      fns = list(label = gt::md("**Overall**"), id = "totals") ~ sum(missing_explicit)/sum(total_duration),
      side = "top",
      fmt = ~ gt::fmt_percent(., decimals = 1),
      columns = dplyr::contains("missing_explicit_pct")
    ) |> 
    gt::grand_summary_rows(
      fns = list(label = gt::md("**Overall**"), id = "totals") ~ paste(unique(.), collapse = "; "),
      side = "top",
      columns = dplyr::any_of("interval")
    ) |> 
    gt::tab_footnote("If n > 0: it is possible that the other summary statistics are affected, as they are calculated based on the most prominent interval.", locations = gt::cells_column_labels(dplyr::any_of("number_irregulars"))) |> 
    gt::tab_footnote("Number of (missing or actual) observations", locations = gt::cells_column_labels(c(dplyr::contains("_n"), dplyr::any_of("number_irregulars")))) |> 
    gt::tab_footnote("Based on times, not necessarily number of observations", locations = gt::cells_grand_summary(c(dplyr::contains("_pct")))) |> 
    gt::tab_style(style = gt::cell_text(weight = "bold"), locations = gt::cells_column_spanners()) |> 
    gt::tab_style(style = gt::cell_fill(color = "grey90"), locations = list(gt::cells_grand_summary(),gt::cells_stub_grand_summary())) |> 
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("left"),
        weight = gt::px(1), color = "grey80"),
      locations = list(gt::cells_body(
        columns = c(dplyr::any_of(vertical_lines))),
        gt::cells_grand_summary(columns = dplyr::any_of(vertical_lines))
      )
    )
}
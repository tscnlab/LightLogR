#this script creates a four-panel overview plot
# library(gtExtras)

light_summary_table <- function(
    dataset, #light exposure dataset
    coordinates, #latitude, longitude
    location, #city, location, etc. (string)
    site, #country code
    color = "grey", #country color for histograms
    Variable.colname = MEDI, #column in dataset containing the light value
    Datetime.colname = Datetime, #column in dataset containing the datetime value
    Id.colname = Id,  #column in dataset containing the Participant Id
    threshold.missing = 0.2, #threshold for missing data
    Variable.label = "melanopic EDI (lx)", #label of the light variable
    histograms = TRUE #display histograms
) {
  
  #number of participants
  n_part <-
    dataset|>
    group_by({{ Id.colname }}) |>
    n_groups()
  
  # percent missingness/irregular
  p_missing <-
    dataset |>
    gap_handler(full.days = TRUE) |>
    durations({{ Variable.colname }}, show.missing = TRUE) |>
    ungroup() |>
    mutate(
      missingness = missing / total
    ) |>
    summarize(
      type = "Overview",
      name = "Missing/Irregular",
      missing = sum(missing, na.rm = TRUE),
      total = sum(total, na.rm = TRUE),
      min = min(missingness, na.rm = TRUE),
      max = max(missingness, na.rm = TRUE),
      mean = missing / total,
      sd = NA,
      plot = list(missingness)
    ) |>
    select(type, name, mean, sd, min, max, plot)
  
  # number of participant-days
  data_part_days <- 
    dataset |>
    group_by({{ Id.colname }}) |>
    add_Date_col(.Date, group.by = TRUE)
  
  n_part_days <-
    data_part_days |>
    n_groups()
  
  # number of participant-days with a coverage of at least 80%
  
  data_part_days_suff <-
    data_part_days |>
    remove_partial_data({{ Variable.colname }}, 
                        threshold.missing = threshold.missing, 
                        handle.gaps = TRUE)
  
  n_part_days_suff <-
    data_part_days_suff |>
    n_groups()
  
  # location and time zone
  
  tzone <- dataset |> pull(Datetime) |> tz()
  
  coordinate_string <- format_coordinates(coordinates)
  
  location_string <-
    paste0(location, ", ", site, " (", coordinates_string, "), TZ: ", tzone)
  
  # average photoperiod
  
  photoperiods <- if("photoperiod" %in% names(dataset)) {
    data_part_days|>
      ungroup()
  } else {
    data_part_days |>
      add_photoperiod(coordinates)|>
      ungroup()
  }
  
  photoperiod_data <-
    photoperiods |> 
    distinct({{ Id.colname }}, .Date, .keep_all = TRUE) |> 
    summarize(
      type = "Overview",
      name = "Photoperiod",
      mean = mean(photoperiod, na.rm = TRUE),
      sd = NA,
      min = min(photoperiod, na.rm = TRUE),
      max = max(photoperiod, na.rm = TRUE),
      plot = list(photoperiod/24)
    ) |>
    mutate(across(c(mean, min, max, sd), as.numeric))
  
  # metrics
  
  metrics_daily_all <-
    data_part_days_suff |>
    summarize(
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
    select(matches("dose|duration|period|timing|mean|midpoint")) |>
    Datetime2Time() |>
    mutate(across(c(brightest_10h_mean, darkest_5h_mean), exp_zero_inflated))
  
  metrics_daily <-
    metrics_daily_all |>
    mutate(across(everything(), as.numeric)) |>
    pivot_longer(everything()) |>
    group_by(name) |>
    summarize(
      type = "Metrics",
      across(
        value,
        list(
          mean = \(x) mean(x, na.rm = TRUE),
          sd = \(x) sd(x, na.rm = TRUE),
          min = \(x) min(x, na.rm = TRUE),
          max = \(x) max(x, na.rm = TRUE),
          plot = \(x) switch(unique(name),
                             first_timing_above_250 = list(x/(24*60*60)),
                             mean_timing_above_250 = ,
                             last_timing_above_250 = ,
                             brightest_10h_midpoint = ,
                             darkest_5h_midpoint = ,
                             list(x/max(x, na.rm = TRUE))
                             
          ) 
        ),
        .names = "{.fn}"
      )
    )
  
  metrics_overall <-
    data_part_days_suff |>
    group_by({{ Id.colname }}) |>
    summarize(
      interdaily_stability({{ Variable.colname }}, {{ Datetime.colname }},
                           na.rm = TRUE, as.df = TRUE),
      intradaily_variability({{ Variable.colname }}, {{ Datetime.colname }},
                             na.rm = TRUE, as.df = TRUE),
      .groups = "drop"
    ) |>
    select(-{{ Id.colname}} ) |>
    pivot_longer(everything()) |>
    group_by(name) |>
    summarize(
      type = "Metrics",
      across(
        value,
        list(
          mean = \(x) mean(x, na.rm = TRUE),
          sd = \(x) sd(x, na.rm = TRUE),
          min = \(x) min(x, na.rm = TRUE),
          max = \(x) max(x, na.rm = TRUE),
          plot =  \(x) switch(unique(name),
                              intradaily_variability = list(x/max(x, na.rm = TRUE)),
                              list(x)
          ) 
        ),
        .names = "{.fn}"
      )
    )
  
  # combining all the data
  
  labels_vec <- c(
    NA,
    NA,
    NA,
    NA,
    NA,
    dose = md("D (lx·h)"),
    dur_above250 = md("TAT<sub>250</sub>"),
    dur_1_10 = md("TWT<sub>1–10</sub>"),
    dur_below1 = md("TBT<sub>1</sub>"),
    period_above250 = md("PAT<sub>250</sub>"),
    dur_above1000 = md("TAT<sub>1000</sub>"),
    first_above250 = md("FLiT<sub>250</sub>"),
    mean_above250 = md("MLiT<sub>250</sub>"),
    last_above250 = md("LLiT<sub>250</sub>"),
    bright10_mid = md("M10<sub>midpoint</sub>"),
    dark5_mid = md("L5<sub>midpoint</sub>"),
    bright10_mean = md("M10<sub>mean</sub> (lx)"),
    dark5_mean = md("L5<sub>mean</sub> (lx)"),
    IS = md("IS"),
    IV = md("IV")
  )
  
  table_summary_overall <-
    tibble(
      type = "Overview",
      name = c(
        "Participants",
        "Participant-days",
        glue("Days ≥{round((1-threshold.missing)*100)}% complete")
      ),
      mean = c(n_part, n_part_days, n_part_days_suff),
      sd = NA,
      min = NA,
      max = NA,
      plot = NA
    ) |>
    add_row(
      p_missing
    ) |>
    add_row(
      photoperiod_data
    )
  
  table_summary_data <-
    table_summary_overall |>
    add_row(
      metrics_daily
    ) |>
    add_row(
      metrics_overall
    ) |>
    add_column(
      row = c(
        1,
        2,
        3,
        4,
        5,
        17,
        15,
        18,
        16,
        6,
        11,
        7,
        9,
        8,
        12,
        14,
        13,
        10,
        19,
        20
      )
    ) |>
    group_by(type) |>
    arrange(row) |>
    select(-row) |>
    add_column(
      symbol = labels_vec
    )
  
  table_summary <-
    table_summary_data |>
    gt(rowname_col = "name") |>
    fmt_integer(rows = c(1, 2, 3, 6)) |>
    fmt_percent(rows = 4) |>
    fmt_duration(
      rows = 5,
      input_units = "hours",
      duration_style = "narrow",
      max_output_units = 2
    ) |>
    fmt_duration(rows = c(7:11), input_units = "seconds", max_output_units = 2) |>
    fmt(
      columns = 3:6,
      rows = c(12:16),
      fns = style_time
    ) |>
    fmt_number(rows = 19:20, decimals = 3) |>
    fmt_number(rows = c(17, 18), decimals = 1) |>
    fmt_markdown(columns = symbol) |>
    cols_merge(
      columns = 3:6,
      pattern = "<strong>{1}</strong><< ±{2}>> <<({3} - {4})>>"
    ) |>
    sub_missing(columns = "symbol", missing_text = "") |>
    cols_label(mean = "", symbol = "", plot = "") |>
    fmt(columns = name, fns = \(x) {
      x |>
        str_to_sentence() |>
        str_replace_all("_", " ") |>
        str_replace("0$", "0 lx") |>
        str_replace("1$", "1 lx")
    }) |>
    tab_header(
      "Summary table",
      subtitle = location_string
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(
        cells_column_labels(columns = everything()),
        cells_row_groups()
      )
    ) |>
    tab_style(
      style = cell_text(align = "left"),
      locations = list(
        cells_body(columns = 3)
      )
    ) |>
    tab_footnote(md(glue(
      "values show: **mean** ±sd (min - max) and are all based on measurements of {Variable.label}" 
    ))) |>
    tab_footnote(
      md(
        "Values were log 10 transformed prior to averaging, with an offset of 0.1, and backtransformed afterwards"
      ),
      locations = cells_stub(rows = c(17, 18))
    ) |>
    tab_footnote(
      glue("Metrics are calculated on a by-participant-day basis (n={table_summary_data[3,3]}) with the exception of IV and IS, which are calculated on a by-participant basis (n={table_summary_data[1,3]})."),
      locations = cells_row_groups("Metrics")
    ) |> 
    cols_move_to_start(symbol) |> 
    tab_options(
      column_labels.hidden = TRUE
    ) |> 
    tab_style(
      style = cell_text(align = "right"),
      locations = list(
        cells_body(columns = symbol)
      )
    )
  
  if(histograms) {
    table_summary <-
      table_summary |> 
      gt_plt_dist(plot, type = "histogram", fill_color = color, line_color = NA, bw = 0.1) |>
      cols_add(footnote = " ") |> 
      tab_footnote(
        "Histogram limits are set from 00:00 to 24:00",
        locations = cells_body(footnote, rows = c(5, 12, 13, 14, 15, 16)),
        placement = "left"
      ) 
    
  }
  # text_transform(
  #   locations = cells_body(columns = plot),
  #   fn = function(list_of_num_vectors) {
  #     # list_of_num_vectors is your list-column (each element is a numeric vector)
  #     map(
  #       table_summary_data$plot,
  #       ~ if(all(.x != "", all(!is.na(.x)), !is.null(.x))){
  #         # browser()
  #         gt::ggplot_image(
  #           ggplot(data.frame(x = .x), aes(x)) +
  #             geom_hline(yintercept = 0, linewidth = 1, color = alpha(color, 0.5)) +
  #             geom_histogram(binwidth = 0.1, fill = color) +
  #             coord_cartesian(xlim = c(0,1), expand = FALSE) +
  #             theme_void() +
  #             theme(
  #               plot.margin = margin(2, 2, 2, 2),
  #               panel.background = element_rect(fill = "white", colour = NA)
  #             ),
  #           height = px(30),        # cell height
  #           aspect_ratio = 3        # width:height
  #         )
  #       } else ""
  #     )
  #   }
  # )
  
  table_summary
  
}
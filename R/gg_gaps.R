#' Visualize gaps and irregular data
#'
#' [gg_gaps()] is built upon [gg_days()], [gap_finder()], and [gg_states()] to
#' visualize where gaps and irregular data in a dataset are. The function does
#' not differentiate between `implicit gaps`, which are missing timestamps of
#' the regular interval, `explicit gaps`, which are `NA` values. Optionally, the
#' function shows `irregular data`, which are datapoints that fall outside the
#' regular interval.
#'
#' @inheritParams gg_days
#' @param alpha A numerical value between 0 and 1 representing the transparency
#'   of the gaps Default is 0.5.
#' @param Datetime.colname The column that contains the datetime. Needs to be a
#'   `POSIXct` and part of the dataset.
#' @param ... Additional arguments given to [gg_days()]. Can be used to change
#'   the color or other aesthetic properties.
#' @param epoch The epoch to use for the gapless sequence. Can be either a
#'   `lubridate::duration()` or a string. If it is a string, it needs to be
#'   either '"dominant.epoch"' (the default) for a guess based on the data or a
#'   valid `lubridate::duration()` string, e.g., `"1 day"` or `"10 sec"`.
#' @param on.top Logical scalar. If `TRUE`, the states will be plotted on top of
#'   the existing plot. If `FALSE`, the states will be plotted underneath the
#' @param Variable.colname Variable that becomes the basis for gap analysis.
#'   expects a symbol
#' @param fill.gaps Fill color for the gaps
#' @param col.irregular Dot color for irregular data
#' @param full.days Logical. Whether full days are expected, even for the first
#'   and last measurement
#' @param show.irregulars Logical. Show irregular data points. Default is
#'   `FALSE`.
#' @param group.by.days Logical. Whether data should be grouped by days. This
#'   can make sense if only very few days from large groups are affected
#' @param include.implicit.gaps Logical. Whether the time series should be expanded only the current observations taken.
#' @returns a `ggplot` object with all gaps and optionally irregular data.
#'   Groups that do not have any gaps nor irregular data will be removed for
#'   clarity. Null if no groups remain
#' @export
#'
#' @examples
#' #calling gg_gaps on a healthy dataset is pointless
#' sample.data.environment |> gg_gaps()
#'
#' #creating a gapped and irregular dataset
#' bad_dataset <-
#' sample.data.environment |>
#'   aggregate_Datetime(unit = "5 mins") |>
#'   dplyr::filter(Id == "Participant") |> 
#'   filter_Date(length = "2 days") |>
#'   dplyr::mutate(
#'    Datetime = dplyr::if_else(
#'      lubridate::date(Datetime) == max(lubridate::date(Datetime)),
#'            Datetime, Datetime + 1
#'      )
#'    ) |>
#' dplyr::filter(MEDI <250)
#' bad_dataset |> has_gaps()
#' bad_dataset |> has_irregulars()
#'
#' #by default, gg_gaps() only shows gaps
#' bad_dataset |> gg_gaps()
#'
#' #it can also show irregular data
#' bad_dataset |> gg_gaps(show.irregulars = TRUE)

gg_gaps <- function(dataset, 
                     Variable.colname = MEDI,
                     Datetime.colname = Datetime,
                     fill.gaps = "red",
                     col.irregular = "red",
                     alpha = 0.5,
                     on.top = FALSE,
                     epoch = "dominant.epoch",
                     full.days = TRUE,
                     show.irregulars = FALSE,
                     group.by.days = FALSE,
                     include.implicit.gaps = TRUE,
                     ...) {
  
  # Initial Checks ----------------------------------------------------------
  
  
  
  stopifnot(
  "The given dataset is not a dataframe" = is.data.frame(dataset),
  "Datetime.colname must be part of the dataset" =
    colname.defused({{ Datetime.colname }}) %in% names(dataset),
  "The given column for X is not a Datetime" =
    lubridate::is.POSIXct(dataset[[colname.defused({{ Datetime.colname }})]]),
  "full.days must be a logical" = is.logical(full.days),
  "show.irregulars must be a logical" = is.logical(show.irregulars),
  "on.top must be a logical" = is.logical(on.top),
  "group.by.days must be a logical" = is.logical(group.by.days),
  "include.implicit.gaps must be a logical" = is.logical(include.implicit.gaps) 
  )
  
  # Function ----------------------------------------------------------
  
  irregulars <- tibble::tibble()
  
  if(group.by.days) {
    dataset <- 
      dataset |> 
      dplyr::group_by(days = lubridate::date({{ Datetime.colname }}), .add = TRUE)
  }
  
  if(show.irregulars) {
    irregulars <- 
      dataset |> gap_handler({{ Datetime.colname }}, 
                             epoch = epoch,
                             behavior = "irregulars")
  }
  
  data_regular <- if(include.implicit.gaps) {
    dataset |> 
      gap_handler(Datetime.colname = {{ Datetime.colname }},
                  epoch = epoch,
                  full.days = full.days)
  } else dataset
  
  data_regular <- 
  data_regular|> 
    dplyr::mutate(gaps = is.na({{ Variable.colname }}), .before = 1) |> 
    dplyr::filter(any(gaps))
  
  if(nrow(data_regular) == 0 & nrow(irregulars) == 0) {
    message("No gaps nor irregular values were found. Plot creation skipped")
    return(invisible())
  }
  
  if(nrow(data_regular) == 0) {
    dataset <- 
      dataset |> 
      dplyr::left_join(irregulars |> 
                         dplyr::mutate(is.irregular = TRUE) |> 
                         dplyr::select(!!!dplyr::groups(irregulars), {{Datetime.colname}}, is.irregular), 
                       by=c(dplyr::group_vars(irregulars), colname.defused({{ Datetime.colname }}))) |> 
      dplyr::filter(any(is.irregular))
    plot <- 
    dataset |> 
      gg_days(y.axis = {{ Variable.colname }}, x.axis = {{ Datetime.colname }},...) +
      ggplot2::geom_point(data = irregulars, col = col.irregular) +
      ggplot2::labs(title = paste0("Irregular data (", col.irregular ," dots)"))
    
    return(plot)
  }
  
  if(nrow(irregulars) == 0) {
    
    plot <- 
    data_regular |> 
      gg_days(y.axis = {{ Variable.colname }}, x.axis = {{ Datetime.colname }},...) |> 
      gg_states(gaps, fill = fill.gaps, alpha = alpha) +
      ggplot2::labs(title = paste0("Gaps in the data (", fill.gaps ," areas)"))
    
    return(plot)
  }
  
  data_regular |> 
    gg_days(y.axis = {{ Variable.colname }}, x.axis = {{ Datetime.colname }},...) |> 
    gg_states(gaps, fill = fill.gaps, alpha = alpha) +
    ggplot2::geom_point(data = irregulars, col = col.irregular) +
    ggplot2::labs(title = paste0("Gaps (", fill.gaps ," areas) and irregular (", col.irregular ," dots) data"))

}

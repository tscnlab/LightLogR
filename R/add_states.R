#' Add states to a dataset based on groups and start/end times
#'
#' [add_states()] brings states to a time series dataset. It uses the
#' `States.dataset` to add states to the `dataset`. The `States.dataset` must at
#' least contain the same variables as the `dataset` grouping, as well as a
#' start and end time (or an interval column). Beware if both datasets operate
#' on different time zones and consider to set `force.tz = TRUE`.
#'
#' Beware if columns in the `dataset` and `States.dataset` have the same name
#' (other then grouping variables). The underlying function,
#' [dplyr::left_join()] will mark the columns in the `dataset` with a suffix
#' `.x`, and in the `States.dataset` with a suffix `.y`.
#'
#' Also be careful if grouping variables have the same name, but a different
#' class - usually, this will result in an error that can be fixed by making
#' sure the classes are identical. This is especially an issue with labelled
#' variables (e.g., a labelled `Id` factor-variable in the main dataset, and a
#' factor variable `Id` in the state dataset) - in those cases, either the
#' unlabelled variable has to be labelled as well, or the other one unlabelled.
#'
#'
#' @param dataset A light logger dataset. Needs to be a dataframe.
#' @param States.dataset A light logger dataset. Needs to be a dataframe. This
#'   dataset must contain the same variables as the `dataset` grouping, as well
#'   as a start and end time. Any other column, that is not in `leave.out` will
#'   be added to the dataset.
#' @param Datetime.colname The column that contains the datetime. Needs to be a
#'   `POSIXct` and part of the dataset.
#' @param start.colname,end.colname The columns that contain the start and end
#'   time. Need to be `POSIXct` and part of the `States.dataset`. Can also
#'   be an `Interval` column, in which case the start and end times will be
#'   extracted via `lubridate` functions.
#' @param leave.out A character vector of columns that should not be carried
#'   over to the `dataset`
#' @param force.tz If `TRUE`, the start and end times of the `States.dataset`
#'   will be forced to the same time zone as the `dataset` using
#'   [lubridate::force_tz()]. If `FALSE` (default), the start and end times of
#'   the `States.dataset` will be used as is.
#'
#' @returns a modified `dataset` with the states added. The states are added as
#'   new columns to the `dataset`. The columns are named after the columns in
#'   the `States.dataset`, except for the start and end times, which are
#'   removed.
#' @export
#'
#' @examples
#' states <-
#' sample.data.environment |>
#'   filter_Date(length = "1 day") |>
#'   extract_states(Daylight, MEDI > 1000)
#'
#' states |> head(2)
#'
#' #add states to a dataset and plot them - as we only looked for states on the
#' # first day (see above), only the first day will show up in the plot
#' sample.data.environment |>
#'  filter_Date(length = "2 day") |>
#'  add_states(states) |>
#'  gg_days() |>
#'  gg_states(Daylight)

add_states <- function(dataset,
                       States.dataset,
                       Datetime.colname = Datetime,
                       start.colname = start,
                       end.colname = end,
                       force.tz = FALSE,
                       leave.out = c("duration", "epoch")
){
  # Initial Checks ----------------------------------------------------------
  
  # Check if dataset is a dataframe
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "States.dataset is not a dataframe" = is.data.frame(States.dataset)
  )
  
  # Check if Datetime.colname is part of the dataset
  Datetime.colname.defused <- colname.defused({{ Datetime.colname }})
  if (!Datetime.colname.defused %in% names(dataset)) {
    stop("Datetime.colname must be part of the dataset")
  }
  
  # Check if start.colname is part of the dataset
  start.colname.defused <- colname.defused({{ start.colname }})
  if (!start.colname.defused %in% names(States.dataset)) {
    stop("start.colname must be part of the States.dataset")
  }
  
  # Check if end.colname is part of the dataset
  end.colname.defused <- colname.defused({{ end.colname }})
  if (!end.colname.defused %in% names(States.dataset)) {
    stop("end.colname must be part of the States.dataset")
  }
  
  # Check if leave.out is a character vector
  if (!is.character(leave.out)) {
    stop("leave.out must be a character vector of columns in the `States.dataset")
  }
  
  # Check if Datetime.colname is a POSIXct
  if (!lubridate::is.POSIXct(dataset[[Datetime.colname.defused]])) {
    stop("Datetime.colname must be a POSIXct")
  }
  
  # Check if start.colname is a POSIXct or an interval column
  if (!(States.dataset[[start.colname.defused]] |> 
        inherits(c("POSIXct", "Interval")))) {
    stop("start.colname must be a POSIXct or Interval")
  }
  
  # Check if end.colname is a POSIXct or an interval column
  if (!(States.dataset[[end.colname.defused]] |> 
        inherits(c("POSIXct", "Interval")))) {
    stop("end.colname must be a POSIXct or Interval")
  }
  
  # Check if force.tz is a logical
  if (!is.logical(force.tz)) {
    stop("force.tz must be a logical")
  }
  
  
  # Function ----------------------------------------------------------
  
  # Check if the dataset grouping variables are present in the States.dataset
  # and error if not
  groups <- dplyr::groups(dataset)
  
  if (!all(dplyr::group_vars(dataset) %in% names(States.dataset))) {
    stop("The grouping variables in the dataset must be present in the States.dataset")
  }
  
  # If start or end are interval columns, extract their start/end information
  if(States.dataset[[start.colname.defused]] |> inherits(c("Interval"))) {
    States.dataset <- 
      States.dataset |> 
      dplyr::mutate(.start = lubridate::int_start({{ start.colname }})
                    )
    if(colname.defused({{ start.colname }}) != colname.defused({{ end.colname }})) {
      States.dataset <-
        States.dataset |> dplyr::select(- {{ start.colname }})
    }
    start.colname <- rlang::expr(.start)
  }
  if(States.dataset[[end.colname.defused]] |> inherits(c("Interval"))) {
    States.dataset <- 
      States.dataset |> 
      dplyr::mutate(.end = lubridate::int_end({{ end.colname }})
                    ) |> 
      dplyr::select(-{{ end.colname }})
    end.colname <- rlang::expr(.end)
  }
  
  
  # If force.tz is TRUE, convert the start/end columns of the States.dataset to the same time zone
  if (force.tz) {
    dataset.tz <- lubridate::tz(dataset[[Datetime.colname.defused]])
    States.dataset <- States.dataset |>
      dplyr::mutate(
        {{ start.colname }} := lubridate::force_tz({{ start.colname }}, tzone = dataset.tz),
        {{ end.colname }} := lubridate::force_tz({{ end.colname }}, tzone = dataset.tz)
      )
  }

  # Join clusters with original data
  dataset |>
    dplyr::left_join(
      States.dataset |>
        dplyr::select(-dplyr::any_of(leave.out)),
      by =
        dplyr::join_by(
          !!!groups,
          {{ Datetime.colname }} >= {{ start.colname}},
          {{ Datetime.colname }} <= {{ end.colname }}
        )
    ) |>
    dplyr::select(-{{ start.colname}}, -{{ end.colname }})
  
}


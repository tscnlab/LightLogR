#' Extract summaries of states
#'
#' Extracts a state from a dataset and provides their start and end times, as
#' well as duration and epoch. The state does not have to exist in the dataset,
#' but can be dynamically created. Extracted states can have group-dropping
#' disabled, meaning that summaries based on the extracted states show empty
#' groups as well.
#'
#' @inheritParams extract_clusters
#' @param State.colname The variable or condition to be evaluated for state
#'   exctration. Expects a symbol. If it is not part of the data, a
#'   `State.expression` is required.
#' @param State.expression If `State.colname` is not part of the `data`, this
#'   expression will be evaluated to generate the state. The result of this
#'   expression will be used for grouping, so it is recommended to be
#'   factor-like. If `State.colname` **is** part of the `data`, this argument will be ignored
#' @param epoch The epoch to use for the gapless sequence. Can be either a
#'   `lubridate::duration()` or a string. If it is a string, it needs to be
#'   either '"dominant.epoch"' (the default) for a guess based on the data or a
#'   valid `lubridate::duration()` string, e.g., `"1 day"` or `"10 sec"`.
#' @param group.by.state Logical. Should the output be automatically be grouped
#'   by the new state?
#'
#' @returns a dataframe with one row per state instance. Each row will consist
#'   of the original dataset grouping, the state column. A state.count column,
#'   start and end Datetimes, as well as a duration of the state
#' @export
#'
#' @examples
#' #summarizing states "photoperiod"
#' states <-
#' sample.data.environment |>
#'   add_photoperiod(c(48.52, 9.06)) |>
#'   extract_states(photoperiod.state)
#' states |> head(2)
#' states |> tail(2)
#' states |> summarize_numeric(c("state.count", "epoch"))
extract_states <- function(data,
                           State.colname,
                           State.expression = NULL,
                           Datetime.colname = Datetime,
                           handle.gaps = FALSE,
                           epoch = "dominant.epoch",
                           drop.empty.groups = TRUE,
                           group.by.state = TRUE
                           ) {
  # Convert variable expression to quosure
  
  if(colname.defused({{ State.colname }}) %in% names(data)) {
    State.expression <- rlang::enquo(State.colname)
  } else State.expression <- rlang::enquo(State.expression)
  
  #handle gaps
  if(handle.gaps) {
    data <- 
      data |> gap_handler()
  }
  
  #get the epochs based on the data
  groups <- dplyr::groups(data)
  
  #keep empty groups
  if(!drop.empty.groups) {
    data <-
      data |> dplyr::group_by(!!!groups, .drop = FALSE)
  }
  
  epochs <- epoch_list(data, Datetime.colname = {{ Datetime.colname }},
                       epoch = epoch)

  # Calculate lengths of times
  data <-
    data |>
    dplyr::mutate(
      {{ State.colname }} := !!State.expression,
      state.count := dplyr::consecutive_id({{ State.colname }}),
      epoch =  ifelse(
        epochs$dominant.epoch[epochs$group.indices == dplyr::cur_group_id()] |> length() == 0,
        NA,
        epochs$dominant.epoch[epochs$group.indices == dplyr::cur_group_id()]
    )) |> 
    dplyr::group_by({{ State.colname }}, state.count, .add = TRUE)
  
  #summarize the instances
  data <- 
  data |> 
    dplyr::summarize(
      epoch = dplyr::first(epoch),
      start = min({{ Datetime.colname }}) - 0.5*epoch,
      end = max({{ Datetime.colname }}) + 0.5*epoch,
      duration = lubridate::as.duration(end - start),
      .groups = "drop_last"
    ) |> 
    dplyr::mutate(state.count = paste({{ State.colname }}, 
                                      dplyr::consecutive_id(state.count)
                                      )
                  )
  
  if(!group.by.state) {
    data |> 
    dplyr::group_by(!!!groups) |> 
      dplyr::arrange(start, .by_group = TRUE)
  } else data
  
}


#' Add states to a dataset based on groups and start/end times
#'
#' [add_states()] brings states to a time series dataset. It uses the
#' `States.dataset` to add states to the `dataset`. The `States.dataset` must at
#' least contain the same variables as the `dataset` grouping, as well as a
#' start and end time. Beware if both datasets operate on different time zones
#' and consider to set `force.tz = TRUE`.
#'
#' Beware if columns in the `dataset` and `States.dataset` have the same name
#' (other then grouping variables). The underlying function,
#' [dplyr::left_join()] will mark the columns in the `dataset` with a suffix
#' `.x`, and in the `States.dataset` with a suffix `.y`.
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
#'   time. Need to be `POSIXct` and part of the `States.dataset`.
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
#'  gg_state(Daylight)

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
  
  # Check if start.colname is a POSIXct
  if (!lubridate::is.POSIXct(States.dataset[[start.colname.defused]])) {
    stop("start.colname must be a POSIXct")
  }
  
  # Check if end.colname is a POSIXct
  if (!lubridate::is.POSIXct(States.dataset[[end.colname.defused]])) {
    stop("end.colname must be a POSIXct")
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
  
  # If force.tz is TRUE, convert the start/end columns of the States.dataset to the same time zone
  if (force.tz) {
    dataset.tz <- lubridate::tz(dataset[[Datetime.colname.defused]])
    States.dataset <- States.dataset |>
      dplyr::mutate(
        {{ start.colname }} := lubridate::with_tz({{ start.colname }}, tzone = dataset.tz),
        {{ end.colname }} := lubridate::with_tz({{ end.colname }}, tzone = dataset.tz)
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
  
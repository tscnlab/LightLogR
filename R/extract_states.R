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

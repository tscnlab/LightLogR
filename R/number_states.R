#' Number non-consecutive state occurrences
#'
#' [number_states()] creates a new column in a dataset that takes a state column
#' and assigns a count value to each state, rising every time a state is
#' replaced by another state. E.g., a column with the states "day" and "night"
#' will produce a column indicating whether this is "day 1", "day 2", and so
#' forth, as will the "night" state with "night 1", "night 2", etc. Grouping
#' within the input dataset is respected, i.e., the count will reset for each
#' group.
#'
#' The state column is not limited to two states, but can have as many states as
#' needed. Also, it does not matter in which time frames these states change, so
#' they do not necessarily conform to a 24-hour day. `NA` values will be treated
#' as their own state.
#'
#' Gaps in the data can lead to non-sensible outcomes, e.g. if there is no
#' in-between state/observation between a day state at "18:00:00" and a day
#' state at "6:00:00" - this would be counted as `day 1` still. In these cases,
#' the [gap_handler()] function can be useful to a priori add observations.
#'
#' @param dataset A `data.frame` with a state column.
#' @param state.colname Column name that contains the state. Expects a `symbol`.
#'   Needs to be part of the `dataset`. Can be of any type, but `character` and
#'   `factor` make the most sense.
#' @param colname.extension The extension that is added to the state name to
#'   create the new column. Defaults to `".count"`.
#' @param use.original.state Logical, whether the original state should be part
#'   of the output column.
#'
#' @returns The input `dataset` with an additional column that counts the
#'   occurrences of each state. The new column will of type `character` if
#'   `use.original.state = TRUE` and `integer` otherwise.
#' @export
#'
#' @examples
#' dataset <- tibble::tibble(
#'  state =
#'  c("day", "day", "day", "night", "night", "day", "day", "night",
#'  "night", "night", "day", "night")
#'  )
#' number_states(dataset, state)
#' number_states(dataset, state, use.original.state = FALSE)
#'
#' #example with photoperiods, calculating the mean values for each day and night
#' coordinates <- c(48.52, 9.06)
#' sample.data.environment |>
#'   add_photoperiod(coordinates) |>
#'   number_states(photoperiod.state) |>
#'   dplyr::group_by(photoperiod.state.count, .add = TRUE) |>
#'   dplyr::summarize(mean_MEDI = mean(MEDI)) |>
#'   tail(13)
#' 
number_states <- function(dataset,
                          state.colname,
                          colname.extension = ".count",
                          use.original.state = TRUE) {
  
  # Initial Checks ----------------------------------------------------------
  
  state.colname.str <- 
    rlang::enexpr(state.colname) |>  rlang::as_string()
  
  stopifnot(
    "dataset is not a data.frame" = is.data.frame(dataset),
    "state.colname is not in the dataset" =
      state.colname.str %in% names(dataset),
    "colname.extension is not a character" = is.character(colname.extension),
    "colname.extension is not a scalar" = length(colname.extension) == 1
  )
  
  # Function ----------------------------------------------------------
  
  dataset |> 
    dplyr::mutate(
      temporary.counter = 
        dplyr::consecutive_id({{state.colname}}),
      temporary.state = {{state.colname}}
      ) |> 
    dplyr::group_by(temporary.state, .add = TRUE) |> 
    dplyr::mutate(
      temporary.counter =
        dplyr::consecutive_id(
          temporary.counter
          ),
      !!paste0(state.colname.str, colname.extension) := 
        if(use.original.state)
          paste({{state.colname}}, temporary.counter)
        else temporary.counter
      ) |>
    dplyr::ungroup(temporary.state) |>
    dplyr::select(-temporary.state, -temporary.counter)
    
  
}
#' Find and extract clusters from a dataset
#'
#' @description `extract_clusters()` searches for and summarizes clusters where
#'   data meets a certain condition. Clusters have a specified duration and can
#'   be interrupted while still counting as one cluster. The variable can either
#'   be a column in the dataset or an expression that gets evaluated in a
#'   [dplyr::mutate()] call.
#'
#'   Cluster start and end times are shifted by half of the epoch each. E.g., a
#'   state lasting for 4 measurement points will have a duration of 4
#'   measurement intervals, and a state only occuring once, of one interval.
#'   This deviates from simply using the time difference between the first and
#'   last occurance, which would be one epoch shorter (e.g., the start and end
#'   points for a state lasting a single point is identical, i.e., zero
#'   duration)
#'
#' **For correct cluster identification, there can be no gaps in the data!**
#'   Gaps can inadvertently be introduced to a gapless dataset through grouping.
#'   E.g., when grouping by photoperiod (day/night) within a participant, this
#'   introduces gaps between the individual days and nights that together form
#'   the group. To avoid this, either group by individual days and nights (e.g.,
#'   by using [number_states()] before grouping), which will make sure a cluster
#'   cannot extend beyond any grouping. Alternatively, you can set `handle.gaps
#'   = TRUE` (at computational cost).
#'
#' @param data A light logger dataset. Expects a dataframe.
#' @param Variable The variable or condition to be evaluated for clustering. Can
#'   be a column name or an expression.
#' @param Datetime.colname Column name that contains the datetime. Defaults to
#'   "Datetime" which is automatically correct for data imported with LightLogR.
#'   Expects a symbol.
#' @param cluster.duration The minimum or maximum duration of a cluster.
#'   Defaults to 30 minutes. Expects a lubridate duration object (or a numeric
#'   in seconds).
#' @param duration.type Type of the duration requirement for clusters. Either
#'   "min" (minimum duration) or "max" (maximum duration). Defaults to "min".
#' @param interruption.duration The duration of allowed interruptions within a
#'   cluster. Defaults to 0 (no interruptions allowed).
#' @param interruption.type Type of the interruption duration. Either "max"
#'   (maximum interruption) or "min" (minimum interruption). Defaults to "max".
#' @param cluster.colname Name of the column to use for the cluster
#'   identification. Defaults to "state.count". Expects a symbol.
#' @param return.only.clusters Whether to return only the identified clusters
#'   (TRUE) or also include non-clusters (FALSE). Defaults to TRUE.
#' @param handle.gaps Logical whether the data shall be treated with
#'   [gap_handler()]. Is set to `FALSE` by default, due to computational costs.
#'
#' @return For `extract_clusters()` a dataframe containing the identified
#'   clusters or all time periods, depending on `return.only.clusters`.
#'
#' @family Clusters
#'
#' @export
#'
#' @examples
#'
#' dataset <-
#' sample.data.environment |>
#' dplyr::filter(Id == "Participant")
#'
#' # Extract clusters with minimum duration of 1 hour and interruptions of up to 5 minutes
#' dataset |>
#'  extract_clusters(
#'   MEDI > 1000,
#'   cluster.duration = "1 hour",
#'   interruption.duration = "5 mins"
#' )
extract_clusters <- function(
    data,
    Variable,
    Datetime.colname = Datetime,
    cluster.duration = "30 mins",
    duration.type = c("min", "max"),
    interruption.duration = 0,
    interruption.type = c("max", "min"),
    cluster.colname = state.count,
    return.only.clusters = TRUE,
    handle.gaps = FALSE) {
  
  # Argument validation
  duration.type <- match.arg(duration.type)
  interruption.type <- match.arg(interruption.type)
  
  #convert duration inputs to durations
  cluster.duration <- cluster.duration |> lubridate::as.duration()
  interruption.duration <- interruption.duration |> lubridate::as.duration()
  
  # Convert variable expression to quosure
  Variable <- rlang::enexpr(Variable)
  
  # Prepare comparison functions
  duration.type <- paste0("p", duration.type)
  duration.type <- rlang::sym(duration.type)
  
  interruption.type <- paste0("p", interruption.type)
  interruption.type <- rlang::sym(interruption.type)
  
  #handle gaps
  if(handle.gaps) {
    data <- 
      data |> gap_handler()
  }
  
  # Datetime
  dominant.epochs <- dominant_epoch(data, {{ Datetime.colname }})
  data <- 
    data |> 
    dplyr::left_join(dominant.epochs, by = dplyr::group_vars(data))
  
  # Calculate lengths of times
  data1 <-
    data |>
    dplyr::mutate(
      .variable = !!Variable,
      {{ cluster.colname }} := dplyr::consecutive_id(.variable)
    ) |>
    dplyr::group_by({{ cluster.colname }}, .add = TRUE) |>
    dplyr::summarize(
      type = dplyr::first(.variable),
      dominant.epoch = dplyr::first(dominant.epoch),
      start = min({{ Datetime.colname }}) - 0.5*dominant.epoch,
      end = max({{ Datetime.colname }}) + 0.5*dominant.epoch,
      duration = lubridate::as.duration(end - start),
      .groups = "drop_last"
    )
  
  # Identify clusters and handle interruptions
  data2 <-
    data1 |>
    dplyr::mutate(
      is.cluster = (!!duration.type)(duration, cluster.duration) == cluster.duration,
      type = dplyr::if_else(
        ((!!interruption.type)(duration, interruption.duration) == interruption.duration) &
          (!type | is.na(type)) &
          (interruption.duration != 0),
        TRUE,
        type
      ),
      {{ cluster.colname }} := dplyr::consecutive_id(type)
    )
  
  # Summarize clusters
  data3 <-
    data2 |>
    dplyr::group_by({{ cluster.colname }}, .add = TRUE) |>
    dplyr::summarize(
      start = min(start),
      end = max(end),
      epoch = mean(dominant.epoch) |> lubridate::as.duration(),
      duration = lubridate::as.duration(end - start),
      is.cluster = all((!!duration.type)(duration, cluster.duration) == cluster.duration,
                 type),
      .groups = "drop_last"
    )
  
  if(!any(data3$is.cluster)){
    message(paste0("No clusters of condition: ", deparse(Variable)," found"))
  }
  
  # Return either all data or only clusters
  if(!return.only.clusters){
    data4 <- 
    data3 |> 
      dplyr::mutate(
        {{ cluster.colname }} := dplyr::consecutive_id(is.cluster)
      ) |> 
      dplyr::group_by({{ cluster.colname }}, .add = TRUE) |>
      dplyr::summarize(
        start = min(start),
        end = max(end),
        epoch = mean(epoch) |> lubridate::as.duration(),
        duration = lubridate::as.duration(end - start),
        is.cluster = dplyr::first(is.cluster),
        .groups = "drop_last"
      ) |> 
      dplyr::mutate(
        {{ cluster.colname }} := dplyr::case_when(is.cluster ~ cumsum(is.cluster), .default = NA) |> as.character()
      )
    return(data4)
  }

  data3 |>
    dplyr::filter(is.cluster) |>
    dplyr::mutate({{ cluster.colname }} := dplyr::consecutive_id({{ cluster.colname }}) |> as.character()) |>
    dplyr::select(-is.cluster)
}

#' Add Clusters to a Dataset
#'
#' @description `add_clusters()` identifies clusters and adds them back into the
#' dataset through a rolling join. This is a convenience function built on [extract_clusters()].
#'
#' @rdname extract_clusters
#'
#' @return For `add_clusters()` a dataframe containing the original data with an additional column
#'   for cluster identification.
#' @export
#' 
#' @family Clusters
#'
#' @examples
#' 
#' # Add clusters to a dataset where lux values are above 1000 for at least 30 minutes
#' dataset_with_clusters <- 
#' sample.data.environment %>% add_clusters(MEDI > 1000)
#'
#' dataset_with_clusters |> dplyr::count(state)
#'
add_clusters <- function(
    data,
    Variable,
    Datetime.colname = Datetime,
    cluster.duration = "30 mins",
    duration.type = c("min", "max"),
    interruption.duration = 0,
    interruption.type = c("max", "min"),
    cluster.colname = state,
    handle.gaps = FALSE) {
  
  # Extract clusters
  episodes <- extract_clusters(
    data = data,
    Variable = {{ Variable }},
    Datetime.colname = {{ Datetime.colname }},
    cluster.duration = cluster.duration,
    duration.type = duration.type,
    interruption.duration = interruption.duration,
    interruption.type = interruption.type,
    cluster.colname = {{ cluster.colname }},
    handle.gaps = handle.gaps
  )
  
  if(nrow(episodes) == 0) {
    message(paste0("No clusters of condition: ", deparse(Variable)," found"))
    return(data)
  }
  
  groups <- dplyr::groups(data)
  
  # Join clusters with original data
  data |>
    dplyr::left_join(
      episodes |>
        dplyr::select(-duration, -epoch),
      by =
      dplyr::join_by(
        !!!groups,
        {{ Datetime.colname }} >= start,
        {{ Datetime.colname }} <= end
      )
    ) |>
    dplyr::select(-start, -end)
}


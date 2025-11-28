#' Add metrics to extracted sSummary
#'
#' This helper function adds metric values to an extract, like from
#' [extract_states()] or [extract_clusters()]. E.g., the average value of a
#' variable during a cluster or state instance might be of interest. The metrics
#' must be specified by the user using the `...` argument.
#'
#' The original `data` does not have to have the cluster/state information, but
#' it will be computationally faster if it does.
#'
#' @param extracted_data A dataframe containing cluster or state summaries,
#'   typically from `extract_clusters()` or `extract_states()`.
#' @param data The original dataset that produced `extracted_data`.
#' @param identifying.colname Name of the column in `extracted_data` that
#'   uniquely identifies each row (in addition to the groups. Expects a symbol.
#'   Defaults to `state.count`
#' @param Datetime.colname Column name that contains the datetime in `data`.
#'   Defaults to "Datetime" which is automatically correct for data imported
#'   with LightLogR. Expects a symbol. This argument is only necessary if `data`
#'   does not contain the `cluster.colname`.
#' @param ... Arguments specifying the metrics to add summary. For example:
#'   `"mean_lux" = mean(lux)`.
#'
#' @return A dataframe containing the extracted data with added metrics.
#'
#' @export
#'
#' @examples
#' # Extract clusters and add mean MEDI value
#' sample.data.environment |>
#' filter_Date(length = "2 days") |> 
#' extract_clusters(MEDI > 1000) |>
#' extract_metric(
#'   sample.data.environment,
#'   "mean_medi" = mean(MEDI, na.rm = TRUE)
#' ) |>
#' dplyr::select(Id, state.count, duration, mean_medi)
#'
#' # Extract states and add mean MEDI value
#' dataset <-
#' sample.data.environment |>
#' filter_Date(length = "2 days") |> 
#'  add_photoperiod(c(48.5, 9))
#'
#' dataset |>
#'   extract_states(photoperiod.state) |>
#'   extract_metric(dataset, mean_lux = mean(MEDI)) |>
#'   dplyr::select(state.count, duration, mean_lux)

extract_metric <- function(
    extracted_data,
    data,
    identifying.colname = state.count,
    Datetime.colname = Datetime,
    ...) {
  
  cc.quo <- rlang::enexpr(identifying.colname)
  cc.name <- rlang::as_name(cc.quo)
  
  groups <- dplyr::group_vars(data)
  
  #if identifying.colname ist not part of the dataset, it will be added from the extracted_data
  if (!cc.name %in% names(data)) {
    data <- 
      data |>
      dplyr::left_join(
        extracted_data |>
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
  
  original_data <- 
    data |>
    dplyr::filter(!is.na({{ identifying.colname }})) |>
    dplyr::group_by({{ identifying.colname }}, .add = TRUE)
  
  extracted_data |> 
    dplyr::left_join(
      original_data |>
        dplyr::summarize(
          ...,
          .groups = "drop_last"
        ), 
      by = dplyr::group_vars(original_data)
    )
  
}

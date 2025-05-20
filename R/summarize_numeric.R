#' Summarize numeric columns in dataframes to means
#'
#' @description 
#' This simple helper function was created to summarize episodes of gaps, clusters, or states, focusing on numeric variables.
#' It calculates mean values for all numeric columns and handles Duration objects appropriately.
#'
#' @param data A dataframe containing numeric data, typically from [extract_clusters()] or [extract_gaps()].
#' @param remove This/These columns will be removed from the summary.
#' @param prefix A prefix to add to the column names of summarized metrics. Defaults to "mean_".
#' @param na.rm Whether to remove NA values when calculating means. Defaults to TRUE.
#' @param add.total.duration Logical, whether the total duration for a given group should be calculated. Only relevant if a column `duration` is part of the input data.
#' @param durations.dec Numeric of number of decimals for the mean calculation of durations. Defaults to 0.
#'
#' @return A dataframe containing the summarized metrics.
#' 
#' @export
#'
#' @examples
#' # Extract clusters and summarize them
#' dataset <- 
#' sample.data.environment %>% 
#' aggregate_Datetime(unit = "15 mins") |> 
#' extract_clusters(MEDI > 1000)
#' 
#' #input to summarize_numeric
#' dataset
#' #output of summarize_numeric (removing state.count and epoch from the summary)
#' dataset |> summarize_numeric(c(state.count, epoch))
summarize_numeric <- function(
    data,
    remove = NULL,
    prefix = "mean_",
    na.rm = TRUE,
    add.total.duration = TRUE,
    durations.dec = 0) {
  
  total <- list(NULL)
  if(add.total.duration & "duration" %in% names(data)) {
    total <- list(
      total_duration = rlang::expr(sum(duration, na.rm = na.rm) |> lubridate::as.duration())
    )
    
  }
  
  data <- 
  data |>
    dplyr::select(-{{ remove}}) |> 
    dplyr::summarize(
      dplyr::across(
        dplyr::where(is.numeric),
        \(x) if(inherits(x, "Duration")) {
          lubridate::duration(mean(x, na.rm = na.rm)) |>
            round(durations.dec)
        } else {
          mean(x, na.rm = na.rm)
        },
        .names = "{prefix}{.col}"
      ),
      "episodes" = dplyr::n(),
      !!!total,
      .groups = "drop_last"
    )
  
  data
  # potential_duration <- rlang::expr(!!rlang::sym(paste0(prefix, "duration")))
  # 
  # if(add.total.duration & (paste0(prefix, "duration") %in% names(data))) {
  #   data |>
  #     dplyr::mutate(
  #       total_duration = {{ potential_duration }}*episodes
  #     )
  # } else data
  
}

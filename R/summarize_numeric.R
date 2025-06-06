#' Summarize numeric columns in dataframes to means
#'
#' @description This simple helper function was created to summarize episodes of
#'   gaps, clusters, or states, focusing on numeric variables. It calculates
#'   mean values for all numeric columns and handles Duration objects
#'   appropriately.
#'
#'   Despite its name, the function actually summarizes all double columns,
#'   which is more inclusive compared to just numeric columns.
#'
#' @inheritParams mean_daily
#' @param data A dataframe containing numeric data, typically from
#'   [extract_clusters()] or [extract_gaps()].
#' @param remove Character vector of columns removed from the summary.
#' @param prefix A prefix to add to the column names of summarized metrics.
#'   Defaults to "mean_".
#' @param na.rm Whether to remove NA values when calculating means. Defaults to
#'   TRUE.
#' @param add.total.duration Logical, whether the total duration for a given
#'   group should be calculated. Only relevant if a column `duration` is part of
#'   the input data.
#' @param durations.dec Numeric of number of decimals for the mean calculation
#'   of durations and times. Defaults to 0.
#' @param complete.groups.on Column name that, together with grouping variables,
#'   can be used to provide a complete set. For example, with
#'   [extract_clusters()], some days might not have clusters. They do not show
#'   up in the summary output then. If it is important however, to consider that
#'   there are zero instances, one could extract the complete set of clusters
#'   and non-clusters, and then set `is.cluster` in this argument, which would
#'   then show zero clusters for those days.
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
#' dataset |> utils::head()
#' #output of summarize_numeric (removing state.count and epoch from the summary)
#' dataset |> summarize_numeric(c("state.count", "epoch"))
summarize_numeric <- function(
    data,
    remove = NULL,
    prefix = "mean_",
    na.rm = TRUE,
    complete.groups.on = NULL,
    add.total.duration = TRUE,
    durations.dec = 0,
    Datetime2Time = TRUE) {
  
  total <- list(NULL)
  if(add.total.duration & "duration" %in% names(data)) {
    total <- list(
      total_duration = rlang::expr(sum(duration, na.rm = na.rm) |> lubridate::as.duration())
    )
    
  }
  
  complete_expr <- rlang::enexpr(complete.groups.on)
  
  if(Datetime2Time) {
    data <-
      data |>
      Datetime2Time(silent = TRUE)
  }
  
  if(!is.null(complete_expr)) {
    groups <- dplyr::groups(data)
    data <- 
      data |> 
      dplyr::group_by({{ complete.groups.on }}, .add = TRUE)
  }
  
  groups <- dplyr::group_vars(data)
  
  data_total <- 
    data |>
    dplyr::select(-dplyr::any_of(remove)) |> 
    dplyr::summarize(
      !!!total,
      "episodes" = dplyr::n(),
      .groups = "drop_last"
    )
  
  data <- 
  data |>
    dplyr::select(-dplyr::any_of(remove)) |> 
    dplyr::summarize(
      dplyr::across(
        dplyr::where(\(x) is.double(x) | is.numeric(x)),
        \(x) if(inherits(x, "Duration")) {
          lubridate::duration(mean(x, na.rm = na.rm)) |>
            round(durations.dec)
        } else if (inherits(x, "hms")) {
          hms::as_hms(mean(x, na.rm = na.rm) |> round(durations.dec)) 
        } else {
          mean(x, na.rm = na.rm)
        },
        .names = "{prefix}{.col}"
      ),
      .groups = "drop_last"
    )
  
  if("episodes" %in% names(data)) data <-  data |> dplyr::select(-episodes)
  
  data <- dplyr::left_join(data, data_total, by= groups)
  
  if(!is.null(complete_expr)) {
      data |> 
        dplyr::ungroup() |> 
        tidyr::complete(!!!groups, {{ complete.groups.on }}, 
                        fill = list(episodes = 0)) |> 
        dplyr::group_by(!!!groups)
  } else data
  
}

#' @rdname summarize_numeric
#' @export
summarise_numeric <- summarize_numeric
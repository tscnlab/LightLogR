#' Normalize counts between sensor outputs
#'
#' This is a niche helper function to normalize counts. Some sensors provide raw
#' counts and gain levels as part of their output. In some cases it is desirable
#' to compare counts between sensors, e.g., to gauge daylight outside by
#' comparing UV counts to photopic counts (a high ratio of UV/Pho indicates
#' outside daylight). Or to gauge daylight inside by comparing IR counts to
#' photopic counts (a high ratio of IR/Pho with a low ratio of UV/Pho indicates
#' daylight in the context of LED or fluorescent lighting). The user can provide
#' their own gain ratiotable, or use a table provided for a sensor in the
#' `gain.ratio.table` dataset from `LightLogR`.
#'
#'
#' @param dataset a `data.table` containing gain and count columns.
#' @param gain.ratio.table a two-column tibble containing `gain` and
#'   `gain.ratio` information. Can be provided by the user or use the
#'   `gain.ratio.table` dataset.
#' @param gain.columns a `character` vector of columns in the `dataset`
#'   containing a gain setting. Columns must not repeat.
#' @param count.columns a `character` vector of columns in the `dataset`
#'   containing raw count data. Must be of the same length as `gain.columns`,
#'   and the order must conform to the order in `gain.columns`.
#'
#' @returns an extended dataset with new columns containing normalized counts
#' @family Spectrum
#' 
#' @export
#'
#' @examples
#' example.table <-
#' tibble::tibble(
#' uvGain = c(4096, 1024, 2),
#' visGain = c(4096, 4096, 4096),
#' irGain = c(2,2,2),
#' uvValue = c(692, 709, 658),
#' visValue = c(128369, 129657, 128609),
#' irValue = c(122193, 127113, 124837))
#'
#' gain.columns = c("uvGain", "visGain", "irGain")
#' count.columns = c("uvValue", "visValue", "irValue")
#'
#' example.table |>
#' normalize_counts(gain.columns, count.columns, gain.ratio.tables$TSL2585)
normalize_counts <- function(dataset, 
                             gain.columns,
                             count.columns, 
                             gain.ratio.table) {
  

  # Initial Checks ----------------------------------------------------------
  
  stopifnot(
    "gain ratio table is not a dataframe" = is.data.frame(gain.ratio.table),
    "gain ratio table does not have 2 columns" = length(gain.ratio.table) == 2,
    "gain ratio table does not contain a `gain` and `gain.ratio` column" =
      names(gain.ratio.table) %in% c("gain", "gain.ratio")
  )
  
  stopifnot(
    "gain.columns and count.columns are not of the same length" =
      length(gain.columns) == length(count.columns)
  )
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "gain.columns must be part of the dataframe" = gain.columns %in% names(dataset),
    "count.columns must be part of the dataframe" = count.columns %in% names(dataset)
  )
  

  # Function definition -----------------------------------------------------

  purrr::reduce2(
    gain.columns, 
    count.columns, 
    \(dataset, gain.column, count.column) {
      dataset |> 
        dplyr::left_join(gain.ratio.table,
                         by = dplyr::join_by(!!gain.column == "gain")
                           ) |> 
        dplyr::mutate(
          gain.ratio = .data[[count.column]] / gain.ratio
          ) |> 
        dplyr::rename(!!paste0(count.column, ".normalized") := gain.ratio)
      },
    .init = dataset
    )
  
}
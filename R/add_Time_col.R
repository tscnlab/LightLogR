
#' Create a Time-of-Day column in the dataset
#'
#' @inheritParams cut_Datetime
#' @param Time.colname Name of the newly created column. Expects a `symbol`. The
#'   default(`Time`) works well with other functions in [LightLogR].
#'   Will overwrite existing columns of identical name.
#' @param output.dataset should the output be a `data.frame` (Default `TRUE`) or
#'   a vector with `hms` (`FALSE`) times? Expects a `logical` scalar.
#'
#' @return a `data.frame` object identical to `dataset` but with the added
#'   column of Time-of-Day data, or a `vector` with the Time-of-Day-data
#' @export
#' @importFrom rlang :=
#' @examples
#' sample.data.environment %>% add_Time_col()
#' 
add_Time_col <- function(dataset, 
                             Datetime.colname = Datetime,
                             Time.colname = Time,
                             output.dataset = TRUE) {
  
  # Initial Checks ----------------------------------------------------------
  Datetime.colname.defused <- colname.defused({{ Datetime.colname }})
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" = 
      Datetime.colname.defused %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(dataset[[Datetime.colname.defused]]),
    "output.dataset must be a logical" = is.logical(output.dataset)
    )
  
  # Manipulation ----------------------------------------------------------
  dataset <-
    dataset %>%
    dplyr::mutate(
      {{ Time.colname }} := {{ Datetime.colname }} %>% hms::as_hms()
    )  
  
  # Return ----------------------------------------------------------
  if(output.dataset) dataset else dataset[[colname.defused({{ Time.colname }})]]

}


#' create_Timedata
#'
#' @param ... Input arguments to [add_Time_col()]
#'
#' @returns a `data.frame` object identical to `dataset` but with the added
#'   column of Time-of-Day data, or a `vector` with the Time-of-Day-data
#' @export
#'
#' @examples
#' sample.data.environment %>% create_Timedata()
#' 
create_Timedata <- function(...) {
  .Deprecated("add_Time_col", msg = "`create_Timedata()` is deprecated as of LightLogR 0.9.1. Please use `add_Time_col()` instead. ")
  add_Time_col(...)
}
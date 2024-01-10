
#' Create a Time-of-Day column in the dataset
#'
#' @inheritParams cut_Datetime
#' @param Time.data Name of the newly created column. Expects a `symbol`. The
#'   default(`Time.data`) works well with other functions in [LightLogR].
#' @param output.dataset should the output be a `data.frame` (Default `TRUE`) or
#'   a vector with `hms` (`FALSE`) times? Expects a `logical` scalar.
#'
#' @return a `data.frame` object identical to `dataset` but with the added
#'   column of Time-of-Day data, or a `vector` with the Time-of-Day-data
#' @export
#' @importFrom rlang :=
#' @examples
#' sample.data.environment %>% create_Timedata()
create_Timedata <- function(dataset, 
                             Datetime.colname = Datetime,
                             Time.data = Time.data,
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
      {{ Time.data }} := {{ Datetime.colname }} %>% hms::as_hms()
    )  
  
  # Return ----------------------------------------------------------
  if(output.dataset) dataset else dataset[[colname.defused({{ Time.data }})]]

}
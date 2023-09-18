
#' Create a Time-of-Day column in the dataset
#'
#' @inheritParams cut_Datetime
#' @param Time.data Name of the newly created column. Expects a `symbol`. The
#'   default(`Time.data`) works well with other functions in [LightLogR].
#'
#' @return a `data.frame` object identical to `dataset` but with the added
#'   column of Time-of-Day data.
#' @export
#' @importFrom rlang :=
#' @examples
#' sample.data.environment %>% create_Time.data
create_Time.data <- function(dataset, 
                             Datetime.colname = Datetime,
                             Time.data = Time.data) {
  
  Datetime.colname.defused <- 
    rlang::enexpr(Datetime.colname) %>% rlang::as_string()
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" = 
      Datetime.colname.defused %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(dataset[[Datetime.colname.defused]])
    )
  
  dataset <-
    dataset %>%
    dplyr::mutate(
      {{ Time.data }} := {{ Datetime.colname }} %>% hms::as_hms()
    )  
  
  dataset
}
#' Filter Times in a dataset.
#'
#' @inheritParams cut_Datetime
#' @param start,end,length a `character` scalar in
#'   the form of `"hh-mm-ss"` giving the respective start, end, or length
#'   for the filtered dataframe.
#' * If one or both of start/end are not provided, the times will be taken from the respective extreme values of the `dataset`.
#' * If `length` is provided and one of start/end is not, the other will be calculated based on the given value.
#' * If `length` is provided and both of start/end are not, the time from the
#'   respective start is taken.
#'
#' @return  a `data.frame` object identical to `dataset` but with only the
#'   specified Times.
#' @export
#'
#' @examples
#' sample.data.environment %>% 
#' filter_Time(start = "4:00:34", length = "12:00:00") %>% 
#' dplyr::pull(Time.data) %>% range() %>% hms::as_hms()
#' @family filter

filter_Time <- function(dataset, 
                            Datetime.colname = Datetime, 
                            start = NULL, 
                            end = NULL, 
                            length = NULL
                            ) {
  
  # Initial Checks ----------------------------------------------------------
  
  Datetime.colname.defused <- 
    rlang::enexpr(Datetime.colname) %>% rlang::as_string()
  x <- rlang::enexpr(Datetime.colname)
  
  if(!is.null(length)){
    test <- 
      stringr::str_detect(length, 
                 pattern = "^([01]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$")
    stopifnot("length needs to be in the format 'hh:mm:ss'" = test)
  }
 
  if(!is.null(start)) {
    test <- 
      stringr::str_detect(start, 
                 pattern = "^([01]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$")
    stopifnot("start needs to be in the format 'hh:mm:ss'" = test)
  }
  if(!is.null(end)) {
    test <- 
      stringr::str_detect(end, 
                 pattern = "^([01]?[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$")
    stopifnot("end needs to be in the format 'hh:mm:ss'" = test)
  }
  
  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" = 
      Datetime.colname.defused %in% names(dataset),
    "Datetime.colname must be a Datetime" = 
      lubridate::is.POSIXct(dataset[[Datetime.colname.defused]]),
    "At least one parameter from `start`, `end` or `length` must be specified" =
      !all(is.null(start), is.null(end), is.null(length))
  )

  # Manipulation ----------------------------------------------------------
  
  #create a time-of-day column
  dataset <- dataset %>% create_Time.data(!!x)
  
  #calculate starting time if length and end are given
  if(is.null(start) & !is.null(length) & !is.null(end)) {
    start <- hms::as_hms(end) - hms::as_hms(length)
  }

  #calculate starting time if NULL
  if(is.null(start)) {
    start <- dataset$Time.data %>% min()
  }

  #calculate end time if length is given
  if(is.null(end) & !is.null(length)) {
    end <- hms::as_hms(start) + hms::as_hms(length)
  }

  #calculate end time if NULL
  if(is.null(end)) {
    end <- dataset$Time.data %>% max()
  }

  # filter start
  dataset <-
    dataset %>%
      dplyr::filter(
      Time.data >= if(hms::is_hms(start)) start else hms::as_hms(start),
      Time.data <= if(hms::is_hms(end)) end else hms::as_hms(end)
    )
  
  # Return --------------------------------------------------------------
  dataset
}
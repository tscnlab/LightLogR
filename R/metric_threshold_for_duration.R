#' Find threshold for given duration
#'
#' This function finds the threshold for which light levels are above/below for
#' a given duration. This function can be considered as the inverse of
#' \code{\link{duration_above_threshold}}.
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Time.vector Vector containing the time data. Can be \link[base]{POSIXct}, 
#'    \link[hms]{hms}, \link[lubridate]{duration}, or \link[base]{difftime}.
#' @param duration The duration for which the threshold should be found. Can be either a
#'  \link[lubridate]{duration} or a string. If it is a string, it needs to be a valid
#'  \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`.
#' @param comparison String specifying whether light levels above or below the threshold 
#'    should be considered. Can be either `"above"` (the default) or `"below"`.
#' @param epoch The epoch at which the data was sampled. Can be either a
#'  \link[lubridate]{duration} or a string. If it is a string, it needs to be
#'  either `"dominant.epoch"` (the default) for a guess based on the data, or a valid
#'  \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`.
#' @param na.rm Logical. Should missing values (NA) be removed for the calculation?
#'    Defaults to `FALSE`.
#' @param as.df Logical. Should a data frame with be returned? If `TRUE`, a data
#'    frame with a single column named `threshold_{comparison}_for_{duration}` will be returned.
#'    Defaults to `FALSE`.
#'
#' @return Single numeric value or single column data frame. 
#' 
#' @export
#' 
#' @family metrics
#'
#' @examples
#' N <- 60
#' # Dataset with 30 min < 250lx and 30min > 250lx
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", N),
#'     Datetime = lubridate::as_datetime(0) + lubridate::minutes(1:N),
#'     MEDI = sample(c(sample(1:249, N / 2, replace = TRUE), 
#'                     sample(250:1000, N / 2, replace = TRUE))),
#'   )
#' 
#' dataset1 %>%
#'   dplyr::reframe("Threshold above which for 30 mins" = 
#'                    threshold_for_duration(MEDI, Datetime, duration = "30 mins"))
#' 
#' dataset1 %>%
#'   dplyr::reframe("Threshold below which for 30 mins" = 
#'                    threshold_for_duration(MEDI, Datetime, duration = "30 mins",
#'                                           comparison = "below"))
#' 
#' dataset1 %>%
#'   dplyr::reframe(threshold_for_duration(MEDI, Datetime, duration = "30 mins",
#'                                         as.df = TRUE))
#' 
threshold_for_duration <- function(Light.vector,
                                   Time.vector,
                                   duration,
                                   comparison = c("above", "below"),
                                   epoch = "dominant.epoch",
                                   na.rm = FALSE,
                                   as.df = FALSE) {
  
  
  # Match input arguments
  comparison <- match.arg(comparison)
  
  # Perform argument checks
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`Time.vector` must be POSIXct, hms, duration, or difftime!" =
      lubridate::is.POSIXct(Time.vector) | hms::is_hms(Time.vector) | 
      lubridate::is.duration(Time.vector) | lubridate::is.difftime(Time.vector),
    "`Light.vector` and `Time.vector` must be same length!" = 
      length(Light.vector) == length(Time.vector),
    "`duration` must either be duration or a string!" = 
      lubridate::is.duration(duration) | is.character(duration),
    "`epoch` must either be a duration or a string" =
      lubridate::is.duration(epoch) | is.character(epoch),
    "`as.df` must be logical!" = is.logical(as.df)
  )
  
  # Get the epochs based on the data
  if (is.character(epoch) && epoch == "dominant.epoch") {
    epoch <- count_difftime(tibble::tibble(Datetime = Time.vector))$difftime[1]
  }
  # If the user specified an epoch, use that instead
  else {
    epoch <- lubridate::as.duration(epoch)
  }
  
  # Duration parameter as duration object
  duration <- lubridate::as.duration(duration)
  
  if(na.rm){
    Light.vector = Light.vector[!is.na(Light.vector)]
  }
  
  # Find threshold for given duration
  idx = floor(duration / epoch)
  sorted <- sort(Light.vector, decreasing = (comparison == "above"))
  threshold <- sorted[idx]
  
  # Return NA if missing values present
  if(any(is.na(Light.vector))){
    threshold <- as.double(NA)
  }
  
  # Return data frame or numeric value
  if (as.df) {
    duration <- stringr::str_extract(as.character(duration), "~.*") %>% 
      stringr::str_remove("~") %>% stringr::str_remove("\\)") %>% stringr::str_replace_all(" ", "_")
    return(tibble::tibble("threshold_{comparison}_for_{duration}" := threshold))
  } else {
    return(threshold)
  }
}

#' Length of longest continuous period above/below threshold
#' 
#' This function finds the length of the longest continous period above/below 
#' a specified threshold light level or within a specified range of light levels.
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Time.vector Vector containing the time data. Can be \link[base]{POSIXct}, 
#'    \link[hms]{hms}, \link[lubridate]{duration}, or \link[base]{difftime}.
#' @param comparison String specifying whether the period of light levels above or 
#'    below threshold should be calculated. Can be either `"above"` (the default) 
#'    or `"below"`. If two values are provided for `threshold`, this argument will be ignored.
#' @param threshold Single numeric value or two numeric values specifying the
#'    threshold light level(s) to compare with. If a vector with two values is provided,
#'    the period of light levels within the two thresholds will be calculated.
#' @param epoch The epoch at which the data was sampled. Can be either a
#'  \link[lubridate]{duration} or a string. If it is a string, it needs to be
#'  either `"dominant.epoch"` (the default) for a guess based on the data, or a valid
#'  \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`.
#' @param loop Logical. Should the data be looped? Defaults to `FALSE`.
#' @param na.replace Logical. Should missing values (NA) be replaced 
#'    for the calculation? If `TRUE` missing values will not be removed but will 
#'    result in `FALSE` when comparing `Light.vector` with `threshold`. 
#'    Defaults to `FALSE`.
#' @param na.rm Logical. Should missing values (NA) be removed for the calculation?
#'    If `TRUE`, this argument will override `na.replace`. Defaults to `FALSE`.  
#' @param as.df Logical. Should a data frame be returned? If `TRUE`, a data
#'    frame with a single column named `period_{comparison}_{threshold}` will be returned.
#'    Defaults to `FALSE`.
#'
#' @return A duration object (see \code{\link[lubridate]{duration}}) as single value,
#'    or single column data frame.
#'    
#' @export
#' 
#' @family metrics
#'
#' @examples
#' 
#' N <- 60
#' # Dataset with continous period of >250lx for 35min
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", N),
#'     Datetime = lubridate::as_datetime(0) + lubridate::minutes(1:N),
#'     MEDI = c(sample(1:249, N-35, replace = TRUE), 
#'              sample(250:1000, 35, replace = TRUE))
#'   )
#' 
#' dataset1 %>%
#'   dplyr::reframe("Period >250lx" = period_above_threshold(MEDI, Datetime, threshold = 250))
#' 
#' dataset1 %>%
#'   dplyr::reframe("Period <250lx" = period_above_threshold(MEDI, Datetime, "below", threshold = 250))
#' 
#' # Dataset with continous period of 100-250lx for 20min
#' dataset2 <-
#'   tibble::tibble(
#'     Id = rep("B", N),
#'     Datetime = lubridate::as_datetime(0) + lubridate::minutes(1:N),
#'     MEDI = c(sample(c(1:99, 251-1000), N-20, replace = TRUE), 
#'              sample(100:250, 20, replace = TRUE)),
#'   )
#' dataset2 %>%
#'   dplyr::reframe("Period 250lx" = period_above_threshold(MEDI, Datetime, threshold = c(100,250)))
#' 
#' # Return data frame
#' dataset1 %>%
#'   dplyr::reframe(period_above_threshold(MEDI, Datetime, threshold = 250, as.df = TRUE))
#' 
period_above_threshold <- function(Light.vector,
                                   Time.vector,
                                   comparison = c("above", "below"),
                                   threshold,
                                   epoch = "dominant.epoch",
                                   loop = FALSE,
                                   na.replace = FALSE,
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
    "`threshold` must be numeric!" = is.numeric(threshold),
    "`threshold` must be either one or two values!" = length(threshold) %in% c(1, 2),
    "`epoch` must either be a duration or a string" =
      lubridate::is.duration(epoch) | is.character(epoch),
    "`loop` must be logical!" = is.logical(loop),
    "`na.rm` must be logical!" = is.logical(na.rm),
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
  
  # Loop data
  if (loop) {
    Light.vector <- c(Light.vector, Light.vector)
  }
  
  # Remove NAs
  if(na.rm){
    Light.vector <- Light.vector[!is.na(Light.vector)]
  }
  
  # Find longest period
  longest_period <- function(x){
    z <- c(x, 0)
    z <- (cumsum(z) * c(diff(z) < 0, 0))
    max(diff(c(0, 0, z[z != 0])))
  }
  out <- compare_threshold(Light.vector, threshold, comparison, na.replace) %>% 
    longest_period()
  
  # As duration object
  out <- lubridate::as.duration(out * epoch)
  
  # Return data frame or numeric value
  if (as.df) {
    if(length(threshold) == 2){
      comparison <- "within"
    }
    threshold <- stringr::str_flatten(sort(threshold), collapse = "-")
    return(tibble::tibble("period_{comparison}_{threshold}" := out))
  } else {
    return(out)
  }
}

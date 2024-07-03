#' Midpoint of cumulative light exposure.
#'
#' This function calculates the timing corresponding to half of the cumulative
#' light exposure within the given time series.
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Time.vector Vector containing the time data. Can be \link[base]{POSIXct}, \link[hms]{hms}, 
#'    \link[lubridate]{duration}, or \link[base]{difftime}.
#' @param na.rm Logical. Should missing values be removed for the calculation? If `TRUE`,
#'    missing values will be replaced by zero. Defaults to `FALSE`. 
#' @param as.df Logical. Should the output be returned as a data frame? If `TRUE`, a data
#'    frame with a single column named `midpointCE` will be returned.
#'    Defaults to `FALSE`.
#'
#' @return Single column data frame or vector.
#' 
#' @export
#' 
#' @family metrics
#'
#' @references 
#'   Shochat, T., Santhi, N., Herer, P., Flavell, S. A., Skeldon, A. C.,
#'   & Dijk, D.-J. (2019). Sleep Timing in Late Autumn and Late Spring Associates
#'   With Light Exposure Rather Than Sun Time in College Students.
#'   \emph{Frontiers in Neuroscience}, 13. \doi{10.3389/fnins.2019.00882}
#'   
#'   Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
#'   Quantification metrics. \emph{Lighting Research & Technology}. 
#'   \doi{10.1177/14771535231170500}
#'
#' @examples
# Dataset with POSIXct time vector
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", 24),
#'     Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
#'     MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
#'   )
#' dataset1 %>%
#'   dplyr::reframe(
#'     "Midpoint of cmulative exposure" = midpointCE(MEDI, Datetime)
#'   )
#' 
#' # Dataset with HMS time vector
#' dataset2 <-
#'   tibble::tibble(
#'     Id = rep("A", 24),
#'     Time = hms::as_hms(lubridate::as_datetime(0) + lubridate::hours(0:23)),
#'     MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
#'   )
#' dataset2 %>%
#'   dplyr::reframe(
#'     "Midpoint of cmulative exposure" = midpointCE(MEDI, Time)
#'   )
#' 
#' # Dataset with duration time vector
#' dataset3 <-
#'   tibble::tibble(
#'     Id = rep("A", 24),
#'     Hour = lubridate::duration(0:23, "hours"),
#'     MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
#'   )
#' dataset3 %>%
#'   dplyr::reframe(
#'     "Midpoint of cmulative exposure" = midpointCE(MEDI, Hour)
#'   )
#' 
midpointCE <- function(Light.vector,
                       Time.vector,
                       na.rm = FALSE,
                       as.df = FALSE) {
  
  # Perform argument checks
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`Time.vector` must be POSIXct, hms, duration, or difftime!" =
      lubridate::is.POSIXct(Time.vector) | hms::is_hms(Time.vector) | 
      lubridate::is.duration(Time.vector) | lubridate::is.difftime(Time.vector),
    "`Light.vector` and `Time.vector` must be same length!" = 
      length(Light.vector) == length(Time.vector),
    "`na.rm` must be logical!" = is.logical(na.rm),
    "`as.df` must be logical!" = is.logical(as.df)
  )
  
  # Replace missing values with 0
  if(na.rm){
    Light.vector[is.na(Light.vector)] <- 0
  }
  
  # If any value is NA, return NA
  if(any(is.na(Light.vector))){
    midpointCE = convert_to_timescale(NA, Time.vector)
  }
  else{
    # Find midpoint of CE
    cumsum <- cumsum(Light.vector)
    halfSum <- cumsum[length(cumsum)] / 2
    midpointCE = Time.vector[which.min(abs(cumsum - halfSum))]
  }
  
  # Return as data frame or numeric vector
  if (as.df) {
    return(tibble::tibble("midpointCE" = midpointCE))
  } else {
    return(midpointCE)
  }
}

#' Exponential moving average filter (EMA)
#'
#' This function smoothes the data using an exponential moving average filter
#' with a specified decay half-life.
#'
#' @param Light.vector Numeric vector containing the light data. Missing values are
#'    replaced by 0.
#' @param Time.vector Vector containing the time data. Can be \link[base]{POSIXct}, \link[hms]{hms}, 
#'    \link[lubridate]{duration}, or \link[base]{difftime}.
#' @param decay The decay half-life controlling the exponential smoothing.
#'    Can be either a \link[lubridate]{duration} or a string. If it is a string, it 
#'    needs to be a valid \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`.
#'    The default is set to `"90 mins"` for a biologically relevant estimate (see
#'    the reference paper). 
#' @param epoch The epoch at which the data was sampled. Can be either a
#'    \link[lubridate]{duration} or a string. If it is a string, it needs to be
#'    either `"dominant.epoch"` (the default) for a guess based on the data, or a valid
#'    \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`.
#'
#' @return A numeric vector containing the smoothed light data. The output has the same
#'    length as `Light.vector`.
#'    
#' @export
#' 
#' @family metrics
#' 
#' @details The timeseries is assumed to be regular. Missing values in the
#'    light data will be replaced by 0.
#'    
#' @references 
#'    Price, L. L. A. (2014). On the Role of Exponential Smoothing in Circadian 
#'    Dosimetry. \emph{Photochemistry and Photobiology}, 90(5), 1184-1192.
#'    \doi{10.1111/php.12282}
#'    
#'    Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
#'    Quantification metrics. \emph{Lighting Research & Technology}. 
#'    \doi{10.1177/14771535231170500}
#'
#' @examples
#' sample.data.environment.EMA = sample.data.environment %>%
#'   dplyr::filter(Id == "Participant") %>%
#'   filter_Datetime(length = lubridate::days(2)) %>%
#'   dplyr::mutate(MEDI.EMA = exponential_moving_average(MEDI, Datetime))
#' 
#' # Plot to compare results
#' sample.data.environment.EMA %>%
#'   ggplot2::ggplot(ggplot2::aes(x = Datetime)) +
#'   ggplot2::geom_line(ggplot2::aes(y = MEDI), colour = "black") +
#'   ggplot2::geom_line(ggplot2::aes(y = MEDI.EMA), colour = "red")
#'   
exponential_moving_average <- function(Light.vector,
                                       Time.vector,
                                       decay = "90 min",
                                       epoch = "dominant.epoch") {
  
  # Perform argument checks
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`Time.vector` must be POSIXct, hms, duration, or difftime!" =
      lubridate::is.POSIXct(Time.vector) | hms::is_hms(Time.vector) | 
      lubridate::is.duration(Time.vector) | lubridate::is.difftime(Time.vector),
    "`Light.vector` and `Time.vector` must be same length!" = 
      length(Light.vector) == length(Time.vector),
    "`decay` must either be a duration or a string" =
      lubridate::is.duration(decay) | is.character(decay),
    "`epoch` must either be a duration or a string" =
      lubridate::is.duration(epoch) | is.character(epoch)
  )
  
  # Get the epochs based on the data
  if (is.character(epoch) && epoch == "dominant.epoch") {
    epoch <- count_difftime(tibble::tibble(Datetime = Time.vector))$difftime[1]
  }
  # If the user specified an epoch, use that instead
  else {
    epoch <- lubridate::as.duration(epoch)
  }
  
  # Replace missing values
  if (any(is.na(Light.vector))) {
    warning("Light data contains missing values! They are replaced by 0.")
    Light.vector[is.na(Light.vector)] <- 0
  }
  
  # Calculate smoothing factor beta
  decay <- lubridate::as.duration(decay)
  beta <- log(2) / (as.numeric(decay) / as.numeric(epoch))
  
  # EMA filter
  D <- double(length(Light.vector))
  for (idx in 1:length(Light.vector)) {
    if (idx == 1) {
      D[idx] <- beta * (Light.vector[idx])
    } else {
      D[idx] <- D[idx - 1] + beta * (Light.vector[idx] - D[idx - 1])
    }
  }
  
  # Return numeric vector of EMA light values
  return(D)
}

#' Circadian lighting metrics from Barroso et al. (2014)
#' 
#' This function calculates the metrics proposed by Barroso et al. (2014)
#' for light-dosimetry in the context of research on the non-visual effects of light.
#' The following metrics are calculated: 
#' 
#' \describe{
#'  \item{`bright_threshold`}{The maximum light intensity for which at least six 
#'    hours of measurements are at the same or higher level.}
#'  \item{`dark_threshold`}{The minimum light intensity for which at least eight 
#'    hours of measurements are at the same or lower level.}
#'  \item{`bright_mean_level`}{The 20% trimmed mean of all light intensity measurements
#'    equal or above the `bright_threshold`.}
#'  \item{`dark_mean_level`}{The 20% trimmed mean of all light intensity measurements
#'    equal or below the `dark_threshold`.}
#'  \item{`bright_cluster`}{The longest continuous time interval above the `bright_threshold`.}
#'  \item{`dark_cluster`}{The longest continuous time interval below the `dark_threshold`.}
#'  \item{`circadian_variation`}{A measure of periodicity of the daily lighting 
#'    schedule over a given set of days. Calculated as the coefficient of variation
#'    of input light data.
#'    }
#' }
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Time.vector Vector containing the time data. Can be \link[base]{POSIXct}, \link[hms]{hms}, 
#'    \link[lubridate]{duration}, or \link[base]{difftime}.
#' @param epoch The epoch at which the data was sampled. Can be either a
#'  \link[lubridate]{duration} or a string. If it is a string, it needs to be
#'  either `"dominant.epoch"` (the default) for a guess based on the data, or a valid
#'  \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`.
#' @param loop Logical. Should the data be looped? Defaults to `FALSE`.
#' @param na.rm Logical. Should missing values (NA) be removed for the calculation? 
#'    Defaults to `FALSE`. If `TRUE`, for the calculation of `bright_cluster` and
#'    `dark_cluster`, missing values will be replaced by 0 
#'    (see \code{\link{period_above_threshold}}). 
#' @param as.df Logical. Should a data frame be returned? If `TRUE`, a data
#'    frame with seven columns will be returned. Defaults to `FALSE`.
#'
#' @return List or dataframe with the seven values: `bright_threshold`, `dark_threshold`,
#'    `bright_mean_level`, `dark_mean_level`, `bright_cluster`, `dark_cluster`, 
#'    `circadian_variation`. The output type of `bright_cluster`, `dark_cluster`, 
#'    is a \link[lubridate]{duration} object.
#'    
#' @details 
#'    
#' @export
#' 
#' @references 
#'    Barroso, A., Simons, K., & Jager, P. de. (2014). Metrics of circadian 
#'    lighting for clinical investigations. \emph{Lighting Research & Technology},
#'    46(6), 637–649. \doi{10.1177/1477153513502664}
#'    
#'   Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
#'   Quantification metrics. \emph{Lighting Research & Technology}. 
#'   \doi{10.1177/14771535231170500}
#'
#' @examples
#' 
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("B", 60 * 24),
#'     Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*24-1)),
#'     MEDI = c(rep(sample(seq(0,1,0.1), 60*8, replace = TRUE)), 
#'              rep(sample(1:1000, 16, replace = TRUE), each = 60))
#'   )
#' 
#' dataset1 %>%
#'   dplyr::reframe(barroso_lighting_metrics(MEDI, Datetime, as.df = TRUE))
#'   
barroso_lighting_metrics <- function(Light.vector,
                                     Time.vector,
                                     epoch = "dominant.epoch",
                                     loop = FALSE,
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
  
  # Bright/dark thresholds
  bright_threshold <- 
    threshold_for_duration(Light.vector, Time.vector, "6 h", "above", epoch, na.rm = na.rm)[1]
  dark_threshold <- 
    threshold_for_duration(Light.vector, Time.vector, "8 h", "below", epoch, na.rm = na.rm)[1]
  
  # Bright/dark mean level
  bright_mean_level <- mean(Light.vector[Light.vector >= bright_threshold], trim = 0.2, na.rm = na.rm)
  dark_mean_level <- mean(Light.vector[Light.vector <= dark_threshold], trim = 0.2, na.rm = na.rm)
  
  # Bright/dark cluster
  bright_cluster <- period_above_threshold(Light.vector, Time.vector, "above", bright_threshold,
                               epoch = epoch, loop = loop, na.replace = na.rm)[1]
  dark_cluster <- period_above_threshold(Light.vector, Time.vector, "below", dark_threshold, 
                               epoch = epoch, loop = loop, na.replace = na.rm)[1]
  
  # Circadian variation
  circadian_variation <- (stats::sd(Light.vector, na.rm = na.rm) / mean(Light.vector, na.rm=na.rm)) %>% 
    round(2)
  
  # Prepare output
  out <- list(
    "bright_threshold" = bright_threshold,
    "dark_threshold" = dark_threshold,
    "bright_mean_level" = bright_mean_level,
    "dark_mean_level" = dark_mean_level,
    "bright_cluster" = bright_cluster,
    "dark_cluster" = dark_cluster,
    "circadian_variation" = circadian_variation
  )
  # Return data frame or list
  if (as.df) {
    out <- tibble::as_tibble(out)
  }
  return(out)
}

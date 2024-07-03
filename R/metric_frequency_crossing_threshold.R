#' Frequency of crossing light threshold
#'
#' This functions calculates the number of times a given threshold
#' light level is crossed.
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param threshold Single numeric value specifying the threshold light level to compare with.
#' @param na.rm Logical. Should missing light values be removed? Defaults to `FALSE`.
#' @param as.df Logical. Should the output be returned as a data frame? If `TRUE`, a data
#'    frame with a single column named `frequency_crossing_{threshold}` will be returned.
#'    Defaults to `FALSE`.
#'
#' @return Data frame or matrix with pairs of threshold and calculated values.
#' 
#' @export
#' 
#' @family metrics
#'
#' @references 
#'    Alvarez, A. A., & Wildsoet, C. F. (2013). Quantifying light
#'    exposure patterns in young adult students. \emph{Journal of Modern Optics},
#'    60(14), 1200–1208. \doi{10.1080/09500340.2013.845700}
#'    
#'   Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
#'   Quantification metrics. \emph{Lighting Research & Technology}. 
#'   \doi{10.1177/14771535231170500}
#'
#' @examples
# 
#' N = 60
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", N),
#'     Datetime = lubridate::as_datetime(0) + lubridate::minutes(1:N),
#'     MEDI = sample(c(sample(1:249, N / 2), sample(250:1000, N / 2))),
#'   )
#' 
#' dataset1 %>%
#'   dplyr::reframe("Frequency crossing 250lx" = frequency_crossing_threshold(MEDI, threshold = 250))
#' 
#' dataset1 %>%
#'   dplyr::reframe(frequency_crossing_threshold(MEDI, threshold = 250, as.df = TRUE))
#' 
frequency_crossing_threshold <- function(Light.vector,
                                        threshold,
                                        na.rm = FALSE,
                                        as.df = FALSE) {
  
  # Perform argument checks
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`threshold` must be numeric!" = is.numeric(threshold),
    "`threshold` must be one value!" = length(threshold) == 1,
    "`na.rm` must be logical!" = is.logical(na.rm),
    "`as.df` must be logical!" = is.logical(as.df)
  )
  
  # Remove NAs
  if (na.rm) {
    Light.vector <- Light.vector[!is.na(Light.vector)]
  }
  
  if (any(is.na(Light.vector))){
    fic <- NA
  }
  else{
    # Calculate FIC
    fic <- sum(abs(diff(compare_threshold(Light.vector, threshold))))
  }
  
  # Return data frame or numeric value
  if (as.df) {
    threshold <- as.character(threshold)
    return(tibble::tibble("frequency_crossing_{threshold}" := fic))
  } else {
    return(fic)
  }
}


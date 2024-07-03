
#' Disparity index
#'
#' This function calculates the continuous disparity index as described in
#' Fernández-Martínez et al. (2018).
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param na.rm Logical. Should missing values be removed? Defaults to FALSE
#' @param as.df Logical. Should the output be returned as a data frame? If `TRUE`, a data
#'    frame with a single column named `disparity_index` will be returned.
#'    Defaults to `FALSE`.
#'
#' @return Single column data frame or vector.
#' 
#' @family metrics
#' 
#' @export
#'
#' @references Fernández-Martínez, M., Vicca, S., Janssens, I. A., Carnicer, J.,
#'   Martín-Vide, J., & Peñuelas, J. (2018).
#'   The consecutive disparity index, D: A measure of temporal variability in
#'   ecological studies. \emph{Ecosphere}, 9(12), e02527.
#'   \doi{10.1002/ecs2.2527}
#'   
#'   Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
#'   Quantification metrics. \emph{Lighting Research & Technology}. 
#'   \doi{10.1177/14771535231170500}
#'
#' @examples
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", 24),
#'     Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
#'     MEDI = sample(0:1000, 24),
#'   )
#' dataset1 %>%
#'   dplyr::reframe(
#'     "Disparity index" = disparity_index(MEDI)
#'   )
#' 
disparity_index <- function(Light.vector,
                            na.rm = FALSE,
                            as.df = FALSE) {
  
  # Perform argument checks
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`na.rm` must be logical!" = is.logical(na.rm),
    "`as.df` must be logical!" = is.logical(as.df)
  )
  
  # Remove NAs
  if (na.rm) {
    Light.vector <- Light.vector[!is.na(Light.vector)]
  }
  
  if (any(is.na(Light.vector))){
    di <- NA
  }
  else{
    if (length(Light.vector) == 1) {
      di <- 0
    } else {
      # Calculate disparity index
      fractions <- (Light.vector[2:length(Light.vector)] + 1) /
        (Light.vector[1:length(Light.vector) - 1] + 1)
      di <- 1 / (length(Light.vector) - 1) * sum(abs(log(fractions)))
    }
  }
  # Return as data frame or numeric vector
  if (as.df) {
    return(tibble::tibble("disparity_index" = di))
  } else {
    return(di)
  }
}

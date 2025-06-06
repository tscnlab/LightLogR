#' Calculate the dose (value·hours)
#'
#' This function calculates the dose from a time series. For light, this is
#' equal to the actual definition of light exposure (CIE term luminous
#' exposure). Output will always be provided in value·hours (e.g., for light, lx·hours).
#'
#' The time series does not have to be regular, however, it will be aggregated
#' to a regular timeseries of the given epoch. Implicit gaps (i.e., no
#' observations), will be converted to NA values (which can be ignored with
#' `na.rm = TRUE`).
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Time.vector Vector containing the time data. Can be
#'   \link[base]{POSIXct}, \link[hms]{hms}, \link[lubridate]{duration}, or
#'   \link[base]{difftime}.
#' @param epoch The epoch at which the data was sampled. Can be either a
#'   \link[lubridate]{duration} or a string. If it is a string, it needs to be
#'   either `"dominant.epoch"` (the default) for a guess based on the data, or a
#'   valid \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`.
#' @param na.rm Logical. Should missing values (NA) be removed for the
#'   calculation? Defaults to `FALSE`.
#' @param as.df Logical. Should a data frame with be returned? If `TRUE`, a data
#'   frame with a single column named `dose` will be returned. Defaults to
#'   `FALSE`.
#'
#' @return A numeric object as single value, or single column data frame with the dose in value·hours
#'
#' @references Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for
#' light-dosimetry studies: Quantification metrics. \emph{Lighting Research &
#' Technology}. \doi{10.1177/14771535231170500}
#'
#' @export
#'
#' @family metrics
#'
#' @examples
#' 
#' dose(c(1,1,1,1), lubridate::dhours(c(1:4)), na.rm = TRUE)
#' #with gaps
#' dose(c(1,1,1), lubridate::dhours(c(1,3:4)), na.rm = TRUE)
#' #gaps can be aggregated to a coarser interval, which can be sensibe
#' #if they are still representative
#' dose(c(1,1,1), lubridate::dhours(c(1,3:4)), na.rm = TRUE, epoch = "2 hours")
#' 
dose <- function(Light.vector,
                 Time.vector,
                 epoch = "dominant.epoch",
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
  
  data <- tibble::tibble(Value = Light.vector, 
                         Datetime = 
                           suppressWarnings(
                             Time.vector |> lubridate::as_datetime())
                         )
  
  #ensure a gapless, regular timeseries
  if(data |> has_irregulars(epoch = epoch)) {
    data <- 
      data |> aggregate_Datetime(unit = epoch, 
                                 numeric.handler = \(x) mean(x, na.rm = na.rm))
  }
  if(data |> has_gaps(epoch = epoch)){
    data <- 
      data |> gap_handler(epoch = epoch)
  }
  
  # Calculate dose
  dose <- data |> 
            dplyr::summarize(
              dose = 
                sum(Value, na.rm = na.rm)*(epoch |> as.numeric())/3600)
  
  # Return data frame or numeric value
  if (as.df) {
    return(dose)
  } else {
    return(dose$dose)
  }
}

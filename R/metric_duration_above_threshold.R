#' Time above/below threshold or within threshold range
#'
#' This function calculates the time spent above/below a specified threshold
#' light level or within a specified range of light levels.
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Time.vector Vector containing the time data. Can be numeric, HMS or POSIXct.
#' @param comparison String specifying whether the time above or below threshold
#'    should be calculated. Can be either `"above"` (the default) or `"below"`. If
#'    two values are provided for `threshold`, this argument will be ignored.
#' @param threshold Single numeric value or two numeric values specifying the
#'    threshold light level(s) to compare with. If a vector with two values is provided,
#'    the time within the two thresholds will be calculated.
#' @param epoch The epoch at which the data was sampled. Can be either a
#'  `lubridate::duration()` or a string. If it is a string, it needs to be
#'  either `"dominant.epoch"` (the default) for a guess based on the data, or a valid
#'  `lubridate::duration()` string, e.g., `"1 day"` or `"10 sec"`.
#' @param na.rm Logical. Should missing values (NA) be removed for the calculation?
#'    Defaults to `FALSE`.
#' @param as.df Logical. Should a data frame with be returned? If `TRUE`, a data
#'    frame with a single column named `TAT_{threshold}` will be returned.
#'    Defaults to `FALSE`.
#'
#' @return A duration object (see \code{\link[lubridate]{duration}}) as single value,
#'    or single column data frame.
#'    
#' @references 
#'   Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
#'   Quantification metrics. \emph{Lighting Research & Technology}. 
#'   \url{https://doi.org/10.1177/14771535231170500}
#'
#' @export
#' 
#' @family metrics
#'
#' @examples
#' N <- 50
#' # Dataset with epoch = 1min
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", N),
#'     Datetime = lubridate::as_datetime(0) + lubridate::minutes(1:N),
#'     MEDI = sample(c(sample(1:249, N / 2), sample(250:1000, N / 2))),
#'   )
#' # Dataset with epoch = 30s
#' dataset2 <-
#'   tibble::tibble(
#'     Id = rep("B", N),
#'     Datetime = lubridate::as_datetime(0) + lubridate::seconds(seq(30, N * 30, 30)),
#'     MEDI = sample(c(sample(1:249, N / 2), sample(250:1000, N / 2))),
#'   )
#' dataset.combined <- rbind(dataset1, dataset2)
#' 
#' dataset1 %>%
#'   dplyr::reframe("TAT >250lx" = duration_above_threshold(MEDI, Datetime, threshold = 250))
#' 
#' dataset1 %>%
#'   dplyr::reframe(duration_above_threshold(MEDI, Datetime, threshold = 250, as.df = TRUE))
#' 
#' # Group by Id to account for different epochs
#' dataset.combined %>%
#'   dplyr::group_by(Id) %>%
#'   dplyr::reframe("TAT >250lx" = duration_above_threshold(MEDI, Datetime, threshold = 250))
#' 
duration_above_threshold <- function(Light.vector,
                                     Time.vector,
                                     comparison = c("above", "below"),
                                     threshold,
                                     epoch = "dominant.epoch",
                                     na.rm = FALSE,
                                     as.df = FALSE) {
  # Match input arguments
  comparison <- match.arg(comparison)

  # Perform argument checks
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`Time.vector` must be numeric, HMS, or POSIXct" =
      is.numeric(Time.vector) | hms::is_hms(Time.vector) | lubridate::is.POSIXct(Time.vector),
    "`threshold` must be numeric!" = is.numeric(threshold),
    "`threshold` must be either one or two values!" = length(threshold) %in% c(1, 2),
    "`epoch` must either be a duration or a string" =
      lubridate::is.duration(epoch) | is.character(epoch),
    "`na.rm` must be logical!" = is.logical(na.rm),
    "`as.df` must be logical!" = is.logical(as.df)
  )

  # Get the epochs based on the data
  if (epoch == "dominant.epoch") {
    epoch <- count_difftime(tibble::tibble(Datetime = Time.vector))$difftime[1]
  }
  # If the user specified an epoch, use that instead
  else {
    epoch <- lubridate::as.duration(epoch)
  }

  # Calculate TAT
  tat <- sum(compare_threshold(Light.vector, threshold, comparison, na.rm)) * as.numeric(epoch)

  # As duration object
  tat <- lubridate::as.duration(tat)

  # Return data frame or numeric value
  if (as.df) {
    threshold <- stringr::str_flatten(sort(threshold), collapse = "-")
    return(tibble::tibble("duration_{comparison}_{threshold}" := tat))
  } else {
    return(tat)
  }
}

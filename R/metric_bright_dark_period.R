#' Brightest or darkest continuous period
#'
#' This function finds the brightest or darkest continuous period of a given
#' timespan and calculates its `mean` light level, as well as the timing of the period's
#' `onset`, `midpoint`, and `offset`. It is defined as the period with the maximum
#' or minimum mean light level. Note that the data need to be regularly spaced 
#' (i.e., no gaps) for correct results.
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Time.vector Vector containing the time data. Can be \link[base]{POSIXct}, 
#'    \link[hms]{hms}, \link[lubridate]{duration}, or \link[base]{difftime}.
#' @param period String indicating the type of period to look for. Can be either
#'  `"brightest"`(the default) or `"darkest"`.
#' @param timespan The timespan across which to calculate. Can be either a
#'  \link[lubridate]{duration} or a \link[lubridate]{duration} string, e.g.,
#'  `"1 day"` or `"10 sec"`.
#' @param epoch The epoch at which the data was sampled. Can be either a
#'    \link[lubridate]{duration} or a string. If it is a string, it needs to be
#'    either `"dominant.epoch"` (the default) for a guess based on the data, or a valid
#'    \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`.
#' @param loop Logical. Should the data be looped? If `TRUE`, a full copy of the data 
#'    will be concatenated at the end of the data. Makes only sense for 24 h data.
#'    Defaults to `FALSE`.
#' @param na.rm Logical. Should missing values be removed for the calculation?
#'    Defaults to `FALSE`.
#' @param as.df Logical. Should the output be returned as a data frame? Defaults
#'    to `TRUE`.
#'
#' @return A named list with the `mean`, `onset`, `midpoint`, and `offset` of the
#'    calculated brightest or darkest period, or if `as.df == TRUE` a data frame 
#'    with columns named `{period}_{timespan}_{metric}`. The output type corresponds
#'    to the type of `Time.vector`, e.g., if `Time.vector` is HMS, the timing metrics 
#'    will be also HMS, and vice versa for POSIXct.
#'
#' @details Assumes regular 24h light data. Otherwise, results may not be
#'    meaningful. Looping the data is recommended for finding the darkest period.
#'
#' @references 
#'   Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
#'   Quantification metrics. \emph{Lighting Research & Technology}. 
#'   \doi{10.1177/14771535231170500}
#'
#' @export
#' 
#' @family metrics
#'
#' @examples
#' # Dataset with light > 250lx between 06:00 and 18:00
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", 24),
#'     Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
#'     MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
#'   )
#' 
#' dataset1 %>%
#'   dplyr::reframe(bright_dark_period(MEDI, Datetime, "brightest", "10 hours",
#'     as.df = TRUE))
#' dataset1 %>%
#'   dplyr::reframe(bright_dark_period(MEDI, Datetime, "darkest", "7 hours",
#'     loop = TRUE, as.df = TRUE))
#' 
#' # Dataset with duration as Time.vector
#' dataset2 <-
#'   tibble::tibble(
#'     Id = rep("A", 24),
#'     Datetime = lubridate::dhours(0:23),
#'     MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
#'   )
#' 
#' dataset2 %>%
#'   dplyr::reframe(bright_dark_period(MEDI, Datetime, "brightest", "10 hours",
#'                                     as.df = TRUE))
#' dataset2 %>%
#'   dplyr::reframe(bright_dark_period(MEDI, Datetime, "darkest", "5 hours",
#'                                     loop = TRUE, as.df = TRUE))
#' 
bright_dark_period <- function(Light.vector,
                               Time.vector,
                               period = c("brightest", "darkest"),
                               timespan = "10 hours",
                               epoch = "dominant.epoch",
                               loop = FALSE,
                               na.rm = FALSE,
                               as.df = FALSE) {
  # Match arguments
  period <- match.arg(period)

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
    "`timespan` must either be a duration or a string" =
      lubridate::is.duration(timespan) | is.character(timespan),
    "`na.rm` must be logical!" = is.logical(na.rm),
    "`as.df` must be logical!" = is.logical(as.df)
  )

  # Check whether time series is regularly spaced
  if (length(unique(diff(Time.vector))) > 1) {
    warning("`Time.vector` is not regularly spaced. Calculated results may be incorrect!")
  }

  # Get the epochs based on the data
  if (is.character(epoch) && epoch == "dominant.epoch") {
    epoch <- count_difftime(tibble::tibble(Datetime = Time.vector))$difftime[1]
  }
  # If the user specified an epoch, use that instead
  epoch <- lubridate::as.duration(epoch)

  # Convert timespan to seconds
  timespan <- lubridate::as.duration(timespan)
  
  # Check if timespan longer than Time.vector
  time.total <- dplyr::last(Time.vector) - dplyr::first(Time.vector)
  stopifnot("Timespan must be shorter than length of `Time.vector` interval!" = 
              timespan < time.total)

  # Loop data
  if (loop) {
    Light.vector <- c(Light.vector, Light.vector)
    Time.vector <- c(Time.vector, Time.vector)
  }

  # Calculate window size
  window <- floor(as.numeric(timespan) / as.numeric(epoch))
  if (window %% 2 != 0) {
    window <- window + 1
  }

  # Calculate rolling means
  means <- slider::slide_vec(Light.vector, .f = mean, na.rm = na.rm,
                         .before = window/2-1, .after = window/2, 
                         .complete = TRUE)

  # Find maximum/minimum mean value
  center <- switch(period,
    "brightest" = which(means == max(means, na.rm = TRUE))[1],
    "darkest" = which(means == min(means, na.rm = TRUE))[1]
  )

  # Prepare output
  out <- list(
    "mean" = means[center],
    "midpoint" = Time.vector[center],
    "onset" = Time.vector[center - (window / 2 - 1)],
    "offset" = Time.vector[center + (window / 2)]
  )

  # Return as data frame or numeric matrix
  if (as.df) {
    ts <- paste0(as.numeric(timespan, unit = "hours"), "h")
    out <- tibble::as_tibble(out) %>%
      dplyr::rename_with(~paste(period, ts, .x, sep = "_"))
  }
  return(out)
}

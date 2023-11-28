#' Brightest or darkest continuous period
#'
#' This function finds the brightest or darkest continuous period of a given
#' timespan and calculates its `mean` light level, as well as the timing of the period's
#' `onset`, `midpoint`, and `offset`. It is defined as the period with the maximum
#' or minimum mean light level. Note that the data need to be regularly spaced 
#' (i.e., no gaps) for correct results.
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Datetime.vector Vector containing the time data. Can be POSIXct or numeric.
#' @param period String indicating the type of period to look for. Can be either
#'  `"brightest"`(the default) or `"darkest"`.
#' @param timespan The timespan across which to calculate. Can be either a
#'  `lubridate::duration()` or a `lubridate::duration()` string, e.g.,
#'  `"1 day"` or `"10 sec"`.
#' @param epoch The epoch at which the data was sampled. Can be either a
#'  `lubridate::duration()` or a string. If it is a string, it needs to be
#'  either `"dominant.epoch"` (the default) for a guess based on the data, or a valid
#'  `lubridate::duration()` string, e.g., `"1 day"` or `"10 sec"`.
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
#'    with columns named `{period}_{timespan}_{metric}`. 
#'
#' @details Assumes regular 24h light data. Otherwise, results may not be
#'    meaningful. Looping the data is recommended for finding the darkest period.
#'
#' @export
#'
#' @examples
# Dataset with light > 250lx between 06:00 and 18:00
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", 24),
#'     Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
#'     MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
#'   )
#'
#' dataset1 %>%
#'   dplyr::summarise(bright_dark_period(MEDI, Datetime, "brightest", "10 hours",
#'     as.df = TRUE
#'   ))
#' dataset1 %>%
#'   dplyr::summarise(bright_dark_period(MEDI, Datetime, "darkest", "5 hours",
#'     loop = TRUE, as.df = TRUE
#'   ))
bright_dark_period <- function(Light.vector,
                               Datetime.vector,
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
    "`Datetime.vector` must be POSIXct" = lubridate::is.POSIXct(Datetime.vector),
    "`epoch` must either be a duration or a string" =
      lubridate::is.duration(epoch) | is.character(epoch),
    "`timespan` must either be a duration or a string" =
      lubridate::is.duration(timespan) | is.character(timespan),
    "`na.rm` must be logical!" = is.logical(na.rm),
    "`as.df` must be logical!" = is.logical(as.df)
  )

  # Check whether time series is regularly spaced
  if (length(unique(diff(Datetime.vector))) > 1) {
    warning("`Datetime.vector` is not regularly spaced. Calculated results may be incorrect!")
  }

  # Get the epochs based on the data
  if (epoch == "dominant.epoch") {
    epoch <- count.difftime(tibble::tibble(Datetime = Datetime.vector))$difftime[1]
  }
  # If the user specified an epoch, use that instead
  epoch <- lubridate::as.duration(epoch)

  # Convert timespan to seconds
  timespan <- lubridate::as.duration(timespan)

  # Loop data
  if (loop) {
    Light.vector <- c(Light.vector, Light.vector)
    span <- dplyr::last(Datetime.vector) - Datetime.vector[1]
    Datetime.vector <- c(Datetime.vector, Datetime.vector + span + epoch)
  }

  # Calculate window size
  window <- floor(as.numeric(timespan) / as.numeric(epoch))
  if (window %% 2 != 0) {
    window <- window + 1
  }

  # Calculate rolling means
  means <- zoo::rollapply(Light.vector, window, mean,
    na.rm = na.rm,
    partial = FALSE, fill = NA
  )

  # Find maximum/minimum mean value
  center <- switch(period,
    "brightest" = which(means == max(means, na.rm = TRUE))[1],
    "darkest" = which(means == min(means, na.rm = TRUE))[1]
  )

  # Prepare output
  out <- list(
    "mean" = means[center],
    "midpoint" = hms::as_hms(Datetime.vector[center]),
    "onset" = hms::as_hms(Datetime.vector[center - (window / 2) + 1]),
    "offset" = hms::as_hms(Datetime.vector[center + (window / 2)])
  )

  # Return as data frame or numeric matrix
  if (as.df) {
    ts <- paste0(as.numeric(timespan, unit = "hours"), "h")
    out <- tibble::as_tibble(out) %>%
      dplyr::rename_with(~ paste(period, ts, .x, sep = "_"))
  }
  return(out)
}

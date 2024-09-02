#' Create a (shifted) sequence of Datetimes for axis breaks
#'
#' Take a vector of Datetimes and create a sequence of Datetimes with a given
#' shift and interval. This is a helper function to create breaks for plotting,
#' e.g. in [gg_days()], and is best used in conjunction with
#' [Datetime_limits()]. The function is a thin wrapper around [seq()].
#'
#' @param x a vector of `Datetimes`
#' @param shift a `numeric` giving the number of  `duration` object, e.g.
#'   `lubridate::duration(12, "hours")`
#' @param by a `character` scalar giving the unit of the interval in
#'   [base::seq()]
#'
#' @return a `vector` of `Datetimes`
#' @export
#'
#' @examples
#' dataset <- c("2023-08-15", "2023-08-20")
#' Datetime_breaks(dataset)
#' Datetime_breaks(dataset, shift = 0)
#' Datetime_breaks(dataset, by = "12 hours")

Datetime_breaks <- function(x, 
                            shift = lubridate::duration(12, "hours"),
                            by = "1 day") {
  x <- lubridate::as_datetime(x)
  (min(x) + shift) %>% 
    seq(max(x), by = by)
}

#' Find or set sensible limits for Datetime axis
#'
#' Take a vector of `Datetimes` and return the start of the first and end of the
#' last day of data. The `start` and the `length` can be adjusted by
#' `durations`, like [lubridate::ddays()]. It is used in the [gg_days()]
#' function to return a sensible x-axis. This function is a thin wrapper around
#' [lubridate::floor_date()] and [lubridate::ceiling_date()].
#'
#' @param x a vector of `Datetimes`
#' @param start optional `duration` object, e.g. `lubridate::ddays(1)` that
#'   shifts the start of the `Datetime` vector by this amount.
#' @param length optional `duration` object, e.g. `lubridate::ddays(7)` that
#'   shifts the end of the `Datetime` vector by this amount from the (adjusted)
#'   start. Depending on the data, you might have to subtract one day from the
#'   desired length to get the correct axis-scaling if you start at midnight.
#' @param unit a `character` scalar giving the unit of rounding in
#'   [lubridate::floor_date()] and [lubridate::ceiling_date()]
#' @param midnight.rollover a `logical` scalar indicating whether to rollover in cases of exact matches of rounded values and input values. Helpful if some cases fall exactly on the rounded values and others don`t.
#' @param ... other arguments passed to [lubridate::floor_date()] and
#'   [lubridate::ceiling_date()]
#'
#' @return a 2 item `vector` of `Datetimes` with the (adjusted) start and end of
#'   the input vector.
#' @export
#'
#' @examples
#' dataset <- c("2023-08-15", "2023-08-20")
#' breaks <- Datetime_breaks(dataset)
#' Datetime_limits(breaks)
#' Datetime_limits(breaks, start = lubridate::ddays(1))
#' Datetime_limits(breaks, length = lubridate::ddays(2))
Datetime_limits <- function(x,
                            start = NULL,
                            length = NULL,
                            unit = "1 day",
                            midnight.rollover = FALSE,
                            ...) {
  
  min_date <- x %>% lubridate::as_datetime() %>% min()
  if(!is.null(start)) {
    min_date <- (min_date + start)
    min_date %>% lubridate::as_datetime()
  }
  max_date <- 
    if(is.null(length)) {
      x %>% lubridate::as_datetime() %>% max()
    } else {
      y <- (min_date + length)
      y %>% lubridate::as_datetime()
    }
  
  if(midnight.rollover) {
  if(identical(max_date, lubridate::ceiling_date(max_date, unit = unit, ...))) {
    max_date <- max_date + lubridate::duration(unit)
  }
  }
  
  c(lubridate::floor_date(min_date, unit = unit, ...),
    lubridate::ceiling_date(max_date, unit = unit, ...))
}
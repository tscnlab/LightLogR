#' Mean/first/last timing above/below threshold.
#'
#' This function calculates the mean/first/last timepoint where light levels are
#' above or below a given threshold intensity within the given interval.
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Time.vector Vector containing the time data. Can be numeric, HMS or POSIXct.
#' @param metric String indicating which timing metrics to calculate. Possible values
#'    are `"mean"` (default), `"first"`, and `"last"` timing above threshold.
#' @param comparison String specifying whether the time above or below threshold
#'    should be calculated. Can be either `"above"` (the default) or `"below"`. If
#'    two values are provided for `threshold`, this argument will be ignored.
#' @param threshold Single numeric value or two numeric values specifying the
#'    threshold light level(s) to compare with. If a vector with two values is provided,
#'    the timing corresponding to light levels between the two thresholds will be
#'    calculated.
#' @param na.rm Logical. Should missing values be removed for the calculation?
#'    Defaults to `FALSE`.
#' @param as.df Logical. Should a data frame with be returned? If `TRUE`, a data
#'    frame with a single column named `{metric}_{threshold}` will be returned.
#'    Defaults to `FALSE`.
#'
#' @return Single value or single column dataframe. If `Time.vector` is of type
#'    HMS or POSIXct a HMS object (see \code{\link[hms]{hms}}) will be returned,
#'    otherwise a numeric object.
#'
#' @references Reid, K. J., Santostasi, G., Baron, K. G., Wilson, J., Kang, J.,
#'    & Zee, P. C. (2014). Timing and Intensity of Light Correlate with Body Weight
#'     in Adults. \emph{PLOS ONE}, 9(4), e92251.
#'      \url{https://doi.org/10.1371/journal.pone.0092251}
#'
#' @export
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
#'   dplyr::summarise(timing_above_threshold(MEDI, Datetime, "mean", "above", 250))
#'
#' dataset1 %>%
#'   dplyr::summarise(timing_above_threshold(MEDI, Datetime, "first", "above", 250))
#'
#' dataset1 %>%
#'   dplyr::summarise(timing_above_threshold(MEDI, Datetime, "last", "above", 250))
#'
timing_above_threshold <- function(Light.vector,
                                   Time.vector,
                                   metric = c("mean", "first", "last"),
                                   comparison = c("above", "below"),
                                   threshold,
                                   na.rm = FALSE,
                                   as.df = FALSE) {
  # Match arguments
  metric <- match.arg(metric)
  comparison <- match.arg(comparison)

  # Perform argument checks
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`Time.vector` must be numeric, HMS, or POSIXct" =
      is.numeric(Time.vector) | hms::is_hms(Time.vector) | lubridate::is.POSIXct(Time.vector),
    "`threshold` must be numeric!" = is.numeric(threshold),
    "`threshold` must be either one or two values!" = length(threshold) %in% c(1, 2),
    "`na.rm` must be logical!" = is.logical(na.rm),
    "`as.df` must be logical!" = is.logical(as.df)
  )

  # Convert to HMS
  if (hms::is_hms(Time.vector) | lubridate::is.POSIXct(Time.vector)) {
    Time.vector <- Time.vector %>% hms::as_hms()
  }

  # Calculate timing metric
  t <- Time.vector[compare_threshold(Light.vector, threshold, comparison, na.rm)]
  xlit <- switch(metric,
    "mean" = t %>% as.numeric() %>% mean() %>% round(),
    "first" = t %>% dplyr::first(),
    "last" = t %>% dplyr::last()
  )

  # Convert to HMS
  if (hms::is_hms(Time.vector) | lubridate::is.POSIXct(Time.vector)) {
    xlit <- xlit %>% hms::as_hms()
  }

  # Return data frame or numeric value
  if (as.df) {
    name <- switch(metric,
      "mean" = "MLiT",
      "first" = "FLiT",
      "last" = "LLiT"
    )
    threshold <- stringr::str_flatten(sort(threshold), collapse = "-")
    return(tibble::tibble("{name}_{threshold}" = xlit))
  } else {
    return(xlit)
  }
}

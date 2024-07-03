#' Mean/first/last timing above/below threshold.
#'
#' This function calculates the mean, first, and last timepoint (MLiT, FLiT, LLiT)
#' where light levels are above or below a given threshold intensity within the given 
#' time interval.
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Time.vector Vector containing the time data. Can be \link[base]{POSIXct}, 
#'    \link[hms]{hms}, \link[lubridate]{duration}, or \link[base]{difftime}.
#' @param comparison String specifying whether the time above or below threshold
#'    should be calculated. Can be either `"above"` (the default) or `"below"`. If
#'    two values are provided for `threshold`, this argument will be ignored.
#' @param threshold Single numeric value or two numeric values specifying the
#'    threshold light level(s) to compare with. If a vector with two values is provided,
#'    the timing corresponding to light levels between the two thresholds will be
#'    calculated.
#' @param na.rm Logical. Should missing values be removed for the calculation?
#'    Defaults to `FALSE`.
#' @param as.df Logical. Should a data frame be returned? If `TRUE`, a data
#'    frame with three columns (MLiT, FLiT, LLiT) and the threshold (e.g., `MLiT_{threshold}`)
#'    will be returned. Defaults to `FALSE`.
#'
#' @return List or dataframe with the three values: `mean`, `first`, and `last` timing
#'    above threshold. The output type corresponds to the type of `Time.vector`, 
#'    e.g., if `Time.vector` is HMS, the timing metrics will be also
#'    HMS, and vice versa for POSIXct and numeric. 
#'
#' @export
#' 
#' @family metrics
#'
#' @references 
#'   Reid, K. J., Santostasi, G., Baron, K. G., Wilson, J., Kang, J.,
#'   & Zee, P. C. (2014). Timing and Intensity of Light Correlate with Body Weight
#'     in Adults. \emph{PLOS ONE}, 9(4), e92251.
#'      \doi{10.1371/journal.pone.0092251}
#'      
#'   Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
#'   Quantification metrics. \emph{Lighting Research & Technology}. 
#'   \doi{10.1177/14771535231170500}
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
#' # Above threshold
#' dataset1 %>%
#'   dplyr::reframe(timing_above_threshold(MEDI, Datetime, "above", 250, as.df = TRUE))
#' 
#' # Below threshold
#' dataset1 %>%
#'   dplyr::reframe(timing_above_threshold(MEDI, Datetime, "below", 10, as.df = TRUE))
#' 
#' # Input = HMS -> Output = HMS
#' dataset1 %>%
#'   dplyr::reframe(timing_above_threshold(MEDI, hms::as_hms(Datetime), "above", 250, as.df = TRUE))
#' 
timing_above_threshold <- function(Light.vector,
                                   Time.vector,
                                   comparison = c("above", "below"),
                                   threshold,
                                   na.rm = FALSE,
                                   as.df = FALSE) {
  # Match arguments
  comparison <- match.arg(comparison)

  # Perform argument checks
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`Time.vector` must be POSIXct, hms, duration, or difftime!" =
      lubridate::is.POSIXct(Time.vector) | hms::is_hms(Time.vector) | 
      lubridate::is.duration(Time.vector) | lubridate::is.difftime(Time.vector),
    "`Light.vector` and `Time.vector` must be same length!" = 
      length(Light.vector) == length(Time.vector),
    "`threshold` must be numeric!" = is.numeric(threshold),
    "`threshold` must be either one or two values!" = length(threshold) %in% c(1, 2),
    "`na.rm` must be logical!" = is.logical(na.rm),
    "`as.df` must be logical!" = is.logical(as.df)
  )

  # Calculate timing metric
  t <- Time.vector[compare_threshold(Light.vector, threshold, comparison, na.rm)]
  mlit = t %>% as.numeric() %>% mean() %>% round()
  flit = t %>% dplyr::first()
  llit = t %>% dplyr::last()
  
  # Convert to corresponding time scale
  mlit <- mlit %>% convert_to_timescale(Time.vector)
  
  # Prepare output
  out <- list(
    "mean" = mlit, 
    "first" = flit, 
    "last" = llit
  )

  # Return data frame or list
  if (as.df) {
    if(length(threshold) == 2){
      comparison <- "within"
    }
    threshold <- stringr::str_flatten(sort(threshold), collapse = "-")
    out <- tibble::as_tibble(out) %>%
      dplyr::rename_with(~paste(.x, "timing", comparison, threshold, sep = "_"))
  }
  return(out)
}

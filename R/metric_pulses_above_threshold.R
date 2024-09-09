#' Pulses above threshold
#'
#' This function clusters the light data into continuous clusters (pulses) of
#' light above/below a given threshold. Clustering may be fine-tuned by setting
#' the minimum length of the clusters and by allowing brief interruptions to be
#' included in a single cluster, with a specified maximum length of interruption
#' episodes and proportion of total amount of interruptions to light above
#' threshold.
#'
#' @param Light.vector Numeric vector containing the light data. Missing values will
#'    be considered as `FALSE` when comparing light levels against the threshold.
#' @param Time.vector Vector containing the time data. Can be \link[base]{POSIXct}, 
#'    \link[hms]{hms}, \link[lubridate]{duration}, or \link[base]{difftime}.
#' @param comparison String specifying whether the time above or below threshold
#'    should be calculated. Can be either `"above"` (the default) or `"below"`. If
#'    two values are provided for `threshold`, this argument will be ignored.
#' @param threshold Single numeric value or two numeric values specifying the
#'    threshold light level(s) to compare with. If a vector with two values is provided,
#'    the timing corresponding to light levels between the two thresholds will be
#'    calculated.
#' @param min.length The minimum length of a pulse. Can be either a
#'  \link[lubridate]{duration} or a string. If it is a string, it needs to be a valid
#'  \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`. Defaults to 
#'  `"2 mins"` as in Wilson et al. (2018).
#' @param max.interrupt Maximum length of each episode of interruptions. Can be either a
#'  \link[lubridate]{duration} or a string. If it is a string, it needs to be a valid
#'  \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`. Defaults to
#'  `"8 mins"` as in Wilson et al. (2018).
#' @param prop.interrupt Numeric value between `0` and `1` specifying the
#'    maximum proportion of the total number of interruptions. Defaults to `0.25` 
#'    as in Wilson et al. (2018).
#' @param epoch The epoch at which the data was sampled. Can be either a
#'    \link[lubridate]{duration} or a string. If it is a string, it needs to be
#'    either `"dominant.epoch"` (the default) for a guess based on the data, or a valid
#'    \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`.
#' @param return.indices Logical. Should the cluster indices be returned? Only works if
#'    `as.df` is `FALSE`. Defaults to `FALSE`.
#' @param na.rm Logical. Should missing values be removed for the calculation of
#'    pulse metrics? Defaults to `FALSE`.
#' @param as.df Logical. Should a data frame be returned? If `TRUE`, a data
#'    frame with seven columns ("n", "mean_level", "mean_duration", "total_duration",
#'    "mean_onset", "mean_midpoint", "mean_offset") and the threshold (e.g., `_{threshold}`)
#'    will be returned. Defaults to `FALSE`.
#'
#' @return List or data frame with calculated values.
#' 
#' @details The timeseries is assumed to be regular. Missing values in the
#'    light data will be replaced by 0.
#'  
#' @export
#' 
#' @family metrics
#'
#' @references Wilson, J., Reid, K. J., Braun, R. I., Abbott, S. M., & Zee, P. C.
#'    (2018). Habitual light exposure relative to circadian timing in delayed
#'    sleep-wake phase disorder. \emph{Sleep}, 41(11).
#'    \doi{10.1093/sleep/zsy166}
#'
#' @examples
#' # Sample data
#' data = sample.data.environment %>%
#'   dplyr::filter(Id == "Participant") %>%
#'   filter_Datetime(length = lubridate::days(1)) %>% 
#'   dplyr::mutate(
#'     Time = hms::as_hms(Datetime),
#'   )
#' 
#' # Time vector as datetime
#' data %>%
#'   dplyr::reframe(pulses_above_threshold(MEDI, Datetime, threshold = 250, as.df = TRUE))
#' 
#' # Time vector as hms time
#' data %>%
#'   dplyr::reframe(pulses_above_threshold(MEDI, Time, threshold = 250, as.df = TRUE))
#' 
#' # Pulses below threshold 
#' data %>%
#'   dplyr::reframe(pulses_above_threshold(MEDI, Datetime, "below", threshold = 250, as.df = TRUE))
#' 
#' # Pulses within threshold range
#' data %>%
#'   dplyr::reframe(pulses_above_threshold(MEDI, Datetime, threshold = c(250,1000), as.df = TRUE))
#' 
pulses_above_threshold <- function(Light.vector,
                                   Time.vector,
                                   comparison = c("above", "below"),
                                   threshold,
                                   min.length = "2 mins",
                                   max.interrupt = "8 mins",
                                   prop.interrupt = 0.25,
                                   epoch = "dominant.epoch",
                                   return.indices = FALSE,
                                   na.rm = FALSE,
                                   as.df = FALSE) {
  
  # Match input arguments
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
    "`min.length` must either be a duration or a valid duration string" = 
      lubridate::is.duration(min.length) | is.character(min.length),
    "`max.interrupt` must either be a duration or a valid duration string" = 
      lubridate::is.duration(max.interrupt) | is.character(max.interrupt),
    "`prop.interrupt` must be numeric" = is.numeric(prop.interrupt),
    "`prop.interrupt` must be between 0 and 1" = prop.interrupt >= 0 & prop.interrupt <= 1,
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
  
  # Parse duration inputs
  min.length <- lubridate::as.duration(min.length)
  max.interrupt <- lubridate::as.duration(max.interrupt)
  
  # Additional checks
  stopifnot(
    "Time parameters must be equal to or longer than the epoch." = 
      any(c(min.length, max.interrupt) >= epoch)
  )
  
  # Convert to sample counts
  min.length <- round(min.length / epoch)
  max.interrupt <- round(max.interrupt / epoch)
  
  # Find pulses
  pulses <- find_clusters(
    compare_threshold(Light.vector, threshold, comparison), 
    min.length, max.interrupt, prop.interrupt, "pulse"
  )
  
  # Summarise pulse metrics
  options(dplyr.summarise.inform = FALSE)
  data.pulses <- 
    tibble::tibble(
      row_idx = 1:length(Light.vector), 
      light = Light.vector, 
      time = as.numeric(Time.vector)
    ) %>% 
    dplyr::left_join(pulses, by = "row_idx") %>%
    dplyr::filter(is_pulse) %>%
    
    # Summarise per pulse
    dplyr::group_by(pulse_idx) %>%
    dplyr::summarise(
      level = mean(light, na.rm = na.rm),
      duration = dplyr::n()*epoch,
      onset = dplyr::first(time),
      offset = dplyr::last(time),
      midpoint = mean(time, na.rm = na.rm)
    ) %>%
    dplyr::ungroup() %>%
    
    # Summarise across pulses
    dplyr::summarise(
      n = dplyr::n(),
      mean_level = mean(level),
      mean_duration = mean(duration) %>% round() %>% lubridate::as.duration(),
      total_duration = mean_duration*n,
      mean_onset = mean(onset) %>% round(),
      mean_midpoint = mean(midpoint) %>% round(),
      mean_offset = mean(offset) %>% round(),
    ) %>%
    
    # Convert timing metrics to corresponding time scale
    dplyr::mutate(
      dplyr::across(
        c(mean_onset, mean_midpoint, mean_offset), 
        ~convert_to_timescale(.x, Time.vector)
      )
    )
  
  # Return data frame or list
  if (as.df) {
    if(length(threshold) == 2){
      comparison <- "within"
    }
    threshold <- stringr::str_flatten(sort(threshold), collapse = "-")
    data.pulses <- data.pulses %>%
      dplyr::rename_with(~paste(.x, "pulses", comparison, threshold, sep = "_"))
    if(return.indices){
      warning("`return.indices`is `TRUE` but indices cannot be returned if `as.df` is `TRUE`. Please use `as.df = FALSE`")
    }
  }
  else{
    data.pulses <- data.pulses %>% as.list()
    if(return.indices){
      data.pulses$pulse_indices <- pulses$row_idx
    }
  }
  return(data.pulses)
}

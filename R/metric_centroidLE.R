#' Centroid of light exposure
#'
#' This function calculates the centroid of light exposure as the mean of the
#' time vector weighted in proportion to the corresponding binned light intensity.
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Time.vector Vector containing the time data. Can be numeric, HMS or POSIXct.
#' @param bin.size Value specifying size of bins to average the light data over.
#'    If `Time.vector` is of type POSIXct or HMS, `bin.size` must be either a 
#'    `lubridate::duration()` or a `lubridate::duration()` string, e.g., 
#'    `"1 day"` or `"10 sec"`. Otherwise, if `Time.vector` is numeric, `bin.size` 
#'    must be also numeric. If nothing is provided, no binning will be performed.
#' @param na.rm Logical. Should missing values be removed for the calculation?
#'    Defaults to `FALSE`.
#' @param as.df Logical. Should the output be returned as a data frame? Defaults
#'    to `FALSE`.
#'
#' @return Single column data frame or vector.
#' 
#' @export
#' 
#' @family metrics
#' 
#' @references Phillips, A. J. K., Clerx, W. M., O’Brien, C. S., Sano, A., Barger,
#'    L. K., Picard, R. W., Lockley, S. W., Klerman, E. B., & Czeisler, C. A. (2017).
#'    Irregular sleep/wake patterns are associated with poorer academic performance
#'    and delayed circadian and sleep/wake timing. \emph{Scientific Reports},
#'    7(1), 3216. \url{https://doi.org/10.1038/s41598-017-03171-4}
#'    
#'   Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
#'   Quantification metrics. \emph{Lighting Research & Technology}. 
#'   \url{https://doi.org/10.1177/14771535231170500}
#'
#' @examples
#' # Dataset with POSIXct time vector
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", 24),
#'     Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
#'     MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
#'   )
#' dataset1 %>%
#'   dplyr::reframe(
#'     "Centroid of light exposure" = centroidLE(MEDI, Datetime, "2 hours")
#'   )
#' 
#' # Dataset with HMS time vector
#' dataset2 <-
#'   tibble::tibble(
#'     Id = rep("A", 24),
#'     Time = hms::as_hms(lubridate::as_datetime(0) + lubridate::hours(0:23)),
#'     MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
#'   )
#' dataset2 %>%
#'   dplyr::reframe(
#'     "Centroid of light exposure" = centroidLE(MEDI, Time, "2 hours")
#'   )
#' 
#' # Dataset with numeric time vector
#' dataset3 <-
#'   tibble::tibble(
#'     Id = rep("A", 24),
#'     Hour = 0:23,
#'     MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
#'   )
#' dataset3 %>%
#'   dplyr::reframe(
#'     "Centroid of light exposure" = centroidLE(MEDI, Hour, 2)
#'   )
#' 
centroidLE <- function(Light.vector,
                       Time.vector,
                       bin.size = NULL,
                       na.rm = FALSE,
                       as.df = FALSE) {
  
  # Perform argument checks
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`Time.vector` must be numeric, HMS, or POSIXct" =
      is.numeric(Time.vector) | hms::is_hms(Time.vector) | lubridate::is.POSIXct(Time.vector),
    "`na.rm` must be logical!" = is.logical(na.rm),
    "`as.df` must be logical!" = is.logical(as.df)
  )
  if (!is.null(bin.size)) {
    if (lubridate::is.POSIXct(Time.vector) | hms::is_hms(Time.vector)) {
      stopifnot("`bin.size` must be a either a `lubridate::duration` object or `lubridate::duration` string, because `Time.vector` is HMS or POSIXct" = 
                  !is.numeric(bin.size))
      bin.size <- lubridate::duration(bin.size)
      stopifnot("`bin.size` must be a either a `lubridate::duration` object or `lubridate::duration` string, because `Time.vector` is HMS or POSIXct" = 
                  !is.na(bin.size) & lubridate::is.duration(bin.size))
      bin.size <- lubridate::as.period(bin.size)
    }
    else {
      stopifnot("`bin.size` must be numeric because `Time.vector` is numeric" = 
                  is.numeric(bin.size))
    }
  }
  
  # Make tibble
  df <- tibble::tibble(Light = Light.vector, Time = Time.vector)
  
  # Average into bins
  if(!is.null(bin.size)) {
    if (lubridate::is.POSIXct(Time.vector)){
      df <- df %>%
        dplyr::group_by(Time = lubridate::floor_date(Time, bin.size)) %>%
        dplyr::summarise(Light = mean(Light, na.rm = na.rm))
    }
    if (hms::is_hms(Time.vector)) {
      df <- df %>%
        dplyr::group_by(
          Time = lubridate::as_datetime(Time, tz = "UTC") %>% 
            lubridate::floor_date(bin.size) %>% hms::as_hms()
        ) %>%
        dplyr::summarise(Light = mean(Light, na.rm = na.rm))
    }
    if (is.numeric(Time.vector)) {
      df <- df %>%
        dplyr::group_by(Time = (Time - Time %% bin.size)) %>%
        dplyr::summarise(Light = mean(Light, na.rm = na.rm))
    }
  }
  
  # Calculate weighted mean
  weights <- (df$Light / sum(df$Light, na.rm = na.rm))
  centroidLE <- sum(as.numeric(df$Time) * weights, na.rm = na.rm)
  
  # Convert to right time class
  if(hms::is_hms(Time.vector)) {
    centroidLE <- centroidLE %>% round() %>% hms::as_hms()
  }
  if(lubridate::is.POSIXct(Time.vector)){
    centroidLE <- centroidLE %>% round() %>% 
      lubridate::as_datetime(tz = lubridate::tz(Time.vector))
  }
  
  # Return data frame or numeric vector
  if (as.df) {
    return(tibble::tibble("centroidLE" = centroidLE))
  } else {
    return(centroidLE)
  }
}

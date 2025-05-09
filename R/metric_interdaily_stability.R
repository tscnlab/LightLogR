#' Interdaily stability (IS)
#'
#' This function calculates the variability of 24h light exposure patterns across
#' multiple days. Calculated as the ratio of the variance of the average daily
#' pattern to the total variance across all days. Calculated with mean hourly
#' light levels. Ranges between 0 (Gaussian noise) and 1 (Perfect Stability).
#'
#' Note that this metric will always be 1 if the data contains only one 24 h day.
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Datetime.vector Vector containing the time data. Must be POSIXct.
#' @param use.samplevar Logical. Should the sample variance be used (divide by N-1)? 
#'    By default (`FALSE`), the population variance (divide by N) is used, as described
#'    in Van Someren et al. (1999).
#' @param na.rm Logical. Should missing values be removed? Defaults to `FALSE`.
#' @param as.df Logical. Should the output be returned as a data frame? If `TRUE`, a data
#'    frame with a single column named `interdaily_stability` will be returned.
#'    Defaults to `FALSE`.
#'
#' @return Numeric value or dataframe with column 'IS'.
#' 
#' @export
#'
#' @family metrics
#' 
#' @references Van Someren, E. J. W., Swaab, D. F., Colenda, C. C., Cohen, W.,
#'    McCall, W. V., & Rosenquist, P. B. (1999). Bright Light Therapy: Improved
#'    Sensitivity to Its Effects on Rest-Activity Rhythms in Alzheimer Patients
#'    by Application of Nonparametric Methods. \emph{Chronobiology International},
#'    16(4), 505–518. \doi{10.3109/07420529908998724}
#'    
#'   Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
#'   Quantification metrics. \emph{Lighting Research & Technology}. 
#'   \doi{10.1177/14771535231170500}
#'
#' @examples
#'set.seed(1)
#' N <- 24 * 7
#' # Calculate metric for seven 24 h days with two measurements per hour
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", N * 2),
#'     Datetime = lubridate::as_datetime(0) + c(lubridate::minutes(seq(0, N * 60 - 30, 30))),
#'     MEDI = sample(1:1000, N * 2)
#'   )
#' dataset1 %>%
#'   dplyr::summarise(
#'     "Interdaily stability" = interdaily_stability(MEDI, Datetime)
#'   )
interdaily_stability <- function(Light.vector,
                                 Datetime.vector,
                                 use.samplevar = FALSE,
                                 na.rm = FALSE,
                                 as.df = FALSE
                                 ) {
  # Initial checks
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`Datetime.vector` must be POSIXct!" = lubridate::is.POSIXct(Datetime.vector),
    "`Light.vector` and `Datetime.vector` must be same length!" = 
      length(Light.vector) == length(Datetime.vector),
    "`use.samplevar` must be logical!" = is.logical(use.samplevar),
    "`na.rm` must be logical!" = is.logical(na.rm),
    "`as.df` must be logical!" = is.logical(as.df)
  )

  # Make data frame
  df <- tibble::tibble(Light = Light.vector, Datetime = Datetime.vector)

  # Count hours per day and missing data per hour
  hours_per_day <- df %>%
    dplyr::group_by(
      Day = lubridate::floor_date(Datetime, unit = "1 day") %>% lubridate::as_date(),
      Hour = lubridate::floor_date(Datetime, unit = "1 hour")
    ) %>%
    dplyr::summarise(is_missing = all(is.na(Light))) %>%
    dplyr::ungroup()

  N_hours <- hours_per_day %>%
    dplyr::group_by(Day) %>%
    dplyr::summarise(N = dplyr::n()) %>%
    dplyr::select(Day, N)

  # Warning if days are not full 24h
  if (any(N_hours$N < 24)) {
    warning(paste(
      "One or more days in the data do not consist of 24 h.",
      "Interdaily stability might not be meaningful for non-24 h days.",
      "These days contain less than 24 h:",
      paste(
        utils::capture.output(print(
          dplyr::filter(N_hours, N < 24) %>% dplyr::pull(Day)
        )),
        sep = "\n", collapse = "\n"
      ),
      "",
      sep = "\n"
    ))
  }

  # Warning if some hours are completely missing
  if (any(hours_per_day$is_missing)) {
    warning(paste(
      "Data contains some hours with only missing values",
      "These hours contain only missing values: ",
      paste(
        utils::capture.output(print(
          dplyr::filter(hours_per_day, is_missing) %>% dplyr::pull(Hour)
        )),
        sep = "\n", collapse = "\n"
      ),
      "",
      sep = "\n"
    ))
  }

  # Remove missing values
  if (na.rm) {
    df <- df %>% tidyr::drop_na(Light)
  }
  
  # Subtract 1 if `use.samplevar == TRUE`
  c = as.numeric(use.samplevar)

  # Hourly averages for each day
  hourly_data <- df %>%
    dplyr::group_by(Datetime = lubridate::floor_date(Datetime, unit = "1 hour")) %>%
    dplyr::summarise(Light = mean(Light))
  
  # N hourly data
  n <- length(hourly_data$Light)
  
  # Overall variance
  var_total <- sum((hourly_data$Light-mean(hourly_data$Light))^2) / (n-c)

  # Hourly average across all days
  avg_hourly <- hourly_data %>%
    dplyr::group_by(Hour = lubridate::hour(Datetime)) %>%
    dplyr::summarise(Light = mean(Light))
  
  # N per day
  p <- length(avg_hourly$Light)
  
  # Variance across average day
  var_avg_day <- sum((avg_hourly$Light-mean(hourly_data$Light))^2) / (p-c)

  # Variance across average day / variance across all days
  is <- var_avg_day / var_total

  # Return data frame or numeric vector
  if (as.df) {
    return(tibble::tibble("interdaily_stability" = is))
  } else {
    return(is)
  }
}

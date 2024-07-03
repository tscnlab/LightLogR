#' Intradaily variability (IV)
#'
#' This function calculates the variability of consecutive Light levels within
#' a 24h day. Calculated as the ratio of the variance of the differences between
#' consecutive Light levels to the total variance across the day. Calculated with
#' mean hourly Light levels. Higher values indicate more fragmentation.
#'
#' @param Light.vector Numeric vector containing the light data.
#' @param Datetime.vector Vector containing the time data. Must be POSIXct.
#' @param na.rm Logical. Should missing values be removed? Defaults to `FALSE`.
#' @param as.df Logical. Should the output be returned as a data frame? If `TRUE`, a data
#'    frame with a single column named `intradaily_variability` will be returned.
#'    Defaults to `FALSE`.
#'
#' @return Numeric value or dataframe with column 'IV'.
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
#'
#' set.seed(1)
#' N <- 24 * 2
#' # Calculate metric for two 24 h days with two measurements per hour
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", N * 2),
#'     Datetime = lubridate::as_datetime(0) + c(lubridate::minutes(seq(0, N * 60 - 30, 30))),
#'     MEDI = sample(1:1000, N * 2)
#'   )
#' dataset1 %>%
#'   dplyr::summarise(
#'     "Intradaily variability" = intradaily_variability(MEDI, Datetime)
#'   )
#'
intradaily_variability <- function(Light.vector,
                                   Datetime.vector,
                                   na.rm = FALSE,
                                   as.df = FALSE) {
  # Initial checks
  stopifnot(
    "`Light.vector` must be numeric!" = is.numeric(Light.vector),
    "`Datetime.vector` must be POSIXct!" = lubridate::is.POSIXct(Datetime.vector),
    "`Light.vector` and `Datetime.vector` must be same length!" = 
      length(Light.vector) == length(Datetime.vector),
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
      "Intradaily variability might not be meaningful for non-24 h days.",
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

  # Hourly averages for each day
  total_hourly <- df %>%
    dplyr::group_by(lubridate::floor_date(Datetime, unit = "1 hour")) %>%
    dplyr::summarise(Light = mean(Light))

  # Variance of consecutive hourly differences
  var_hourly_diff <-
    sum(diff(total_hourly$Light)^2) / (length(total_hourly$Light) - 1)

  # Variance of consecutive differences / variance across all days
  iv <- var_hourly_diff / stats::var(total_hourly$Light)

  # Return data frame or numeric vector
  if (as.df) {
    return(tibble::tibble("intradaily_variability" = iv))
  } else {
    return(iv)
  }
}

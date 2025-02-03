#' Calculate boundary times of the photoperiod
#'
#' @param coordinates A two element numeric vector representing the latitude and
#'   longitude of the location. *Important note: **Latitude** is the first
#'   element and **Longitude** is the second element.*
#' @param dates A date of format `Date`, or coercible to `Date` through
#'   `lubridate::as_date()`
#' @param tz Timezone of the data. Expects a `character`. You can look up the
#'   supported timezones with [OlsonNames()].
#' @param solarDep A numerical value representing the solar depression angle.
#'   This means a value of 6 equals -6 degrees above the horizon. Default is 6,
#'   equalling `Civil dawn/dusk`. Other common values are 12 degrees for
#'   `Nautical dawn/dusk`, 18 degrees for `Astronomical dawn/dusk`, and 0
#'   degrees for `Sunrise/Sunset`.
#'
#' @returns A `tibble` with the calculated times of dawn and dusk for the given
#'   location and date. The tibble will contain four columns:
#'  - `date` with the date of the calculation, stored as class `Date`
#'  - `tz` with the timezone of the output, stored as class `character`
#'  - `dawn` and `dusk` with the calculated datetimes, stored as class `POSIXct`
#' @export
#'
#' @examples

photoperiod <- function(coordinates, dates, tz, solarDep = 6) {
  
  # Initial Checks ----------------------------------------------------------
  
  #check if coordinates is a sensible vector
  stopifnot(
    "coordinates must be a numeric vector" = is.numeric(coordinates),
    "coordinates must have two elements" = length(coordinates) == 2,
    "none of the coordinates can be NA" = !anyNA(coordinates),
    "none of the coordinates can be NaN" = !anyNA(coordinates)
  )
  
  #check if date is a sensible date
  dates <- lubridate::as_date(dates)
  stopifnot(
    "date must not be NA or coerced to NA" = !is.na(dates),
    "date must be a date" = lubridate::is.Date(dates)
  )
  
  #check if tz is a sensible time zone
  stopifnot(
    "tz must be a character" = is.character(tz),
    "tz must be a valid time zone" = tz %in% OlsonNames()
  )
  
  
  # Function ----------------------------------------------------------
  
  c("dawn", "dusk") |>
    purrr::map(
      \(x) {
        suntools::crepuscule(
          crds = matrix(c(coordinates[2], coordinates[1]), nrow = 1),
          dateTime = as.POSIXct(dates, tz = tz),
          solarDep = solarDep,
          direction = x,
          POSIXct.out = TRUE
        ) |> 
          dplyr::select(time) |>
          dplyr::rename(!!x := time)
      }
     ) |>  
    purrr::list_cbind() |>
    dplyr::mutate(date = dates,
                  tz = tz,
                  .before = 1)
}

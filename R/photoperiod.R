#' Calculate boundary times of the photoperiod
#'
#' @param coordinates A two element numeric vector representing the latitude and
#'   longitude of the location. *Important note: **Latitude** is the first
#'   element and **Longitude** is the second element.*
#' @param dates A date of format `Date`, or coercible to `Date` through
#'   [lubridate::as_date()]
#' @param tz Timezone of the data. Expects a `character`. You can look up the
#'   supported timezones with [OlsonNames()].
#' @param solarDep A numerical value representing the solar depression angle
#'   between 90 and -90. This means a value of 6 equals **-6** degrees above the
#'   horizon. Default is 6, equalling `Civil dawn/dusk`. Other common values are
#'   12 degrees for `Nautical dawn/dusk`, 18 degrees for `Astronomical
#'   dawn/dusk`, and 0 degrees for `Sunrise/Sunset`. Note that the output
#'   columns will always be named `dawn` and `dusk`, regardless of the
#'   `solarDep` value.
#'
#' @returns A `tibble` with the calculated times of dawn and dusk for the given
#'   location and date. The tibble contains the following columns:
#'  - `date` with the date of the calculation, stored as class `Date`
#'  - `tz` with the timezone of the output, stored as class `character`
#'  - `lat` and `lon` with the latitude and longitude of the location, stored
#'    as class `numeric`
#'  - `solar.angle` with the negative solar depression angle, i.e. the sun 
#'    elevation above the horizon. stored as class `numeric`
#'  - `dawn` and `dusk` with the calculated datetimes, stored as class `POSIXct`
#' @export
#'
#' @examples
#'   coordinates <- c(47.018711, 12.34256)
#'   dates <- c("2023-06-01", "2025-08-23")
#'   tz <- "Europe/Zurich"
#'   #civil dawn/dusk
#'   photoperiod(coordinates, dates, tz)
#'   #sunrise/sunset
#'   photoperiod(coordinates, dates, tz, solarDep = 0)

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
    "tz must be a valid time zone" = tz %in% OlsonNames(),
    "tz must be a scalar" = length(tz) == 1
  )
  
  #check that solarDep is a sensible number
  stopifnot(
    "solarDep must be a number" = is.numeric(solarDep),
    "solarDep must not be NA" = !is.na(solarDep),
    "solarDep must be between 90 and -90" = solarDep <= 90 & solarDep >= -90
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
                  lat = coordinates[1],
                  lon = coordinates[2],
                  solar.angle = -solarDep,
                  .before = 1)
}

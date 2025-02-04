
# photoperiod -------------------------------------------------------------


#' Calculate photoperiod and boundary times
#'
#' A family of functions to extract and add photoperiod information.
#' [photoperiod()] creates a `tibble` with the calculated times of dawn and dusk
#' for the given location and date. The function is a convenience wrapper for
#' [suntools::crepuscule()] to calculate the times of dawn and dusk. By default,
#' civil dawn and dusk are calculated, but the function can be used to calculate
#' other times by changing the `solarDep` parameter (e.g., 0 for sunrise/sunset,
#' 12 for nautical, and 18 for astronomical).
#'
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
#' @returns [photoperiod()] returns a `tibble` with the calculated times of dawn
#'   and dusk for the given location and date, with the length equal to the
#'   `dates` input parameter . The tibble contains the following columns:
#'  - `date` with the date of the calculation, stored as class `Date`
#'  - `tz` with the timezone of the output, stored as class `character`
#'  - `lat` and `lon` with the latitude and longitude of the location, stored
#'   as class `numeric`
#'  - `solar.angle` with the negative solar depression angle, i.e. the sun
#'   elevation above the horizon. stored as class `numeric`
#'  - `dawn` and `dusk` with the calculated datetimes, stored as class `POSIXct`
#'  - `photoperiod` with the calculated photoperiod, stored as class `difftime`.
#'
#' @export
#'
#' @family photoperiod
#'
#' @examples
#'  #example für Tübingen, Germany
#'  coordinates <- c(48.521637, 9.057645)
#'  dates <- c("2023-06-01", "2025-08-23")
#'  tz <- "Europe/Berlin"
#'  
#'  #civil dawn/dusk
#'  photoperiod(coordinates, dates, tz)
#'  #sunrise/sunset
#'  photoperiod(coordinates, dates, tz, solarDep = 0)

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
                  .before = 1) |> 
    dplyr::mutate(photoperiod = dusk - dawn)
}


# extract_photoperiod ------------------------------------------------------


#' Calculate photoperiods and their boundary times for a light logger dataset
#'
#' Taking a light exposure dataset as input, [extract_photoperiod()] calculates
#' the photoperiods and their boundary times for each unique day in the dataset,
#' given a location and boundary condition (i.e., the solar depression angle).
#' Basically, this is a convenience wrapper for [photoperiod()] that takes a
#' light logger dataset and extracts unique dates and the time zone from the
#' dataset.
#'
#' @inheritParams photoperiod
#' @inheritParams cut_Datetime
#'
#' @returns [extract_photoperiod()] returns a `tibble` of the same structure as
#'   [photoperiod()], but with a length equal to the number of unique dates in
#'   the dataset.
#' @export
#' @family photoperiod
#' @rdname photoperiod
#'
#' @examples 
#'  #extract_photoperiod
#'  sample.data.environment |> 
#'    extract_photoperiod(coordinates)
#' 
extract_photoperiod <- function(dataset, 
                               coordinates, 
                               Datetime.colname = Datetime,
                               solarDep = 6) {
  
  
  # Initial Checks ----------------------------------------------------------
  
  Datetime.colname.defused <-
    rlang::enexpr(Datetime.colname) |>  rlang::as_string()

  stopifnot(
    "dataset is not a dataframe" = is.data.frame(dataset),
    "Datetime.colname must be part of the dataset" =
      Datetime.colname.defused %in% names(dataset),
    "Datetime.colname must be a Datetime" =
        lubridate::is.POSIXct(dataset[[Datetime.colname.defused]])
  )
  
  # Function ----------------------------------------------------------------

  #extract the information from the dataset
  tz_dataset <- lubridate::tz(dataset[[Datetime.colname.defused]])
  
  relevant_days <- 
    dataset |> 
    dplyr::pull({{ Datetime.colname }}) |> 
    lubridate::date() |> 
    unique()
  
  #calculate the photoperiods
  photoperiod(coordinates = coordinates,
              dates = relevant_days,
              tz = tz_dataset,
              solarDep = solarDep
              )
  }


# add_photoperiod ---------------------------------------------------------

#' Add photoperiod information to a light exposure dataset
#'
#' [add_photoperiod()] adds photoperiod information to a light logger dataset.
#' Beyond the photoperiod information, it will categorize the
#' `photoperiod.state` as `"day"` or `"night"`. If `overwrite` is set to `TRUE`,
#' the function will overwrite any columns with the same name.
#'
#' Please note that all functions of the `photoperiod` family work with one
#' coordinate pair at a time. If you have multiple locations (and multiple time
#' zones), you need to run the function for each location separately. We suggest
#' using a nested dataframe structure, and employ the `purrr` package to iterate
#' over the locations.
#'
#' @inheritParams extract_photoperiod
#' @param overwrite Logical scalar. If `TRUE`, the function will overwrite any
#'  columns with the same name. If `FALSE` (default), the function will stop if any of
#'  the columns already exist in the dataset.
#' @returns The input dataset with the added photoperiod information. The
#'   information is appended with the following columns: `dawn`, `dusk`,
#'   `photoperiod`, and `photoperiod.state`.
#' @export
#' @family photoperiod
#' @rdname photoperiod
#'
#' @examples
#'
#' #add_photoperiod
#' added_photoperiod <- 
#'  sample.data.environment |> 
#'  dplyr::mutate(Datetime = lubridate::force_tz(Datetime, tz)) |> 
#'  add_photoperiod(coordinates)
#'    
#' added_photoperiod |> head()
#' 
#' added_photoperiod |> 
#'   filter_Date(length = "3 days") |>  
#'   gg_days(aes_col = photoperiod.state,
#'           group = dplyr::consecutive_id(photoperiod.state), 
#'           jco_color = TRUE 
#'           )
#' 
#' added_photoperiod |> 
#'   filter_Date(length = "3 days") |>
#'   gg_day(aes_col = Id) +
#'   ggplot2:: geom_rect(
#'   data = \(x) x |> dplyr::ungroup(Id) |> dplyr::summarize(dawn = mean(dawn) |> hms::as_hms()),
#'   ggplot2::aes(xmin = 0, xmax = dawn, ymin = -Inf, ymax = Inf),
#'   alpha = 0.1
#'   ) +
#'   ggplot2:: geom_rect(
#'   data = \(x) x |> dplyr::ungroup(Id) |> dplyr::summarize(dusk = mean(dusk) |> hms::as_hms()),
#'   ggplot2::aes(xmin = dusk, xmax = 24*60*60, ymin = -Inf, ymax = Inf),
#'   alpha = 0.1
#'   )
#'    
#'    
#'  added_photoperiod |> dplyr::summarize(dawn = mean(dawn) |> hms::as_hms())
add_photoperiod <- function(dataset,
                               coordinates,
                               Datetime.colname = Datetime,
                               solarDep = 6,
                               overwrite = FALSE) {
  
  # Initial Checks ----------------------------------------------------------
  
  Datetime.colname <-
    rlang::enexpr(Datetime.colname)
  
  new_names <- c("dusk", "dawn", "photoperiod", "photoperiod.state")
  
  if(!overwrite) {
    stopifnot(
      "dataset is not a dataframe" = is.data.frame(dataset),
      "existing columns would be overwritten, consider setting `overwrite = TRUE`" = 
        !any(new_names %in% names(dataset))
      )
  }
  
  if(overwrite) {
    dataset <- 
      dataset |> dplyr::select(-dplyr::any_of(new_names))
  }
  
  # Function ----------------------------------------------------------------
  
  #extract the photoperiods
  extracted_photoperiods <- 
    extract_photoperiod(dataset, coordinates, !!Datetime.colname, solarDep)

  extracted_photoperiods <- 
    extracted_photoperiods |> dplyr::select(date, dawn, dusk, photoperiod)
  
  #set a date column in the dataset
  dataset <- 
    dataset |> 
    dplyr::mutate(
      date.for.photoperiod = lubridate::date({{ Datetime.colname }})
      )
  
  #join the two datasets
  dataset <- 
    dataset |>  
    dplyr::left_join(extracted_photoperiods, 
                     by = c("date.for.photoperiod" = "date"))

  #add the photoperiod state
  dataset <-
    dataset |>
    dplyr::group_by(date.for.photoperiod, .add = TRUE) %>%
    dplyr::mutate(
      photoperiod.state =
        dplyr::case_when(
          !!Datetime.colname < dawn ~ "night",
          !!Datetime.colname < dusk ~ "day",
          !!Datetime.colname >= dusk ~ "night"
        )
    ) |> 
    dplyr::ungroup(date.for.photoperiod)
  
  #remove the temporary date column
  dataset <- 
    dataset |> 
    dplyr::select(-date.for.photoperiod)
  
  #return
  dataset
}


# solar_noon --------------------------------------------------------------

#' Calculate solar noon
#' 
#' [solar_noon()] calculates the solar noon for a given location and date. The
#' function is a convenience wrapper for [suntools::solarnoon()]. The function
#' has no companians like [extract_photoperiod()] or [add_photoperiod()], but
#' will be extended, if there is sufficient interest.
#'
#' @inheritParams photoperiod
#'
#' @returns [solar_noon()] returns a `tibble` with the calculated solar noon
#' @export
#'
#' @family photoperiod
#' @rdname photoperiod
#'
#' @examples
#' 
#'  #solar_noon()
#'  solar_noon(coordinates, dates, tz)
#' 
solar_noon <- function(coordinates, dates, tz) {
  
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
  
  # Function ----------------------------------------------------------
  
    suntools::solarnoon(
          crds = matrix(c(coordinates[2], coordinates[1]), nrow = 1),
          dateTime = as.POSIXct(dates, tz = tz),
          POSIXct.out = TRUE
        ) |> 
          dplyr::select(time) |>
          dplyr::rename(solar.noon = time
    ) |>  
    dplyr::mutate(date = dates,
                  tz = tz,
                  lat = coordinates[1],
                  lon = coordinates[2],
                  .before = 1)
}


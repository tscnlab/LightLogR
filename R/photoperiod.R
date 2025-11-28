
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
  dates2 <- lubridate::as_date(dates)
  stopifnot(
    "date must not be NA or coerced to NA" = !is.na(dates2),
    "date must be a date" = lubridate::is.Date(dates2)
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
          dateTime = as.POSIXct(dates |> as.character(), tz = tz),
          solarDep = solarDep,
          direction = x,
          POSIXct.out = TRUE
        ) |> 
          dplyr::select(time) |>
          dplyr::rename(!!x := time)
      }
     ) |>  
    purrr::list_cbind() |>
    dplyr::mutate(date = dates |> lubridate::as_date(),
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
   colname.defused({{ Datetime.colname }})

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
#'   columns with the same name. If `FALSE` (default), the function will stop if
#'   any of the columns already exist in the dataset.
#' @returns [add_photoperiod] returns the input dataset with the added
#'   photoperiod information. The information is appended with the following
#'   columns: `dawn`, `dusk`, `photoperiod`, and `photoperiod.state`.
#' @export
#' @rdname photoperiod
#'
#' @examples
#'
#' #add_photoperiod
#' added_photoperiod <-
#'  sample.data.environment |>
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
#' has no companions like [extract_photoperiod()] or [add_photoperiod()], but
#' will be extended, if there is sufficient interest.
#'
#' @inheritParams photoperiod
#'
#' @returns [solar_noon()] returns a `tibble` with the calculated solar noon
#' @export
#'
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

# gg_photoperiod --------------------------------------------------------------

#' Add photoperiods to gg_day() or gg_days() plots
#'
#' [gg_photoperiod()] is a helper function to add photoperiod information to
#' plots generated with [gg_day()] or [gg_days()]. The function can either draw
#' on the `dawn` and `dusk` columns of the dataset or use the `coordinates` and
#' `solarDep` arguments to calculate the photoperiods.
#'
#' If used in combination with [gg_doubleplot()], with that function in the
#' `type = "repeat"` setting (either manually set, or because there is only one
#' day of data per group present), photoperiods need to be added separately
#' through [add_photoperiod()], or the second photoperiod in each panel will be
#' off by one day. See the examples for more information.
#'
#' In general, if the photoperiod setup is more complex, it makes sense to add
#' it prior to plotting and make sure the photoperiods are correct.
#'
#' @inheritParams photoperiod
#' @param ggplot_obj A `ggplot` object generated with [gg_day()] or [gg_days()]
#'   (or [gg_doubleplot()].
#' @param coordinates A two element numeric vector representing the latitude and
#'   longitude of the location. If `NULL`, the default, the function will look
#'   for the `dawn` and `dusk` columns in the dataset. If those are not present,
#'   (and in the `POSIXct` format), the function will stop with an error.
#'   Further, if `NULL`, the `solarDep` argument will be ignored.
#' @param alpha A numerical value between 0 and 1 representing the transparency
#'   of the photoperiods. Default is 0.2.
#' @param ... Additional arguments given to the [ggplot2::geom_rect()] used to
#'   construct the photoperiod shading. Can be used to change the fill color or
#'   other aesthetic properties.
#' @param on.top Logical scalar. If `TRUE`, the photoperiods will be plotted on
#'   top of the existing plot. If `FALSE`, the photoperiods will be plotted
#'   underneath the existing plot. Default is `FALSE`.
#' @param ymin,ymax customize the height of the photoperiod rectangle. By
#'   default it will cover the whole vertical range (-Inf, Inf), but can be set
#'   to any value. If it is important to set the height conditionally, using
#'   [gg_states()] is recommended.
#' @param by.group Logical that indicates whether the photoperiod to display is
#'   calculated within the groups of the dataset. By default this is`TRUE` for a
#'   `POSIXct` axis ([gg_days()]) and `FALSE` for a `hms` axis ([gg_day()]). If
#'   provided as a length-2 vector, the first logical will be used for `POSIXct`
#'   and the second for `hms`. If a scalar is provided, it will be used for both
#'   conditions.
#' @param Datetime.colname Column name in the underlying dataset that contains
#'   the datetime. Defaults to `Datetime`. Is used to calculate photoperiod (if
#'   missing), and for grouping (only `POSIXct` axes).
#'
#' @returns a modified `ggplot` object with the photoperiods added.
#' @export
#' @family photoperiod
#'
#' @examples
#' coordinates <- c(48.521637, 9.057645)
#' #adding photoperiods to a ggplot
#' sample.data.environment |>
#'   gg_days() |>
#'   gg_photoperiod(coordinates)
#'
#' #adding photoperiods prior to plotting
#' sample.data.environment |>
#'   add_photoperiod(coordinates, solarDep = 0) |>
#'   gg_days() |>
#'   gg_photoperiod()
#'
#' #more examples that are not executed for computation time:
#' \donttest{
#' #plotting photoperiods automatically works for both gg_day() and gg_days()
#' sample.data.environment |>
#'   gg_day() |>
#'   gg_photoperiod(coordinates)
#'
#' #plotting for gg_doubleplot mostly works fine
#' sample.data.environment |>
#'   filter_Date(length = "2 days") |>
#'   gg_doubleplot() |>
#'   gg_photoperiod(coordinates)
#'
#' #however, in cases where only one day of data per group is available, or the
#' #type = "repeat" setting is used, the photoperiods need to be added
#' #separately. Otherwise the second day will be off by one day in each panel.
#' #The visual difference is subtle, and might not be visible at all, as
#' #photoperiod only every changes by few minutes per day.
#'
#' #WRONG
#' sample.data.environment |>
#'   filter_Date(length = "1 days") |>
#'   gg_doubleplot() |>
#'   gg_photoperiod(coordinates)
#'
#' #CORRECT
#' sample.data.environment |>
#'   filter_Date(length = "1 days") |>
#'   add_photoperiod(coordinates) |>
#'   gg_doubleplot() |>
#'   gg_photoperiod()
#'   }

gg_photoperiod <- function(ggplot_obj, 
                           coordinates = NULL, 
                           ymin = -Inf,
                           ymax = Inf,
                           alpha = 0.2,
                           solarDep = 6,
                           on.top = FALSE,
                           Datetime.colname = Datetime,
                           by.group = c(TRUE, FALSE),
                           ...) {
  
  # Initial Checks ----------------------------------------------------------
  
  Datetime.colname <-
    rlang::enexpr(Datetime.colname)
  
  #ggplot must be a ggplot object
  stopifnot(
    "ggplot_obj must be a ggplot object" = inherits(ggplot_obj, "gg")
  )
  
  #coordinates must either be a length two numerical vector without NA/NaN, 
  #or NULL
  if(!is.null(coordinates)) {
    stopifnot(
      "coordinates must be a numeric vector" = is.numeric(coordinates),
      "coordinates must have two elements" = length(coordinates) == 2,
      "none of the coordinates can be NA" = !anyNA(coordinates),
      "none of the coordinates can be NaN" = !anyNA(coordinates)
    )
  }
  
  #solarDep must be a number between 90 and -90
  stopifnot(
    "solarDep must be a number" = is.numeric(solarDep),
    "solarDep must not be NA" = !is.na(solarDep),
    "solarDep must be between 90 and -90" = solarDep <= 90 & solarDep >= -90,
    "solarDep must be a scalar" = is.all.scalar(solarDep)
  )
  
  #by.group must be logical
  stopifnot(
    "by.group must be a length 1 or 2 logical" = is.logical(by.group) & (length(by.group) %in% 1:2)
  )
  
  #if coordinates is NULL, the ggplot data must have dawn and dusk columns
  if(is.null(coordinates)) {
    stopifnot(
      "dawn column must be present in the ggplot data" = 
        "dawn" %in% names(ggplot_obj$data),
      "dusk column must be present in the ggplot data" = 
        "dusk" %in% names(ggplot_obj$data),
      "dawn column must be a POSIXct" = 
        lubridate::is.POSIXct(ggplot_obj$data$dawn),
      "dusk column must be a POSIXct" = 
        lubridate::is.POSIXct(ggplot_obj$data$dusk)
    )
  }
  
  #determine whether the ggplot has a hms or a POSIXct x-axis
  x_axis_type <- 
    get_ggplot_axis_type(ggplot_obj, "x")
  
  #if by.group is a scalar, make a length-2-vector out of it
  if(rlang::is_scalar_logical(by.group)){
    by.group[2] <- by.group
  }
  
  # Function ----------------------------------------------------------
  
  #add photoperiods to the data if coordinates are provided
  if(!is.null(coordinates)) {
    #calculate the photoperiods
    ggplot_obj$data <- 
      ggplot_obj$data |> 
      add_photoperiod(coordinates, 
                      solarDep = solarDep, 
                      overwrite = TRUE,
                      Datetime.colname = !!Datetime.colname)
  }
  
  #if the y_axis_type is time
  if(x_axis_type == "time") {
    #create a table of photoperiods, by date
    photoperiod_data <-
      ggplot_obj$data |>
      dplyr::group_by(date.grouper = lubridate::date(!!Datetime.colname), 
                      .add = by.group[1]) |>
      dplyr::summarize(dawn = mean(dawn, na.rm = TRUE),
                       dusk = mean(dusk, na.rm = TRUE))

    # add midnight starts and ends. If dusk is before dawn (on a 24-hour scale),
    # then only create a single rectangle
    photoperiod_data <-
      photoperiod_data |>
      dplyr::mutate(
        midnight.before = dplyr::case_when(
          !is.na(dplyr::lag(dawn)) ~ dawn,
          .default = 
            lubridate::floor_date(dawn, "day")
        ),
        midnight.after = dplyr::case_when(
          !is.na(dplyr::lead(dawn)) ~ dplyr::lead(dawn),
          .default = 
            lubridate::ceiling_date(date.grouper, "day")
        )
      ) |> 
      dplyr::mutate(
        dawn = dplyr::case_when(min(date.grouper) > lubridate::date(dawn) ~ NA,
                                .default = dawn),
        dusk = dplyr::case_when(max(date.grouper) < lubridate::date(dusk) ~ NA,
                                .default = dusk),
        midnight.before = dplyr::case_when(min(date.grouper) > 
                                             lubridate::date(midnight.before) ~ NA,
                                .default = midnight.before),
      )
  }
  
  #if the x_axis_type is hms
  if(x_axis_type == "hms") {
    
    photoperiod_data <- 
      ggplot_obj$data |> 
      dplyr::group_by(Day.data, .add = by.group[2]) |>
      dplyr::summarize(dawn = mean(dawn, na.rm = TRUE),
                       dusk = mean(dusk, na.rm = TRUE),
                       midnight.before = lubridate::floor_date(dawn, "day"),
                       midnight.after = lubridate::ceiling_date(dusk, "day")
      )
    
    photoperiod_data <- 
      photoperiod_data |> 
      dplyr::mutate(
        dawn = dawn |> hms::as_hms(),
        dusk = dusk |> hms::as_hms(),
        midnight.before = ifelse(dusk < dawn, dawn, 0),
        midnight.after = ifelse(dusk < dawn, dawn, 24*3600)
      )
  }
  
  #create the geoms for the photoperiods
  photoperiod_geoms <- 
    list(
      ggplot2::geom_rect(
        inherit.aes = FALSE,
        data = photoperiod_data |> tidyr::drop_na(dawn, midnight.before),
        ggplot2::aes(
          xmin = midnight.before,
          xmax = dawn,
          ymin = {{ ymin }},
          ymax = {{ ymax }}, 
        ),
        alpha = alpha,
        ...
      ),
      ggplot2::geom_rect(
        inherit.aes = FALSE,
        data = photoperiod_data |> tidyr::drop_na(dusk, midnight.after),
        ggplot2::aes(
          xmin = dusk,
          xmax = midnight.after,
          ymin = {{ ymin }},
          ymax = {{ ymax }},
        ),
        alpha = alpha,
        ...
      )
    )
  
  #add the geoms to the ggplot
  new_plot_obj <- ggplot_obj + photoperiod_geoms

  if(on.top) {
    return(new_plot_obj)
  }
  
  #reorder the layers so that the new geoms are at the very bottom
  new_plot_obj$layers <-
    c(new_plot_obj$layers |> utils::tail(2), 
      new_plot_obj$layers |> utils::head(-2))

  #return
  new_plot_obj
  
}

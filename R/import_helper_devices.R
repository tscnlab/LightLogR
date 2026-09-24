# Shared reader for ActLumus and ActLumus Plus exports. The first file defines
# the table layout for the batch. Explicit col_types override the defaults.
read_actlumus <- function(filename, tz, n_max, locale, ...) {
  dots <- rlang::list2(...)

  rows_to_skip <- detect_starting_row(
    filename[1],
    locale = locale,
    column_names = "DATE/TIME",
    n_max = 250
  )

  if (!"col_types" %in% names(dots)) {
    header_dots <- dots
    header_dots$col_types <- readr::cols(.default = readr::col_character())
    header <- suppressMessages(
      rlang::inject(readr::read_delim(
        filename[1],
        skip = rows_to_skip,
        delim = ";",
        n_max = 0,
        id = "file.name",
        locale = locale,
        name_repair = "universal",
        !!!header_dots
      ))
    )

    # Explicit types protect sparse descriptions and empty measurement columns.
    # Additional export columns retain readr's usual type inference.
    col_types <- readr::cols(
      .default = readr::col_guess(),
      DATE.TIME = "c",
      SLEEP.EVENT.DESC = "c",
      ORIENTATION.DESC = "c",
      STATE.DESC = "c",
      MODEL = "c",
      MS = "d",
      EVENT = "d",
      SLEEP.EVENT = "d",
      TEMPERATURE = "d",
      EXT.TEMPERATURE = "d",
      ORIENTATION = "d",
      PIM = "d",
      PIMn = "d",
      TAT = "d",
      TATn = "d",
      ZCM = "d",
      ZCMn = "d",
      LIGHT = "d",
      AMB.LIGHT = "d",
      RED.LIGHT = "d",
      GREEN.LIGHT = "d",
      BLUE.LIGHT = "d",
      IR.LIGHT = "d",
      UVA.LIGHT = "d",
      UVB.LIGHT = "d",
      CAP_SENS_1 = "d",
      CAP_SENS_2 = "d",
      F1 = "d",
      F2 = "d",
      F3 = "d",
      F4 = "d",
      F5 = "d",
      F6 = "d",
      F7 = "d",
      F8 = "d",
      MELANOPIC.EDI = "d",
      CLEAR = "d",
      S.CONE.OPIC.EDI = "d",
      M.CONE.OPIC.EDI = "d",
      L.CONE.OPIC.EDI = "d",
      RHODOPIC.EDI = "d",
      Z.LIGHT = "d",
      Y.LIGHT = "d",
      X.LIGHT = "d",
      FD.LIGHT = "d",
      CLA2 = "d",
      CS = "d",
      INITIAL.STATE = "d",
      STATE = "d"
    )
    col_types$cols <- col_types$cols[names(col_types$cols) %in% names(header)]
    dots$col_types <- col_types
  }

  data <- suppressMessages(
    rlang::inject(readr::read_delim(
      filename,
      skip = rows_to_skip,
      delim = ";",
      n_max = n_max,
      id = "file.name",
      locale = locale,
      name_repair = "universal",
      !!!dots
    ))
  )

  data |>
    dplyr::rename(Datetime = "DATE.TIME", MEDI = "MELANOPIC.EDI") |>
    dplyr::mutate(Datetime = lubridate::dmy_hms(.data$Datetime, tz = tz))
}

# Shared reader for LYS Button and LYS Button PRO exports.
read_lys <- function(filename, tz, n_max, locale, ...) {
  dots <- rlang::list2(...)
  if (!"na" %in% names(dots)) {
    dots$na <- c("", "NA", "None")
  }

  if (!"col_types" %in% names(dots)) {
    header_dots <- dots
    header_dots$col_types <- readr::cols(.default = readr::col_character())
    header <- suppressMessages(
      rlang::inject(readr::read_csv(
        filename[1],
        n_max = 0,
        id = "file.name",
        locale = locale,
        name_repair = "universal",
        !!!header_dots
      ))
    )

    col_types <- readr::cols(
      .default = readr::col_guess(),
      sensor = "f",
      Email = "c",
      lux = "d",
      kelvin = "d",
      rgbR = "d",
      rgbG = "d",
      rgbB = "d",
      rgbIR = "d",
      movement = "d",
      mEDI = "d",
      R. = "d",
      G. = "d",
      B. = "d",
      Clear = "d",
      F1 = "d",
      F2 = "d",
      F3 = "d",
      F4 = "d",
      F5 = "d",
      F6 = "d",
      F7 = "d",
      F8 = "d",
      NIR = "d",
      Flicker = "d",
      Movement = "d",
      Lux = "d",
      CCT = "d"
    )
    timestamp_columns <- grep(
      "^timestamp",
      names(header),
      ignore.case = TRUE,
      value = TRUE
    )
    for (column in timestamp_columns) {
      col_types$cols[[column]] <- readr::col_character()
    }
    col_types$cols <- col_types$cols[names(col_types$cols) %in% names(header)]
    dots$col_types <- col_types
  }

  data <- suppressMessages(
    rlang::inject(readr::read_csv(
      filename,
      n_max = n_max,
      id = "file.name",
      locale = locale,
      name_repair = "universal",
      !!!dots
    ))
  )

  timestamp_columns <- grep(
    "^timestamp",
    names(data),
    ignore.case = TRUE,
    value = TRUE
  )
  if (length(timestamp_columns) != 1L) {
    stop(
      "LYS exports must contain exactly one timestamp column whose name starts with `timestamp`.",
      call. = FALSE
    )
  }
  data <- data |>
    dplyr::rename(Datetime = dplyr::all_of(timestamp_columns), MEDI = "mEDI")

  datetime <- data$Datetime
  if (!inherits(datetime, "POSIXt")) {
    timestamps <- as.character(datetime)
    # Select the date order explicitly to avoid guessing legacy dates as years.
    year_first <- grepl("^\\s*[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}", timestamps)
    datetime <- lubridate::as_datetime(
      rep(NA_real_, length(timestamps)),
      tz = "UTC"
    )
    datetime[year_first] <- lubridate::parse_date_time(
      timestamps[year_first],
      orders = c("Ymd HMOSz", "Ymd HMSz", "Ymd HMOS", "Ymd HMS"),
      tz = "UTC"
    )
    datetime[!year_first] <- lubridate::dmy_hms(
      timestamps[!year_first],
      tz = "UTC"
    )
  }

  data |>
    dplyr::mutate(Datetime = lubridate::with_tz(datetime, tzone = tz))
}

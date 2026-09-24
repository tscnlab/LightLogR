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

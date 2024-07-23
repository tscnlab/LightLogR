#this file contains all the import expressions for the different devices
#they are to be used with the `imports` or `import_adjustment` function to create a proper import function

ll_import_expr <- list(
  #ActTrust 1 & 2
  ActTrust = rlang::expr({
    column_names <- c("DATE/TIME", "MS", "EVENT", "TEMPERATURE")
    data <- 
      purrr::map(
        filename,
        \(x) {
          rows_to_skip <- detect_starting_row(x, 
                                              locale = locale, 
                                              column_names = column_names,
                                              n_max = 1000)
          suppressMessages(
            readr::read_delim(
              x,
              skip = rows_to_skip,
              delim = ";",
              n_max = n_max,
              col_types = paste0("c", paste0(rep("d", 20), collapse = "")),
              id = "file.name",
              locale = locale,
              name_repair = "universal",
              ...
            )
          )
        }) %>% purrr::list_rbind()
    data <-
      data %>%
      dplyr::rename(Datetime = DATE.TIME) %>%
      dplyr::mutate(Datetime = Datetime %>% 
                      lubridate::dmy_hms(tz = tz, quiet = TRUE))
  }),
  #SpectraWear
  SpectraWear = rlang::expr({
    data <-suppressMessages( 
      readr::read_csv(filename,
                      n_max = n_max,
                      id = "file.name",
                      locale = locale,
                      name_repair = "universal",
                      ...
      ))
    data <- data %>%
      dplyr::rename(MEDI = Mel
      ) %>% 
      dplyr::mutate(Datetime =
                      lubridate::dmy_hms(paste(Date, Time), tz = tz),
                    Id = paste(.data$id, .data$ls, sep = ".")
      )
  }),
  #Speccy
  Speccy = rlang::expr({
    data <-suppressMessages( 
      readr::read_csv(filename,
                      n_max = n_max,
                      id = "file.name",
                      locale = locale,
                      name_repair = "universal",
                      col_types = c("fccdddddddddddddddddd"),
                      ...
      ))
    data <- data %>%
      dplyr::rename(MEDI = Melanopic.EDI) %>% 
      dplyr::mutate(Datetime =
                      Datetime %>% lubridate::parse_date_time(
                        orders =  c("%H:%M:%S %d/%m/%y", "%d/%m/%Y %H:%M"),
                        tz = tz, exact = TRUE))
  }),
  #Intelligent Automation Inc DeLux
  DeLux = rlang::expr({
    data <-suppressMessages( 
      readr::read_csv(filename,
                      n_max = n_max,
                      id = "file.name",
                      locale = locale,
                      name_repair = "universal",
                      col_types = c("fccdddddddddddddddddd"),
                      ...
      ))
    data <- data %>%
      dplyr::rename(Datetime = Timestamp) %>%
      dplyr::mutate(Datetime =
                      Datetime %>% lubridate::ymd_hms(tz = tz))
  }),
  #LiDo
  LiDo = rlang::expr({
    data <- suppressMessages(
      readr::read_delim(
        filename,
        delim = ";",
        n_max = n_max,
        id = "file.name",
        locale = locale,
        name_repair = "universal",
        ...
      )
    )
    data <- data %>%
      dplyr::rename(Datetime = UTC.Timestamp,
                    MEDI = Ev_mel_D65.in.lx) %>% 
      dplyr::mutate(Datetime = 
                      Datetime %>% 
                      lubridate::dmy_hms() %>% 
                      lubridate::with_tz(tzone = tz))
  }),
  #ActLumus
  ActLumus = rlang::expr({
    data <- suppressMessages( 
      readr::read_delim(
        filename,
        skip = 32,
        delim = ";",
        n_max = n_max,
        col_types = paste0("c", paste0(rep("d", 32), collapse = "")),
        id = "file.name",
        locale = locale,
        name_repair = "universal",
        ...
      ))
    data <- data %>%
      dplyr::rename(Datetime = DATE.TIME,
                    MEDI = MELANOPIC.EDI) %>%
      dplyr::mutate(Datetime =
                      Datetime %>% lubridate::dmy_hms(tz = tz))
  }),
  #LYS
  LYS = rlang::expr({
    data <-suppressMessages( 
      readr::read_csv(filename,
                      n_max = n_max,
                      col_types = c("cfddddddddddd"),
                      id = "file.name",
                      locale = locale,
                      name_repair = "universal",
                      ...
      ))
    data <- data %>%
      dplyr::rename(Datetime = timestamp,
                    MEDI = mEDI) %>%
      dplyr::mutate(Datetime =
                      Datetime %>% lubridate::dmy_hms() %>% 
                      lubridate::with_tz(tzone = tz))
  }),
  #LightWatcher
  LightWatcher = rlang::expr({
    column_names <- c("Date", "Time", "Light_Lux", "Light_IR")
    locale$decimal_mark <- ","
    data <- 
      purrr::map(
        filename,
        \(x) {
          rows_to_skip <- detect_starting_row(x, 
                                              locale = locale, 
                                              column_names = column_names,
                                              n_max = 1000)
          dat <- 
            suppressMessages(
            readr::read_delim(
              x,
              skip = rows_to_skip,
              delim = "\t",
              n_max = n_max,
              col_types = 
                paste0(c("icc"), paste0(rep("d", 22), collapse = ""), "-"),
              id = "file.name",
              locale = locale,
              name_repair = "universal",
              na = c("", "NA", "-----"),
              ...
            )
          )
          dat
          
        }) %>% purrr::list_rbind()
    data <-
      data %>%
      dplyr::mutate(Time = Time %>% stringr::str_replace_all(",","."),
                    Datetime = paste(Date, Time) %>% 
                      lubridate::ymd_hms(tz = tz, quiet = TRUE),.before = Nr)
  }),
  #Actiwatch Spectrum
  Actiwatch_Spectrum = rlang::expr({
    #separate the dots list in the column_names and the rest
    dots <- rlang::list2(...)
    column_names <- dots$column_names
    if(is.null(column_names)) 
      stop("Actiwatch Spectrum requires a vector of `column_names` in the order in which they appear in the file in order to properly detect the starting row")
    dots$column_names <- NULL
    
    data <- 
      purrr::map(
        filename,
        \(x) {
          rows_to_skip <- detect_starting_row(x, 
                                              locale = locale, 
                                              column_names = column_names,
                                              n_max = 1000)
          df <- suppressMessages(do.call(
            readr::read_csv,
            append(list(
              x, 
              skip = rows_to_skip,
              locale=locale,
              id = "file.name",
              show_col_types = FALSE,
              col_types = c("iDtfdfccccfdf"),
              name_repair = "universal"
            ),
            dots)))
          
          df %>% 
            dplyr::select(!dplyr::starts_with("..."))
          
        }) %>% purrr::list_rbind()
    data <- data %>%
      tidyr::unite(col = "Datetime",
                   tidyselect::where(lubridate::is.Date),
                   tidyselect::where(hms::is_hms),
                   remove = FALSE
      ) %>% 
      dplyr::mutate(
        Datetime = lubridate::ymd_hms(Datetime),
        dplyr::across(
          dplyr::where(is.character) &
            dplyr::where(~ any(stringr::str_detect(.x, ","), na.rm = TRUE)),
          ~ stringr::str_replace(.x, ",", ".") %>%
            as.numeric()
        )
      )
  }),
  #nanoLambda
  nanoLambda = rlang::expr({

    #create a helper function for nanoLambda
    process_chunk <- function(chunk) {
      chunk_data <- 
        purrr::map(chunk, ~ stringr::str_split(.x, ", ", simplify = TRUE))
      chunk_data <- chunk_data[-length(chunk_data)]
      col_names <- chunk_data %>% purrr::map(\(x) x[[1]])
      col_values <- chunk_data %>% purrr::map(\(x) x[-1])
      names(col_values) <- col_names
      col_values %>% 
        purrr::map(~ if(length(.x) == 1) .x else list(as.numeric(.x))) %>% 
        tibble::as_tibble()
    }
    
    #read in the data
    data <- 
      purrr::map(
        filename,
        \(x) {
          lines <- readr::read_lines(filename)
          measurements <- sum(stringr::str_detect(lines,"File Name"))
          chunks <- 
            if(measurements == 1) {
              list(lines)
            } else split(
              lines, 
              ceiling(seq_along(lines)/(length(lines)/measurements)))
          purrr::map(chunks, process_chunk) %>% 
            purrr::list_rbind() %>% 
            dplyr::rename_with(~ stringr::str_replace(., " ", "_")) %>% 
            dplyr::rename(Datetime = Modify_Time) %>% 
            dplyr::mutate(
              Datetime = lubridate::ymd_hms(Datetime, tz = tz, quiet = TRUE),
              dplyr::across(
                            where(
                              is.character) & 
                              !tidyselect::any_of(
                                c("File_Name", "Device_Name", "Device_Address",
                                  "Sensor_ID", "Is_Saturate")), 
                            as.numeric),
                          file.name = filename
            )
        }) %>% purrr::list_rbind()
  }),
  Veet = rlang::expr({
    #separate the dots list in the column_names and the rest
    dots <- rlang::list2(...)
    modality <- dots$modality
    dots$modality <- NULL
    stopifnot(modality %in% c("ALS", "IMU", "INF", "PHO", "TOF"))
    veet_names <- list(
      ALS = c(time_stamp = TRUE, modality = FALSE, integration_time = TRUE, 
              uvGain = TRUE, visGain = TRUE, irGain = TRUE, uvValue = TRUE, 
              visValue = TRUE, irValue = TRUE, Flicker = TRUE, Lux = TRUE),
      IMU = c(time_stamp = TRUE, modality = FALSE, ax = TRUE, ay = TRUE, 
              az = TRUE, gx = TRUE, gy = TRUE, gz = TRUE, temp = TRUE),
      INF = c(time_stamp = TRUE, modality = FALSE, product_name = FALSE, 
              serial_number = FALSE, fw_version = FALSE, Researcher_ID = FALSE, 
              Participant_ID = FALSE, Time_Zone_offset = TRUE, IMU_interval = TRUE, 
              PHO_interval = TRUE, TOF_interval = TRUE, ALS_interval = TRUE, 
              Temple_Config = FALSE, TOF_Iterations = TRUE, 
              IMU_Cal_Table = TRUE, PHO_Cal_Table = TRUE, 
              ToF_Cal_Table = TRUE, ALS_Cal_Table = TRUE, 
              Unit_Timestamp = FALSE, Unit_Batt_Voltage = FALSE, 
              Unit_Sensor_Interval = FALSE, Unit_IMU_Accel = FALSE, 
              Unit_IMU_Gyro = FALSE, Unit_Pho_Cts = FALSE, Unit_Pho_Gain = FALSE, 
              Unit_ToF = FALSE, Unit_ALS_Cts = FALSE, Unit_ALS_Gain = FALSE, 
              Unit_ALS_Flicker = FALSE),
      PHO = c(time_stamp = TRUE, modality = FALSE, integration_time = TRUE, 
              Gain = TRUE, s415 = TRUE, s445 = TRUE, s480 = TRUE, s515 = TRUE, 
              s555 = TRUE, s590 = TRUE, s630 = TRUE, s680 = TRUE, s940 = TRUE, 
              Dark = TRUE, ClearL = TRUE, ClearR = TRUE),
      TOF = stats::setNames(
        c(TRUE, FALSE, rep(TRUE, 4 * 64)), 
          c("time_stamp", "modality", 
            paste0("conf1_", 0:63), 
            paste0("conf2_", 0:63), 
            paste0("dist1_", 0:63), 
            paste0("dist2_", 0:63))
        )
    )
    data <- 
      purrr::map(filename, \(filename) {
        pattern <- paste0("^(?:[^,]*,){1}\\b", modality, "\\b")
        data <- 
          readr::read_lines(file = filename, locale = locale, n_max = n_max)
        data <- data[data %>% stringr::str_detect(pattern)]
        data <- stringr::str_split(data, ",") %>% 
          purrr::list_transpose() %>% list2DF()
        names(data) <- names(veet_names[[modality]])
        data <- data %>% 
          dplyr::mutate(file.name = filename, .before = 1)
        data
      }) %>% purrr::list_rbind()
    data <- data %>% 
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(
            veet_names[[modality]][veet_names[[modality]]] %>% names()), 
        as.numeric),
        Datetime = lubridate::with_tz(
          lubridate::as_datetime(time_stamp, tz = "UTC"), tz), .before = 1
      )
  }
  )
)

#order the list by their names
ll_import_expr <- ll_import_expr[order(names(ll_import_expr))]

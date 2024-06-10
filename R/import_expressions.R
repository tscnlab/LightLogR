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
  })
  
  
)

#order the list by their names
ll_import_expr <- ll_import_expr[order(names(ll_import_expr))]

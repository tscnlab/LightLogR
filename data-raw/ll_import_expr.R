## code to prepare `ll_import_expr` dataset goes here
#this list contains the expressions to import data for each device

ll_import_expr <- list(
  #SpectraWear
  SpectraWear = rlang::expr({
    tmp <-suppressMessages( 
      readr::read_csv(filename,
                      n_max = n_max,
                      id = "file.name",
                      locale = locale,
                      name_repair = "universal",
                      ...
      ))
    tmp <- tmp %>%
      dplyr::rename(MEDI = Mel
      ) %>% 
      dplyr::mutate(Datetime =
                      lubridate::dmy_hms(paste(Date, Time), tz = tz),
                    Id = paste(.data$id, .data$ls, sep = ".")
      )
  }),
  #Speccy
  Speccy = rlang::expr({
    tmp <-suppressMessages( 
      readr::read_csv(filename,
                      n_max = n_max,
                      id = "file.name",
                      locale = locale,
                      name_repair = "universal",
                      ...
      ))
    tmp <- tmp %>%
      dplyr::rename(MEDI = Melanopic.EDI) %>% 
      dplyr::mutate(Datetime =
                      Datetime %>% lubridate::parse_date_time(
                        orders =  c("%H:%M:%S %d/%m/%y", "%d/%m/%Y %H:%M") ,tz = tz, exact = TRUE))
  }),
  #Intelligent Automation Inc DeLux
  DeLux = rlang::expr({
    tmp <-suppressMessages( 
      readr::read_csv(filename,
                      n_max = n_max,
                      id = "file.name",
                      locale = locale,
                      name_repair = "universal",
                      col_types = c("fccdddddddddddddddddd"),
                      ...
      ))
    tmp <- tmp %>%
      dplyr::rename(Datetime = Timestamp) %>%
      dplyr::mutate(Datetime =
                      Datetime %>% lubridate::ymd_hms(tz = tz))
  }),
  #LiDo
  LiDo = rlang::expr({
    tmp <- suppressMessages(
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
    tmp <- tmp %>%
      dplyr::rename(Datetime = UTC.Timestamp,
                    MEDI = Ev_mel_D65.in.lx) %>% 
      dplyr::mutate(Datetime = 
                      Datetime %>% 
                      lubridate::dmy_hms() %>% 
                      lubridate::with_tz(tzone = tz))
  }),
  #ActLumus
  ActLumus = rlang::expr({
    tmp <- suppressMessages( 
      readr::read_delim(
        filename,
        skip = 32,
        delim = ";",
        n_max = n_max,
        col_types = paste0("c", rep("d", 32)),
        id = "file.name",
        locale = locale,
        name_repair = "universal",
        ...
      ))
    tmp <- tmp %>%
      dplyr::rename(Datetime = DATE.TIME,
                    MEDI = MELANOPIC.EDI) %>%
      dplyr::mutate(Datetime =
                      Datetime %>% lubridate::dmy_hms(tz = tz))
  }),
  #LYS
  LYS = rlang::expr({
    tmp <-suppressMessages( 
      readr::read_csv(filename,
                      n_max = n_max,
                      col_types = c("cfddddddddddd"),
                      id = "file.name",
                      locale = locale,
                      name_repair = "universal",
                      ...
      ))
    tmp <- tmp %>%
      dplyr::rename(Datetime = timestamp,
                    MEDI = mEDI) %>%
      dplyr::mutate(Datetime =
                      Datetime %>% lubridate::dmy_hms(tz = tz))
  }),
  #Actiwatch Spectrum
  Actiwatch_Spectrum = rlang::expr({
    #separate the dots list in the column_names and the rest
    dots <- rlang::list2(...)
    column_names <- dots$column_names
    if(is.null(column_names)) 
      stop("Actiwatch Spectrum requires a vector of `column_names` in the order in which they appear in the file in order to properly detect the starting row")
    dots$column_names <- NULL
    
    tmp <- 
      purrr::map(
        filename,
        \(x) {
          rows_to_skip <- detect_starting_row(x, 
                                              locale = locale, 
                                              column_names = column_names,
                                              n_max = n_max)
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
    tmp <- tmp %>%
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
  })
  
)

#this is needed to make the list available to the package
usethis::use_data(ll_import_expr, overwrite = TRUE, internal = TRUE)
#this is needed to make the list available to the user
usethis::use_data(ll_import_expr, overwrite = TRUE)

# Generic import function --------------------------------------------------------

#' Import a light logger dataset or related data
#'
#' @description
#'
#' Imports a dataset and does the necessary transformations to get the right
#' column formats. Unless specified otherwise, the function will set the
#' timezone of the data to `UTC`. It will also enforce an `Id` to separate
#' different datasets and will order/arrange the dataset within each `Id` by 
#' Datetime. See the Details and Devices section for more information and the
#' full list of arguments.
#'
#' @details 
#' There are specific and a general import function. The general import function
#' is described below, whereas the specific import functions take the form of
#' `import$device()`. The general import function is a thin wrapper around the
#' specific import functions. The specific import functions take the following
#' arguments:
#'
#' * `filename`: Filename(s) for the Dataset. Can also contain the filepath,
#' but `path` must then be `NULL`. Expects a `character`. If the vector is
#' longer than `1`, multiple files will be read in into one Tibble.
#' * `path`: Optional path for the dataset(s). `NULL` is the default. Expects
#' a `character`.
#' * `n_max`: maximum number of lines to read. Default is `Inf`.
#' * `tz`: Timezone of the data. `"UTC"` is the default. Expects a
#' `character`. You can look up the supported timezones with [OlsonNames()].
#' * `Id.colname`: Lets you specify a column for the id of a dataset. Expects a
#' symbol (Default is `Id`). This column will be used for grouping
#' ([dplyr::group_by()]).
#' * `auto.id`: If the `Id.colname` column is not part of the `dataset`, the `Id`
#' can be automatically extracted from the filename. The argument expects a
#' regular expression [regex] and will by default just give the whole filename
#' without file extension.
#' * `manual.id`: If this argument is not `NULL`, and no `Id` column is part
#' of the `dataset`, this `character` scalar will be used. **We discourage the
#' use of this arguments when importing more than one file**
#' * `locale`: The locale controls defaults that vary from place to place.
#' * `...`: supply additional arguments to the [readr] import functions, like `na`. Might also be used to supply arguments to the specific import functions, like `column_names` for `Actiwatch_Spectrum` devices. Those devices will alway throw a helpful error message if you forget to supply the necessary arguments.
#'   If the `Id` column is already part of the `dataset` it will just use
#'   this column. If the column is not present it will add this column and fill
#'   it with the filename of the importfile (see param `auto.id`).
#'
#' @param ... Parameters that get handed down to the specific import functions
#' @param device From what device do you want to import? For a few devices,
#'   there is a sample data file that you can use to test the function (see the
#'   examples). See [supported.devices] for a list of supported devices and see
#'   below for more information on devices with specific requirements.
#' @importFrom rlang :=
#' @return Tibble/Dataframe with a POSIXct column for the datetime
#' @export
#' @seealso [supported.devices]
#' @section Devices: 
#'   The set of import functions provide a convenient way to import light logger
#'   data that is then perfectly formatted to add metadata, make visualizations
#'   and analyses. There are a number of devices supported, where import should
#'   just work out of the box. To get an overview, you can simply call the
#'   `supported.devices` dataset. The list will grow continuously as the package
#'   is maintained.
#' ```{r}
#' supported.devices
#' ```
#' 
#'   ## ActLumus 
#'   A sample file is provided with the package, it can be accessed through
#'   `system.file("extdata/205_actlumus_Log_1020_20230904101707532.txt.zip",
#'   package = "LightLogR")`. It does not need to be unzipped to be imported.
#'   This sample file is a good example for a regular dataset without gaps
#'   ## LYS 
#'   A sample file is provided with the package, it can be accessed
#'   through `system.file("extdata/sample_data_LYS.csv", package =
#'   "LightLogR")`. This sample file is a good example for an irregular dataset.
#'   ## Actiwatch_Spectrum:
#'   **Required Argument: `column_names`** A character vector containing column 
#'   names in the order in which they appear in the file. This is necessary to 
#'   find the starting point of actual data.
#'
#' @section Examples:
#'
#'   ## Imports made easy
#'
#'   To import a file, simple specify the filename (and path) and feed it to the
#'   `import_Dataset` function. There are sample datasets for all devices.
#'
#'   The import functions provide a basic overview of the data after import,
#'   such as the intervals between measurements or the start and end dates.
#'
#' ```{r}
#' filepath <- system.file("extdata/sample_data_LYS.csv", package = "LightLogR")
#' dataset <- import_Dataset("LYS", filepath)
#' ```
#'   Import functions can also be called directly:
#'
#' ```{r}
#' filepath <- system.file("extdata/205_actlumus_Log_1020_20230904101707532.txt.zip", package = "LightLogR")
#' dataset <- import$ActLumus(filepath)
#' ```
#'
#' ```{r}
#' dataset %>%
#' dplyr::select(Datetime, TEMPERATURE, LIGHT, MEDI, Id) %>%
#' dplyr::slice(1500:1505) %>%
#' flextable::flextable() %>%
#' flextable::autofit()
#' ```

import_Dataset <- function(device, ...) {
  
  #input control
  stopifnot(
    "device specification is not in the list of supported devices, see the documentation for more info" = 
      device %in% supported.devices
  )
  
  import_function_expr <- rlang::parse_expr(paste0("import$", device))
  
  eval(import_function_expr)(...)
}

# General ----------------------------------------------------------------
#This internal helper function is a function factory to create import functions
#based on device name and specific import expression
imports <- function(device,
                    import.expr) {
  
  import.expr <- rlang::enexpr(import.expr)
  #this next step is needed to make the function work with the rlang::new_function
  Id.colname <- quote({{ Id.colname }})
  
  rlang::new_function(
    #function arguments
    rlang::exprs(
      filename =, 
      path = NULL, 
      n_max = Inf,
      tz = "UTC",
      Id.colname = Id,
      auto.id = ".*",
      manual.id = NULL,
      locale = readr::default_locale(),
      silent = FALSE,
      ... =
    ),
    #function expression
    rlang::expr({
      
      if (!is.null(path)) {
        filename <- file.path(path, filename)
      }
      
      id.colname.defused <- colname.defused(!!Id.colname)
      #initial checks
      stopifnot(
        "filename needs to be a character (vector)" = is.character(filename),
        "device needs to be a character" = is.character(!!device),
        "tz needs to be a character" = is.character(tz),
        "tz needs to be a valid time zone, see `OlsonNames()`" = tz %in% OlsonNames(),
        "auto.id needs to be a string" = is.character(auto.id),
        "n_max needs to be a positive numeric" = is.numeric(n_max)
      )
      #import the file
      tmp <- rlang::eval_tidy(!!import.expr)
      
      #validate/manipulate the file
      if(dim(tmp)[1] == 0) {
        stop("No data could be imported. Please check your file and settings")
      }
      
      if(!id.colname.defused %in% names(tmp)) {
        switch(is.null(manual.id) %>% as.character(),
               "TRUE" =
                 {tmp <- tmp %>%
                   dplyr::mutate(!!Id.colname :=
                                   basename(file.name) %>%
                                   tools::file_path_sans_ext() %>%
                                   stringr::str_extract(
                                     auto.id,
                                     group =  
                                       if(stringr::str_detect(auto.id, "\\(")) 1
                                     ),
                                 .before = 1)},
               "FALSE" =
                 {tmp <- tmp %>%
                   dplyr::mutate(!!Id.colname := manual.id, .before = 1)}
        )
      }
      tmp <- tmp %>%
        dplyr::mutate(file.name = basename(file.name) %>%
                        tools::file_path_sans_ext(),
                      !!Id.colname := factor(!!Id.colname)) %>%
        dplyr::group_by(Id = !!Id.colname) %>%
        dplyr::arrange(Datetime, .by_group = TRUE)
      
      #give info about the file
      if(!silent) import.info(tmp, !!device, tz, Id)
      
      #return the file
      tmp
      
    }),
    rlang::caller_env()
  )
}

import_arguments <- list(
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
                        orders =  "HMSdmy",tz = tz))
  }),
  #Intelligent Automation Inc DeLux
  DeLux = rlang::expr({
    tmp <-suppressMessages( 
      readr::read_csv(filename,
                      n_max = n_max,
                      id = "file.name",
                      locale = locale,
                      name_repair = "universal",
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
      readr::read_csv2(
        filename,
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

#' Import Datasets from supported devices
#'
#' @rdname import_Dataset
#' @export
import <- purrr::imap(import_arguments, \(x, idx) imports(idx,x))

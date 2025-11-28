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
#' @details There are specific and a general import function. The general import
#'   function is described below, whereas the specific import functions take the
#'   form of `import$device()`. The general import function is a thin wrapper
#'   around the specific import functions. The specific import functions take
#'   the following arguments:
#'
#' * `filename`: Filename(s) for the Dataset. Can also contain the filepath,
#'   but `path` must then be `NULL`. Expects a `character`. If the vector is
#'   longer than `1`, multiple files will be read in into one Tibble.
#' * `path`: Optional path for the dataset(s). `NULL` is the default. Expects
#'   a `character`.
#' * `n_max`: maximum number of lines to read. Default is `Inf`.
#' * `tz`: Timezone of the data. `"UTC"` is the default. Expects a
#'   `character`. You can look up the supported timezones with [OlsonNames()].
#' * `version`: Data formats can change, e.g. with software updates. This 
#'    argument allows switching between known data formats of the same device 
#'    model. Expects a `character` scalar. The default is `"default"`, which will
#'    always use the latest version. To find out which software versions are
#'    contained, call [supported_versions()].
#' * `Id.colname`: Lets you specify a column for the id of a dataset. Expects a
#'   symbol (Default is `Id`). This column will be used for grouping
#'   ([dplyr::group_by()]).
#' * `auto.id`: If the `Id.colname` column is not part of the `dataset`, the `Id`
#'   can be automatically extracted from the filename. The argument expects a
#'   regular expression [regex] and will by default just give the whole filename
#'   without file extension.
#' * `manual.id`: If this argument is not `NULL`, and no `Id` column is part
#'   of the `dataset`, this `character` scalar will be used. **We discourage the
#'   use of this arguments when importing more than one file**
#' * `silent`: If set to `TRUE`, the function will not print a summary message
#'   of the import or plot the overview. Default is `FALSE`.
#' * `locale`: The locale controls defaults that vary from place to place.
#' * `not.before`: Remove data prior to this date. This argument is provided to `start` of [filter_Date()]. Data will be filtered out before any of the summaries are shown.
#' * `dst_adjustment`: If a file crosses daylight savings time, but the device does not adjust time stamps accordingly, you can set this argument to `TRUE`, to apply this shift manually. It is selective, so it will only be done in files that cross between DST and standard time. Default is `FALSE`. Uses [dst_change_handler()] to do the adjustment. Look there for more infos. It is not equipped to handle two jumps in one file (so back and forth between DST and standard time), but will work fine if jums occur in separate files.
#' * `auto.plot`: a logical on whether to call [gg_overview()] after import. Default is `TRUE`. But is set to `FALSE` if the argument `silent` is set to `TRUE`.
#' * `...`: supply additional arguments to the \pkg{readr} import functions, like `na`. Might also be used to supply arguments to the specific import functions, like `column_names` for `Actiwatch_Spectrum` devices. Those devices will always throw a helpful error message if you forget to supply the necessary arguments.
#'   If the `Id` column is already part of the `dataset` it will just use this
#'   column. If the column is not present it will add this column and fill it
#'   with the filename of the importfile (see param `auto.id`).
#' * `print_n` can be used if you want to see more rows from the observation intervals
#' * `remove_duplicates` can be used if identical observations are present 
#'   within or across multiple files. The default is `FALSE`. The function keeps 
#'   only unique observations (=rows) if set to' TRUE'. This is a convenience 
#'   implementation of [dplyr::distinct()].
#'
#' @param ... Parameters that get handed down to the specific import functions
#' @param device From what device do you want to import? For a few devices,
#'   there is a sample data file that you can use to test the function (see the
#'   examples). See [supported_devices()] for a list of supported devices and
#'   see below for more information on devices with specific requirements.
#' @importFrom rlang :=
#' @return Tibble/Dataframe with a POSIXct column for the datetime
#' @export
#' @seealso [supported_devices]
#' @section Devices: The set of import functions provide a convenient way to
#'   import light logger data that is then perfectly formatted to add metadata,
#'   make visualizations and analyses. There are a number of devices supported,
#'   where import should just work out of the box. To get an overview, you can
#'   simply call the `supported_devices()` dataset. The list will grow
#'   continuously as the package is maintained. More than one data formats may 
#'   be available for a given device. Check with `supported_versions()` if you
#'   run into problems with imports, despite a correct device setting.
#' ```{r}
#' supported_devices()
#' ```
#'
#'   ## ActLumus
#'
#'   Manufacturer: Condor Instruments
#'
#'   Model: ActLumus
#'
#'   Implemented: Sep 2023
#'
#'   A sample file is provided with the package, it can be accessed through
#'   `system.file("extdata/205_actlumus_Log_1020_20230904101707532.txt.zip",
#'   package = "LightLogR")`. It does not need to be unzipped to be imported.
#'   This sample file is a good example for a regular dataset without gaps.
#'
#'   ## LYS
#'
#'   Manufacturer: LYS Technologies
#'
#'   Model: LYS Button
#'
#'   Implemented: Sep 2023
#'
#'   A sample file is provided with the package, it can be accessed through
#'   `sample.data.irregular`. This
#'   sample file is a good example for an irregular dataset.
#'
#'   ## Actiwatch_Spectrum & Actiwatch_Spectrum_de
#'
#'   Manufacturer: Philips Respironics
#'
#'   Model: Actiwatch Spectrum
#'
#'   Implemented: Nov 2023 / July 2024
#'   
#'   ## ActTrust
#'
#'   Manufacturer: Condor Instruments
#'
#'   Model: ActTrust1, ActTrust2
#'
#'   Implemented: Mar 2024
#'
#'   This function works for both ActTrust 1 and 2 devices
#'
#'   ## Speccy
#'
#'   Manufacturer: Monash University
#'
#'   Model: Speccy
#'
#'   Implemented: Feb 2024
#'
#'   ## DeLux
#'
#'   Manufacturer: Intelligent Automation Inc
#'
#'   Model: DeLux
#'
#'   Implemented: Dec 2023
#'
#'   ## LiDo
#'
#'   Manufacturer: University of Lucerne
#'
#'   Model: LiDo
#'
#'   Implemented: Nov 2023
#'
#'   ## SpectraWear
#'
#'   Manufacturer: University of Manchester
#'
#'   Model: SpectraWear
#'
#'   Implemented: May 2024
#'
#'   ## NanoLambda
#'
#'   Manufacturer: NanoLambda
#'
#'   Model: XL-500 BLE
#'
#'   Implemented: May 2024
#'
#'   ## LightWatcher
#'
#'   Manufacturer: Object-Tracker
#'
#'   Model: LightWatcher
#'
#'   Implemented: June 2024
#'
#'   ## VEET
#'
#'   Manufacturer: Meta Reality Labs
#'
#'   Model: VEET
#'
#'   Implemented: July 2024
#'
#'   **Required Argument: `modality`** A character scalar describing the
#'   modality to be imported from. Can be one of `"ALS"` (Ambient light sensor),
#'   `"IMU"` (Inertial Measurement Unit), `"INF"` (Information), `"PHO"`
#'   (Spectral Sensor), `"TOF"` (Time of Flight)
#'
#'   ## Circadian_Eye
#'
#'   Manufacturer: Max-Planck-Institute for Biological Cybernetics, Tübingen
#'
#'   Model: melanopiQ Circadian Eye (Prototype)
#'
#'   Implemented: July 2024
#'
#'   ## Kronowise
#'
#'   Manufacturer: Kronohealth
#'
#'   Model: Kronowise
#'
#'   Implemented: July 2024
#'
#'   ## GENEActiv with GGIR preprocessing
#'
#'   Manufacturer: Activeinsights
#'
#'   Model: GENEActiv
#'
#'   **Note:** This import function takes GENEActiv data that was preprocessed
#'   through the [GGIR]( https://cran.r-project.org/package=GGIR) package. By
#'   default, `GGIR` aggregates light data into intervals of 15 minutes. This
#'   can be set by the `windowsizes` argument in GGIR, which is a three-value
#'   vector, where the second values is set to 900 seconds by default. To import
#'   the preprocessed data with `LightLogR`, the `filename` argument requires a
#'   path to the parent directory of the GGIR output folders, specifically the
#'   `meta` folder, which contains the light exposure data. Multiple `filename`s
#'   can be specified, each of which needs to be a path to a different GGIR
#'   parent directory. GGIR exports can contain data from multiple participants,
#'   these will always be imported fully by providing the parent directory. Use
#'   the `pattern` argument to extract sensible `Id`s from the *.RData*
#'   filenames within the *meta/basic/* folder. As per the author, [Dr. Vincent
#'   van Hees](https://www.accelting.com), GGIR preprocessed data are always in
#'   local time, provided the `desiredtz`/`configtz` are properly set in GGIR.
#'   `LightLogR` still requires a timezone to be set, but will not timeshift the
#'   import data.
#'
#'   ## MotionWatch 8
#'
#'   Manufacturer: CamNtech
#'
#'   Implemented: September 2024
#'
#'   ## LIMO
#'
#'   Manufacturer: ENTPE
#'
#'   Implemented: September 2024
#'
#'   LIMO exports `LIGHT` data and `IMU` (inertia measurements, also UV) in
#'   separate files. Both can be read in with this function, but not at the same
#'   time. Please decide what type of data you need and provide the respective
#'   filenames.
#'
#'   ## OcuWEAR
#'
#'   Manufacturer: Ocutune
#'
#'   Implemented: September 2024
#'
#'   OcuWEAR data contains spectral data. Due to the format of the data file,
#'   the spectrum is not directly part of the tibble, but rather a list column
#'   of tibbles within the imported data, containing a `Wavelength` (nm) and
#'   `Intensity` (mW/m^2) column.
#'
#'   ## Clouclip
#'
#'   Manufacturer: Clouclip
#'
#'   Implemented: April 2025
#'
#'   Clouclip export files have the ending `.xls`, but are no real Microsoft
#'   Excel files, rather they are tab-separated text files. LightLogR thus does
#'   not read them in with an excel import routine. The measurement columns
#'   `Lux` and `Dis` contain sentinel values. `-1` (`Dis` and `Lux`) indicates
#'   sleep mode, whereas `204` (only `Dis`) indicates an out of range
#'   measurement. These values will be set to `NA`, and an additional column is
#'   added that translates these status codes. The columns carry the name
#'   `{.col}_status`.
#'
#'   ## MiEye
#'
#'   Manufacturer: CHI. Circadian Health Innovations
#'
#'   Implemented: October 2025
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
#' filepath <- system.file("extdata/205_actlumus_Log_1020_20230904101707532.txt.zip", package = "LightLogR")
#' dataset <- import_Dataset("ActLumus", filepath, auto.plot = FALSE)
#' ```
#'   Import functions can also be called directly:
#'
#' ```{r}
#' dataset <- import$ActLumus(filepath, auto.plot = FALSE)
#' ```
#' 
#' ```{r}
#' dataset %>%
#' dplyr::select(Datetime, TEMPERATURE, LIGHT, MEDI, Id) %>%
#' dplyr::slice(1500:1505)
#' ```


import_Dataset <- function(device, ...) {
  
  #input control
  stopifnot(
    "device specification is not in the list of supported devices, see the documentation for more info" = 
      device %in% supported_devices()
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
      tz = "UTC",
      path = NULL,
      version = "default",
      n_max = Inf,
      not.before = "2001-01-01",
      dst_adjustment = FALSE,
      Id.colname = Id,
      auto.id = ".*",
      manual.id = NULL,
      auto.plot = TRUE,
      locale = readr::default_locale(),
      silent = FALSE,
      print_n = 10,
      remove_duplicates = FALSE,
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
        "dst_adjustment needs to be a logical" = is.logical(dst_adjustment),
        "n_max needs to be a positive numeric" = is.numeric(n_max),
        "version needs to be a character" = is.character(version)
      )
      #import the file
      data <- rlang::eval_tidy(!!import.expr)
      
      #validate/manipulate the file
      if(dim(data)[1] == 0) {
        stop("No data could be imported. Please check your file and settings")
      }
      
      #check if the id column is present, if not, add it
      if(!id.colname.defused %in% names(data)) {
        switch(is.null(manual.id) %>% as.character(),
               "TRUE" =
                 {data <- data %>%
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
                 {data <- data %>%
                   dplyr::mutate(!!Id.colname := manual.id, .before = 1)}
        )
      }
      
      #add a filename
      #check if the id column is a factor, if not, make it one, and group by it
      data <- data %>%
        dplyr::mutate(file.name = basename(file.name) %>%
                        tools::file_path_sans_ext(),
                      Id = factor(!!Id.colname), .before = 1) %>%
        dplyr::group_by(Id) %>%
        dplyr::arrange(Datetime, .by_group = TRUE)
      
      #if there are Datetimes with NA value, drop them
      na.count <- 0
      if(any(is.na(data$Datetime))) {
        na.count <- sum(is.na(data$Datetime))
        data <- data %>% tidyr::drop_na(Datetime)
      }
      
      #remove dates from not.before
      if(min(data$Datetime) < lubridate::as_datetime(not.before)) {
      data <- 
        data |> 
        filter_Date(start = not.before)
      } else not.before <- NULL
      
      #if there is an Id with less than two observations, give a warning
      if(any(table(data$Id) < 2)) {
        stop("Some Ids have only one observation. This causes problems with functions in LightLogR that calculate time differences. Please remove these Ids from import: ", 
             which(table(data$Id) < 2) %>% names()
        )
      }
      # browser()
      #if there are duplicate rows, remove them and print an info message
      var_names <- names(data) |> setdiff("file.name")
      duplicate.table <- 
        data |>  
        dplyr::add_count(dplyr::pick(dplyr::all_of(var_names)), name = "dupes") |> 
        dplyr::filter(dupes > 1) |> 
        dplyr::arrange(dplyr::desc(dupes))
      duplicates <- 
        duplicate.table |> 
        nrow()
      
      orig_rows <- data %>% nrow()
      
      if(duplicates > 0 & remove_duplicates) {
        data <- data %>% dplyr::distinct(dplyr::pick(-file.name),.keep_all = TRUE)
        cat(paste0(format(orig_rows - nrow(data), big.mark = "'"), " duplicate rows were removed during import.\n"))
      }
      
      #if there are untreated duplicate rows, give a warning
      if(duplicates > 0 & !remove_duplicates) {
        messages <- paste0(format(duplicates, big.mark = "'"), " rows in your dataset(s) are identical to at least one other row. This causes problems during analysis. Please set `remove_duplicates = TRUE` during import. Import will be stopped now and a dataframe with the duplicate rows returned.\n")
        warning(messages)
        return(duplicate.table)
      }
      
      #if dst_adjustment is TRUE, adjust the datetime column
      if(dst_adjustment) {
        data <- data %>% dst_change_handler(filename.colname = file.name)
      }
      #give info about the file
      if(!silent) 
        import.info(
          data = data, #the data 
          device = !!device, #the type of device
          tz = tz, #the timezone
          Id.colname = Id, #the id column name
          dst_adjustment = dst_adjustment, #whether there is a dst adjustment
          filename = filename, #what the filename(s) is/are
          na.count = na.count, #how many NA values were dropped
          print_n = print_n, #how many rows to print for observation intervals
          not.before = not.before #when the earliest import was
          )
      
      #if autoplot is TRUE & silent is FALSE, make a plot
      if(auto.plot & !silent) {
        data %>% gg_overview() %>% print()
      }
      #return the file
      data
      
    }),
    rlang::caller_env()
  )
}

# Import functions -------------------------------------------------------

#source the import expressions
source("R/import_expressions.R", local = TRUE)

#' Import Datasets from supported devices
#'
#' @rdname import_Dataset
#' @export
import <- purrr::imap(import_expr, \(x, idx) imports(idx,x))


#' Adjust device imports or make your own
#'
#' @param import_expr A named list of import expressions. The basis for
#'   `LightLogR`'s import functions is the included dataset `ll_import_expr()`. If
#'   this function were to be given that exact dataset, and bound to a variable
#'   called `import`, it would be identical to the `import` function. See
#'   `details`.
#'
#' @details This function should only be used with some knowledge of how
#' expressions work in R. The minimal required output for an expression to work
#' as expected, it must lead to a data frame containing a `Datetime` column with
#' the correct time zone. It has access to all arguments defined in the
#' description of `import_Dataset()`. The `...` argument should be passed to
#' whatever csv reader function is used, so that it works as expected. Look at
#' `ll_import_expr()$ActLumus` for a quite minimal example.
#'
#' @return A list of import functions
#' @export
#'
#' @examples
#' #create a new import function for the ActLumus device, same as the old
#' new_import <- import_adjustment(ll_import_expr())
#' #the new one is identical to the old one in terms of the function body
#' identical(body(import$ActLumus), body(new_import$ActLumus))
#'
#' #change the import expression for the ActLumus device to add a message at the top
#' new_import_expr <- ll_import_expr()
#' new_import_expr$ActLumus[[6]] <-
#' rlang::expr({ cat("**This is a new import function**\n")
#' data
#' })
#' new_import <- import_adjustment(new_import_expr)
#' filepath <- 
#' system.file("extdata/205_actlumus_Log_1020_20230904101707532.txt.zip", 
#'             package = "LightLogR")
#' #Now, a message is printed when the import function is called
#' data <- new_import$ActLumus(filepath, auto.plot = FALSE)

import_adjustment <- function(import_expr) {
    purrr::imap(import_expr, \(x, idx) imports(idx,x))
}

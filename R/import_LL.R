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
#' * `dst_adjustment`: If a file crosses daylight savings time, but the device does not adjust time stamps accordingly, you can set this argument to `TRUE`, to apply this shift manually. It is selective, so it will only be done in files that cross between DST and standard time. Default is `FALSE`. Uses `dst_change_handler()` to do the adjustment. Look there for more infos. It is not equipped to handle two jumps in one file (so back and forth between DST and standard time), but will work fine if jums occur in separate files.
#' * `auto.plot`: a logical on whether to call [gg_overview()] after import. Default is `TRUE`.
#' * `...`: supply additional arguments to the [readr] import functions, like `na`. Might also be used to supply arguments to the specific import functions, like `column_names` for `Actiwatch_Spectrum` devices. Those devices will always throw a helpful error message if you forget to supply the necessary arguments.
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
#'   ## Actiwatch_Spectrum
#'   **Required Argument: `column_names`** A character vector containing column 
#'   names in the order in which they appear in the file. This is necessary to 
#'   find the starting point of actual data.
#'   ## ActTrust
#'   This function works for both ActTrust 1 and 2 devices
#'   ## Speccy
#'   .  
#'   ## DeLux
#'   . 
#'   ## LiDo
#'   .  
#'   ## SpectraWear
#'   .
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
#' dataset <- import_Dataset("LYS", filepath, auto.plot = FALSE)
#' ```
#'   Import functions can also be called directly:
#'
#' ```{r}
#' filepath <- system.file("extdata/205_actlumus_Log_1020_20230904101707532.txt.zip", package = "LightLogR")
#' dataset <- import$ActLumus(filepath, auto.plot = FALSE)
#' dataset %>% gg_days()
#' ```
#' ```{r}
#' dataset %>%
#' dplyr::select(Datetime, TEMPERATURE, LIGHT, MEDI, Id) %>%
#' dplyr::slice(1500:1505)
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
      dst_adjustment = FALSE,
      Id.colname = Id,
      auto.id = ".*",
      manual.id = NULL,
      auto.plot = TRUE,
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
        "dst_adjustment needs to be a logical" = is.logical(dst_adjustment),
        "n_max needs to be a positive numeric" = is.numeric(n_max)
      )
      #import the file
      tmp <- rlang::eval_tidy(!!import.expr)
      
      #validate/manipulate the file
      if(dim(tmp)[1] == 0) {
        stop("No data could be imported. Please check your file and settings")
      }
      
      #check if the id column is present, if not, add it
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
      
      #add a filename
      #check if the id column is a factor, if not, make it one, and group by it
      tmp <- tmp %>%
        dplyr::mutate(file.name = basename(file.name) %>%
                        tools::file_path_sans_ext(),
                      !!Id.colname := factor(!!Id.colname)) %>%
        dplyr::group_by(Id = !!Id.colname) %>%
        dplyr::arrange(Datetime, .by_group = TRUE)
      
      #if there are Datetimes with NA value, drop them
      na.count <- 0
      if(any(is.na(tmp$Datetime))) {
        na.count <- sum(is.na(tmp$Datetime))
        tmp <- tmp %>% tidyr::drop_na(Datetime)
      }
      
      #if dst_adjustment is TRUE, adjust the datetime column
      if(dst_adjustment) {
        tmp <- tmp %>% dst_change_handler(filename.colname = file.name)
      }
      #give info about the file
      if(!silent) 
        import.info(
          tmp = tmp, #the data 
          device = !!device, #the type of device
          tz = tz, #the timezone
          Id.colname = Id, #the id column name
          dst_adjustment = dst_adjustment, #whether there is a dst adjustment
          filename = filename, #what the filename(s) is/are
          na.count = na.count #how many NA values were dropped
          )
      
      #if autoplot is TRUE, make a plot
      if(auto.plot) {
        tmp %>% gg_overview() %>% print()
      }
      #return the file
      tmp
      
    }),
    rlang::caller_env()
  )
}

# Import functions -------------------------------------------------------

#source the import expressions
source("R/import_expressions.R")

#' Import Datasets from supported devices
#'
#' @rdname import_Dataset
#' @export
import <- purrr::imap(ll_import_expr, \(x, idx) imports(idx,x))


#' Adjust device imports or make your own
#'
#' @param import_expr A named list of import expressions. The basis for
#'   `LightLogR`'s import functions is the included dataset `ll_import_expr`. If
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
#' `ll_import_expr$LYS` for a quite minimal example.
#'
#' @return A list of import functions
#' @export
#'
#' @examples
#' #create a new import function for the LYS device, same as the old
#' new_import <- import_adjustment(ll_import_expr)
#' #the new one is identical to the old one in terms of the function body
#' identical(body(import$LYS), body(new_import$LYS))
#'
#' #change the import expression for the LYS device to add a message at the top
#' ll_import_expr$LYS[[4]] <-
#' rlang::expr({ cat("**This is a new import function**\n")
#' tmp
#' })
#' new_import <- import_adjustment(ll_import_expr)
#' filepath <- system.file("extdata/sample_data_LYS.csv", package = "LightLogR")
#' #Now, a message is printed when the import function is called
#' new_import <- new_import$LYS(filepath)

import_adjustment <- function(import_expr) {
    purrr::imap(import_expr, \(x, idx) imports(idx,x))
}
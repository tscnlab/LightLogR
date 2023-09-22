# Generic import function --------------------------------------------------------

#' Import a light logger dataset or related data
#'
#' Imports a dataset and does the necessary transformations to get the right
#' column formats. Unless specified otherwise, the function will set the
#' timezone of the data to `UTC`. It will also enforce an `id` to separate
#' different datasets and will order/arrange the dataset within each `id`.
#'
#' @param filename Filename(s) for the Dataset. Can also contain the filepath,
#'   but `path` must then be `NULL`. Expects a `character`. If the vector is
#'   longer than `1`, multiple files will be read in into one Tibble.
#' @param path Optional path for the dataset(s). `NULL` is the default. Expects
#'   a `character`.
#' @param n_max maximum number of lines to read. Default is `Inf`.
#' @param tz Timezone of the data. `"UTC"` is the default. Expects a
#'   `character`. You can look up the supported timezones with [OlsonNames()].
#' @param ID.colname Lets you specify a column for the participant id. Expects a
#'   symbol (Default is `Id`). If the column is already part of the `dataset` it
#'   will just use this column. If the column is not present it will add this
#'   column and fill it with the filename of the importfile (see param
#'   `auto.id`). This column will be used for grouping ([dplyr::group_by()]).
#' @param auto.id If the `Id.colname` column is added to the `dataset`, the 
#'   `Id` can be automatically extracted from the filename. The argument expects
#'   a regular expression [regex] and will by default just give the whole
#'   filename without file extension.
#' @param manual.id If this argument is not `NULL`, and no `ID` column is part 
#' of the `dataset`, this `character` scalar will be used. 
#' **Don´t use this argument if multiple files from different participants are used!**.
#' @param ... Parameters that get handed down to the specific import functions
#' @param device From what device do you want to import? For every supported
#'   device, there is a sample data file that you can use to test the function
#'   (see the examples). Currently the following devices are supported (followed
#'   by the `device.ext` spec to access the sample file):
#' * `"ActLumus"` (ActLumus.txt)
#' * `"LYS"` (LYS.csv)
#' @importFrom rlang :=
#' @return Tibble/Dataframe with a POSIXct column for the datetime
#' @export
#' @section Examples:
#'
#'   ## Imports made easy
#'
#'   The set of import functions provide a convenient way to import light logger
#'   data that is then perfectly formatted to add metadata, make visualizations
#'   and analyses. There are a number of devices supported, where import should
#'   just work out of the box. To get an overview, you can simply call the
#'   `supported.devices` dataset. The list will grow continuously as the package
#'   is maintained.
#'
#' ```{r}
#' supported.devices
#' ```
#'
#'   To import a file, simple specify the filename (and path) and feed it to the
#'   `import.Dataset` function. There are sample datasets for all devices.
#'
#'   The import functions provide a basic overview of the data after import,
#'   such as the intervals between measurements or the start and end dates.
#'
#' ```{r}
#' filepath <- system.file("extdata/sample_data_LYS.csv", package = "LightLogR")
#' dataset <- import.Dataset("LYS", filepath)
#' ```
#'   Import functions can also be called directly:
#'
#' ```{r}
#' filepath <- system.file("extdata/sample_data_ActLumus.txt", package = "LightLogR")
#' dataset <- import.ActLumus(filepath)
#' ```
#'
#' ```{r}
#' dataset %>%
#' dplyr::select(Datetime, TEMPERATURE, LIGHT, `MELANOPIC EDI`) %>%
#' dplyr::slice(1500:1505) %>%
#' flextable::flextable() %>%
#' flextable::autofit()
#' ```

import.Dataset <- function(device, ...) {
  
  #input control
  stopifnot(
    "device specification is not in the list of supported devices, see the documentation for more info" = 
      device %in% supported.devices
  )
  
  import_function_expr <- rlang::parse_expr(paste0("import.", device))
  
  eval(import_function_expr)(...)
}

# ActLumus ----------------------------------------------------------------

#' Import Dataset from ActLumus
#'
#' @rdname import.Dataset
#' @export

import.ActLumus <- 
  function(filename, 
           path = NULL, 
           n_max = Inf,
           tz = "UTC",
           ID.colname = Id,
           auto.id = ".*",
           manual.id = NULL) {
    
    #special handling for ActLumus files
    import.expr <- rlang::expr(
      {tmp <- readr::read_delim(paste0(!!path, !!filename),
                                skip = 32,
                                delim = ";",
                                n_max = !!n_max,
                                col_types = paste0("c",rep("d",32)),
                                id = "file.name"
      )
      tmp <- tmp %>%
        dplyr::rename(Datetime = `DATE/TIME`) %>%
        dplyr::mutate(Datetime =
                        Datetime %>% lubridate::dmy_hms(tz = !!tz))
      }
    )
    
    #generic import function
    import.link("ActLumus", {{ ID.colname }})
    
  }


# LYS ---------------------------------------------------------------------

#' Import Dataset from LYS Button
#'
#' @rdname import.Dataset
#' @export

import.LYS <- function(filename, 
                      path = NULL, 
                      n_max = Inf,
                      tz = "UTC",
                      ID.colname = Id,
                      auto.id = ".*",
                      manual.id = NULL) {
  
  #special handling for LYS files
  import.expr <- rlang::expr(
    {tmp <- readr::read_csv(paste0(!!path, !!filename),
                            n_max = !!n_max,
                            col_types = c("cfddddddddddd"),
                            id = "file.name"
    )
    tmp <- tmp %>%
      dplyr::rename(Datetime = timestamp) %>%
      dplyr::mutate(Datetime =
                      Datetime %>% lubridate::dmy_hms(tz = !!tz))
    }
  )
  
  #generic import function
  import.link("LYS", {{ ID.colname }})
  
}

#' Import Dataset from LYS Button
#'
#' Imports a dataset and does the necessary transformations to get the right
#' column formats. Unless specified otherwise, the function will set the
#' timezone of the data to `UTC`.
#'
#' @param filename Filename(s) for the Dataset. Can also contain the filepath,
#'   but `path` must then be `NULL`. Expects a `character`. If the vector is
#'   longer than `1`, multiple files will be read in into one Tibble.
#' @param path Optional path for the dataset(s). `NULL` is the default. Expects
#'   a `character`.
#' @param file.id Should the filename be stored as part of the data? Useful if
#'   you read in data from multiple files
#' @param n_max maximum number of lines to read. Default is `Inf`.
#'
#' @return Tibble/Dataframe with a POSIXct column for the datetime
#' @export
#' @examples
#' filepath <- system.file("extdata/sample_data_LYS.csv", package = "LightLogR")
#' sampledata <- import.LYS(filepath)
#' dplyr::slice_sample(sampledata, n = 5) %>% gt::gt()
#' 

import.LYS <- function(filename, 
                      path = NULL, 
                      file.id = FALSE,
                      n_max = Inf,
                      tz = "UTC") {
  
  #special handling for LYS files
  import.exp <- expr(
    {tmp <- readr::read_csv(paste0(path, filename),
                            n_max = n_max,
                            col_types = c("cfddddddddddd"),
                            id = if(file.id) "filepath"
    )
    tmp <- tmp %>%
      dplyr::rename(Datetime = timestamp) %>%
      dplyr::mutate(Datetime =
                      Datetime %>% lubridate::dmy_hms(tz = tz))
    }
  )
  
  #generic import function
  import.LL(filename = filename,
            device = "LYS",
            import.exp = eval(import.exp),
            file.id = file.id,
            n_max = n_max,
            tz = tz)
  
}

#' Import Dataset from ActLumus
#'
#' Imports a dataset and does the necessary transformations to get the right
#' column formats. Unless specified otherwise, the function will set the
#' timezone of the data to `UTC`.
#'
#' @param filename Filename(s) for the Dataset. Can also contain the filepath,
#'   but `path` must then be `NULL`. Expects a `character`. If the vector is
#'   longer than `1`, multiple files will be read in into one Tibble.
#' @param path Optional path for the dataset(s). `NULL` is the default. Expects
#'   a `character`.
#' @param file.id Should the filename be stored as part of the data? Useful if
#'   you read in data from multiple files
#' @param n_max maximum number of lines to read. Default is `Inf`.
#'
#' @return Tibble/Dataframe with a POSIXct column for the datetime
#' @export
#' @examples
#' filepath <- system.file("extdata/sample_data_ActLumus.txt", package = "LightLogR")
#' sampledata <- import.ActLumus(filepath)
#' dplyr::slice_sample(sampledata, n = 5) %>% gt::gt()
#' gg_day(sampledata, x.axis = Datetime, y.axis = `MELANOPIC EDI`, y.axis.label = "mEDI (lx)")

import.ActLumus <- 
  function(filename, 
           path = NULL, 
           file.id = FALSE,
           n_max = Inf,
           tz = "UTC") {
  
  #special handling for ActLumus files
  import.exp <- expr(
    {tmp <- readr::read_delim(paste0(path, filename),
                            skip = 32,
                            delim = ";",
                            n_max = n_max,
                            col_types = paste0("c",rep("d",32)),
                            id = if(file.id) "filepath"
    )
    tmp <- tmp %>%
      dplyr::rename(Datetime = `DATE/TIME`) %>%
      dplyr::mutate(Datetime =
                      Datetime %>% lubridate::dmy_hms(tz = tz))
    }
  )
  
  #generic import function
  import.LL(filename = filename,
            device = "ActLumus",
            import.exp = eval(import.exp),
            file.id = file.id,
            n_max = n_max,
            tz = tz)
  
}

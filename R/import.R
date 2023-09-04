#' Import Dataset from LYS Button
#'
#' Imports a dataset and does the necessary transformations to get the right
#' column formats.
#' 
#' @param filename Filename(s) for the Dataset. Can also contain the filepath,
#'   but **path** must then be *NULL*. Expects a *character*. If the vector is
#'   longer than *1*, multiple files will be read in into one Tibble.
#' @param path Optional path for the dataset(s). *NULL* is the default. Expects
#'   a *character*.
#' @param file.id Should the filename be stored as part of the data? Useful if
#'   you read in data from multiple files
#' @param n_max maximum number of lines to read. Default is *Inf*.
#'
#' @return Tibble/Dataframe with a POSIXct column for the datetime
#' @export
#' @examples
#' filepath <- system.file("extdata/sample_data_LYS.csv", package = "LightLogR")
#' sampledata <- importLYS(filepath, file.id = TRUE)
#' head(sampledata)
#' 

importLYS <- function(filename, 
                      path = NULL, 
                      file.id = FALSE,
                      n_max = Inf) {
  #initial checks
  stopifnot(is.character(filename))
  stopifnot(is.logical(file.id))
  stopifnot(is.numeric(n_max))
  #import the file
  tmp <- readr::read_csv(paste0(path, filename),
                         col_types = c("cfddddddddddd"),
                         id = if(file.id) "filepath"
                         )
  tmp <- tmp %>% 
    dplyr::rename(Datetime = timestamp) %>% 
    dplyr::mutate(Datetime = Datetime %>% lubridate::dmy_hms())
  #validate the file
  
  #give info about the file
  message("Successfully read in ", nrow(tmp), " observations")
  message("Start: ", tmp$Datetime[1])
  message("End: ", max(tmp$Datetime))
  
  #return the file
  tmp
}

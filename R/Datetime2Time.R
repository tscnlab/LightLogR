#' Convert Datetime columns to Time columns
#'
#' @param dataset A data.frame with POSIXct columns.
#' @param cols The column names to convert. Expects a `symbol`. The default will
#'   convert all POSIXct columns. If uncertain whether columns exist in the
#'   dataset, use [dplyr::any_of()].
#' @param silent Logical on whether no message shall be shown if input and
#'   output are identical. Defaults to `FALSE` (i.e., a message is shown).
#'
#' @returns The input dataset with converted POSIXct columns as time (hms)
#'   columns. With the default settings, if no POSIXct column exists, input and
#'   output will be identical.
#' @export
#'
#' @examples
#' sample.data.environment |> Datetime2Time()
#' #more than one POSIX col
#' sample.data.environment |>
#'   dplyr::mutate(Datetime2 = lubridate::POSIXct(1)) |>
#'   Datetime2Time()
#' #only converting one of them
#' sample.data.environment |>
#'   dplyr::mutate(Datetime2 = lubridate::POSIXct(1)) |>
#'   Datetime2Time(Datetime)
#' #if uncertain whether column exists
#' sample.data.environment |>
#'   Datetime2Time(dplyr::any_of("Datetime3"))

Datetime2Time <- function(dataset, 
                          cols = dplyr::where(lubridate::is.POSIXct),
                          silent = FALSE) {
  stopifnot("dataset neets to be a data.frame" = is.data.frame(dataset))
  
  data <- 
  dataset |> 
    dplyr::mutate(
      dplyr::across({{ cols }}, hms::as_hms)
      )
    
  if(identical(data, dataset) & !silent){
    message("No columns were affected")
  }
  data
}
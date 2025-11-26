#' Get all the supported devices in LightLogR
#'
#' Returns a vector of all the supported devices in LightLogR.
#'
#' These are all supported devices where there is a dedicated import function.
#' Import functions can be called either through [import_Dataset()] with the
#' respective `device = "device"` argument, or directly, e.g.,
#' `import$ActLumus()`.
#'
#' @return A character vector of all supported devices
#' @export
#' @seealso [import_Dataset]
#'
#' @examples supported_devices()


supported_devices <- function() {
  names(import_expr)[order(names(import_expr))]
}

#' Get all the supported device-formats in LightLogR
#'
#' Returns all the supported versions for device-formats in LightLogR.
#'
#' This list contains all data formats for each device. These formats can be
#' used through [import_Dataset()] with the respective `version`. When there are
#' no entries for a device, this device has only one known format.
#'
#' @param device optionally specify a device to only show entries for this
#'   devices
#'
#' @return A list of tibbles (or just a tibble if device is provided) containing
#'   the device name, version names, a logical that indicates whether this is
#'   the version that is used by default, and a description.
#' @export
#' @seealso [import_Dataset], [supported_devices]
#'
#' @examples supported_versions()
supported_versions <- function(device = NULL) {
  
  stopifnot("device must bei either `NULL` or a character scalar" = 
              rlang::is_scalar_character(device) | is.null(device))
  versions <- 
  supported_devices() |> 
    as.list() |> 
    rlang::set_names() |> 
    purrr::map(\(x) {
      tibble::tibble(
      Device = x,
      Version = "initial",
      Default = TRUE,
      Description = "Device format as it was initially implemented in LightLogR."
      )
      })
  
  #Actiwatch Spectrum
  versions[["Actiwatch_Spectrum"]][2,] <- 
    tibble::tibble_row("Actiwatch_Spectrum",
                       "de",
                       FALSE,
                       "This version is for a german file format, which slightly differs in the datetime format, the column names, and the decimal separator.")

  #VEET
  versions[["VEET"]][1,3] <- FALSE
  versions[["VEET"]][2,] <- 
    tibble::tibble_row("VEET",
                       "2.1.7",
                       TRUE,
                       "In firmware version 2.1.7 a change was introduced to the `PHO` modality, where only one `CLEAR` channel is exported instead of two.")
  
  if(is.null(device)){
   return(versions)
  } else return(versions[[device]])
}

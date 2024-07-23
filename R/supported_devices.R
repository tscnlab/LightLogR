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

#' Sample of wearable data combined with environmental data
#'
#' A subset of data from a study at the TSCN-Lab using the ActLumus light
#' logger. This dataset contains personal light exposure information for one
#' participant over the course of five full days. The dataset is measured with a
#' 10 second epoch and is complete (no missing values). Additionally
#' environmental light data was captured with a second light logger mounted
#' horizontally at the TUM university roof, without any obstructions (besides a
#' transparent plastic halfdome). The epoch for this data is 30 seconds. This
#' dataset allows for some interesting calculations based on *available*
#' daylight at a given point in time.
#'
#' @format `sample.data.environment` A tibble with 69,120 rows and 3 columns:
#' \describe{
#'   \item{Datetime}{POSIXct Datetime}
#'   \item{MEDI}{melanopic EDI measurement data. Unit is lux.}
#'   \item{Id}{A `character` vector indicating whether the data is from the `Participant` or from the `Environment`.}
#' }
#' @source <https://www.tscnlab.org>
"sample.data.environment"


#' A vector of all supported devices for import functions
#'
#' These are all supported devices where there is a dedicated import function.
#' Import functions can be called either through [import_Dataset()] with the
#' respective `device = "device"` argument, or directly, e.g.,
#' `import$ActLumus()`.
#'
#' @format `supported.devices` A character vector, listing all supported devices
#' \describe{
#'   \item{suppored.devices}{strings}
#' }
"supported.devices"

#' A list of the specific device import functions
#'
#' These expressions are used to import and prepare data from specific devices.
#' The list is made explicit, so that a user, requiring slight changes to the
#' import functions, (e.g., because a timestamp is formatted differently) can
#' modify or add to the list. The list can be turned into a fully functional 
#' import function through `import_adjustment()`.
#'
#' @format `ll_import_expr` A list, with specific expressions for each supported device
#' \describe{
#'   \item{ll_import_expr}{expressions}
#' }
"ll_import_expr"

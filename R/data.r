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
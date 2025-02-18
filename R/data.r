#' Sample of wearable data combined with environmental data
#'
#' A subset of data from a study at the TSCN-Lab using the ActLumus light
#' logger. This dataset contains personal light exposure information for one
#' participant over the course of six full days. The dataset is measured with a
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

#' Gain / Gain-ratio tables to normalize counts
#'
#' A list of tables containing gain and gain-ratios to normalize counts across
#' different sensor gains. 
#' 
#' **Utility:** Some sensors provide raw counts and gain levels as
#' part of their output. In some cases it is desirable to compare counts between
#' sensors, e.g., to gauge daylight outside by comparing UV counts to photopic
#' counts (a high ratio of UV/Pho indicates outside daylight). Or to gauge 
#' daylight inside by comparing IR counts to photopic counts (a high ratio of
#' IR/Pho with a low ratio of UV/Pho indicates daylight in the context of LED or
#' fluorescent lighting)
#'
#' @format `gain.ratio.tables` A list containing two-column tibbles
#' \describe{
#'   \item{TSL2585}{gain table for the ambient light sensor [TSL2585](https://look.ams-osram.com/m/7899f3742d5a3f00/original/TSL2585-Miniature-Ambient-Light-Sensor-with-UV-and-Light-Flicker-Detection.pdf)}
#'   \item{Info}{A named `character` vector specifying the version and date a sensor was added}
#' }
"gain.ratio.tables"

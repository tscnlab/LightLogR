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

#' Sample of highly irregular wearable data
#'
#' A dataset collected with a wearable device that has a somewhat irregular
#' recording pattern. Overall, the data are recorded every 15 seconds. Every
#' tenth or so measurement takes 16 seconds, every hundredths 17 seconds, every
#' thousandths 18 seconds, and so on. This makes the dataset a prime example for
#' handling and dealing with irregular data.
#'
#' @format `sample.data.irregular` A tibble with 11,422 rows and 13 columns:
#' \describe{
#'   \item{Id}{A `character` vector indicating the participant (only `P1`).}
#'   \item{Datetime}{POSIXct Datetime}
#'   \item{lux}{numeric Illuminance. Unit is lux.}
#'   \item{kelvin}{numeric correlated colour temperature (CCT). Unit is Kelvin.}
#'   \item{rgbR}{numeric red sensor channel output. Unit is W/m2/nm.}
#'   \item{rgbG}{numeric green sensor channel output. Unit is W/m2/nm.}
#'   \item{rgbB}{numeric blue sensor channel output. Unit is W/m2/nm.}
#'   \item{rgbIR}{numeric infrared sensor channel output. Unit is W/m2/nm.}
#'   \item{movement}{numeric indicator for movement (intensity) of the device. Movement is given in discrete counts correlating to the number of instances the accelerometer records instances greater than 0.1875g per 15s sampling interval.}
#'   \item{MEDI}{melanopic EDI measurement data. Unit is lux.}
#'   \item{R.}{Unknown, but likely direct or derived output from the red sensor channel}
#'   \item{G.}{Unknown, but likely direct or derived output from the green sensor channel}
#'   \item{B.}{Unknown, but likely direct or derived output from the blue sensor channel}
#' }
"sample.data.irregular"


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

#' Alphaopic (+ photopic) action spectra
#'
#' A dataframe of alphaopic action spectra plus the photopic action spectrum.
#' The alphaopic action spectra are according to the [CIE S
#' 026/E:2018](https://www.cie.co.at/publications/cie-system-metrology-optical-radiation-iprgc-influenced-responses-light-0)
#' standard. The alphaopic action spectra are for a 32-year-old standard
#' observer. The photopic action spectrum is for a 2° standard observer.
#'
#' @format `alphaopic.action.spectra` A datafram with 471 rows and 7 columns:
#' \describe{
#'   \item{wavelength}{integer of wavelength, from 360 to 830 nm. Unit is nm}
#'   \item{melanopic}{numeric melanopic action spectrum}
#'   \item{l_cone_opic}{numeric L-cone opic action spectrum}
#'   \item{m_cone_opic}{numeric M-cone opic action spectrum}
#'   \item{s_cone_opic}{numeric S-cone opic action spectrum}
#'   \item{rhodopic}{numeric rhodopic action spectrum}
#'   \item{photopic}{numeric photopic action spectrum}
#' }
#' @source
#'   <https://www.cie.co.at/publications/cie-system-metrology-optical-radiation-iprgc-influenced-responses-light-0>
#' @source
#'   <https://cie.co.at/datatable/cie-spectral-luminous-efficiency-photopic-vision>
#' @source <https://files.cie.co.at/CIE S 026 alpha-opic Toolbox.xlsx>
#' @references CIE (2019). ISO/CIE 11664-1:2019(E). Colorimetry — Part 1: CIE
#'   standard colorimetric observers. Vienna, CIE
#' @references CIE (2018). CIE S 026/E:2018. CIE system for metrology of optical
#'   radiation for ipRGC-influenced responses of light. Vienna, CIE
"alphaopic.action.spectra"

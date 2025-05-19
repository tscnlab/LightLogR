#' Reconstruct spectral irradiance from sensor counts
#'
#' This function takes sensor data in the form of (normalized) counts and
#' reconstructs a spectral power distribution (SPD) through a calibration matrix.
#' The matrix takes the form of `sensor channel x wavelength`, and the spectrum
#' results form a linear combination of `counts x calibration-value` for any
#' wavelength in the matrix. Handles multiple sensor readings by returning a list of spectra
#'
#' Please note that calibration matrices are not provided by LightLogR, but can
#' be provided by a wearable device manufacturer. Counts can be normalized with
#' the [normalize_counts()] function, provided that the output also contains a
#' `gain` column.
#'
#' @param sensor_channels Named numeric vector or dataframe with
#'   sensor readings. Names must match calibration matrix columns.
#' @param calibration_matrix Matrix or dataframe with sensor-named columns and
#'   wavelength-indexed rows
#' @param format Output format: "long" (list of tibbles) or "wide" (dataframe)
#'
#' @return 
#' - "long": List of tibbles (wavelength, irradiance)
#' - "wide": Dataframe with wavelength columns and one row per spectrum
#'
#' @family Spectrum
#'
#' @export
#'
#' @examples
#' # Calibration matrix example
#' calib <- matrix(1:12, ncol=3, dimnames = list(400:403, c("R", "G", "B")))
#'
#' # Named vector input
#' spectral_reconstruction(c(R=1, G=2, B=3), calib)
#'
#' # Dataframe input
#' df <- data.frame(R=1, G=2, B=3, other_col=10)
#' spectral_reconstruction(dplyr::select(df, R:B), calib)
#' 
#' # Multiple spectra: as list columns
#' df <- data.frame(Measurement = c(1,2), R=c(1,2), G=c(2,4), B=c(3,6))
#' df <- 
#' df |> 
#'   dplyr::mutate(
#'       Spectrum = spectral_reconstruction(dplyr::pick(R:B), calib)
#'       )
#' df |> tidyr::unnest(Spectrum)
#' 
#' # Multiple spectra: as extended dataframes
#' df |> 
#'   dplyr::mutate(
#'       Spectrum = spectral_reconstruction(dplyr::pick(R:B), calib, "wide"))

spectral_reconstruction <- function(sensor_channels, 
                                    calibration_matrix,
                                    format = c("long", "wide")) {
  format <- match.arg(format)
  
  # Convert calibration matrix to tibble
  calibration_df <- tryCatch({
    dplyr::as_tibble(
      as.data.frame(calibration_matrix), 
      rownames = "wavelength"
    )
  }, error = function(e) stop("Invalid calibration matrix format"))
  
  # Validate calibration structure
  if (!"wavelength" %in% colnames(calibration_df)) {
    stop("Calibration matrix must have wavelength row names")
  }
  cal_cols <- setdiff(colnames(calibration_df), "wavelength")
  if (length(cal_cols) == 0) stop("Calibration matrix needs sensor columns")
  
  # Process sensor inputs
  if (is.data.frame(sensor_channels)) {
    sensor_data <- tryCatch(
      dplyr::select(sensor_channels, dplyr::all_of(cal_cols)),
      error = function(e) stop("Missing required sensor columns")
    )
    sensor_matrix <- as.matrix(sensor_data)
  } else if (is.numeric(sensor_channels) && !is.null(names(sensor_channels))) {
    if (!all(cal_cols %in% names(sensor_channels))) {
      stop("Sensor names mismatch with calibration columns")
    }
    sensor_matrix <- matrix(
      sensor_channels[cal_cols], 
      nrow = 1,
      dimnames = list(NULL, cal_cols)
    )
  } else {
    stop("sensor_channels must be named vector or dataframe")
  }
  
  # Matrix multiplication
  calib_values <- as.matrix(calibration_df[, cal_cols])
  irradiance_matrix <- calib_values %*% t(sensor_matrix)
  
  # Format output
  wavelengths <- as.numeric(calibration_df$wavelength)
  
  if (format == "wide") {
    wide_df <- as.data.frame(t(irradiance_matrix))
    colnames(wide_df) <- wavelengths
    return(wide_df)
  } else {
    result_list <- lapply(1:ncol(irradiance_matrix), function(i) {
      dplyr::tibble(
        wavelength = wavelengths,
        irradiance = irradiance_matrix[, i]
      )
    })
    if (ncol(irradiance_matrix) == 1) result_list[[1]] else result_list
  }
}
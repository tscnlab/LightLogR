#' Integrate spectral irradiance with optional weighting
#'
#' Integrates over a given spectrum, optionally over only a portion of the
#' spectrum, optionally with a weighing function. Can be used to calculate
#' spectral contributions in certain wavelength ranges, or to calculate
#' (alphaopically equivalent daylight) illuminance.
#'
#' The function uses trapezoidal integration and recognizes differing
#' step-widths in the spectrum. If an action spectrum is used, values of the
#' action spectrum at the spectral wavelenghts are interpolated with
#' [stats::approx()].
#' 
#' The used efficacies for for the auto-weighting are:
#' - photopic: 683.0015478 
#' - melanopic: 1/0.0013262
#' - rhodopic: 1/0.0014497
#' - l_cone_opic: 1/0.0016289
#' - m_cone_opic: 1/0.0014558
#' - s_cone_opic: 1/0.0008173
#' 
#' This requires input values in W/(m^2) for the spectrum. If it is provided in
#' other units, the result has to be rescaled afterwards.
#'
#' @param spectrum Tibble with spectral data (1st col: wavelength, 2nd col: SPD values)
#' @param wavelength.range Optional integration bounds (length-2 numeric)
#' @param action.spectrum Either: 
#'   - Tibble with wavelength and weighting columns
#'   - Name of built-in spectrum: "photopic", "melanopic", "rhodopic", 
#'     "l_cone_opic", "m_cone_opic", "s_cone_opic"
#' @param general.weight Scalar multiplier or "auto" for built-in efficacies
#'
#' @return Numeric integrated value
#' 
#' @family Spectrum
#' 
#' @export
#'
#' @examples
#' # creating an equal energy spectrum of value 1
#' spd <- data.frame(wl = 380:780, values = 1)
#' 
#' #integrating over the full spectrum
#' spectral_integration(spd)
#' 
#' #integrating over wavelengths 400-500 nm
#' spectral_integration(spd, wavelength.range = c(400, 500))
#' 
#' #calculating the photopic illuminance of an equal energy spectrum with 1 W/(m^2*nm)
#' spectral_integration(spd, action.spectrum = "photopic", general.weight = "auto")
#' 
#' #calculating the melanopic EDI of an equal energy spectrum with 1 W/(m^2*nm)
#' spectral_integration(spd, action.spectrum = "melanopic", general.weight = "auto")
#' 
#' # Custom action spectrum
#' custom_act <- data.frame(wavelength = 400:700, weight = 0.5)
#' spectral_integration(spd, wavelength.range = c(400,700), 
#'                      action.spectrum = custom_act, general.weight = 2)
#'                      
#' #using a spectrum that is broader then the action spectrum will not change the
#' #output, as the action spectrum will use zeros beyond its range
#' 
#' 
spectral_integration <- function(spectrum,
                                 wavelength.range = NULL,
                                 action.spectrum = NULL,
                                 general.weight = 1
                                 ) {
  
  # Validate spectrum structure
  if (ncol(spectrum) < 2) stop("Spectrum must have at least 2 columns")
  spd_cols <- names(spectrum)[1:2]
  spectrum <- stats::setNames(spectrum[1:2], c("wavelength", "spd"))
  
  # Set integration range
  if (is.null(wavelength.range)) {
    wavelength.range <- range(spectrum$wavelength)
  } else if (length(wavelength.range) != 2) {
    stop("wavelength.range must be length-2 numeric")
  }
  
  # Filter spectrum
  spec <- dplyr::filter(spectrum, 
                        wavelength >= wavelength.range[1],
                        wavelength <= wavelength.range[2])
  
  alphaopic.action.spectra <- LightLogR::alphaopic.action.spectra
  
  # Handle action spectrum
  if (!is.null(action.spectrum)) {
    if (is.character(action.spectrum)) {
      if (!action.spectrum %in% names(alphaopic.action.spectra)) {
        stop("Invalid built-in action spectrum")
      }
      act_spec <- alphaopic.action.spectra[, c("wavelength", action.spectrum)]
      act_spec <- stats::na.omit(act_spec)
    } else {
      if (!"wavelength" %in% names(action.spectrum)) {
        stop("Custom action.spectrum needs wavelength column")
      }
      weight_col <- setdiff(names(action.spectrum), "wavelength")
      if (length(weight_col) != 1) {
        stop("Action spectrum must contain exactly one weighting column")
      }
      act_spec <- action.spectrum
    }
    
    # Interpolate weights
    action_weight <- stats::approx(
      x = act_spec$wavelength,
      y = act_spec[[setdiff(names(act_spec), "wavelength")]],
      xout = spec$wavelength,
      rule = 1
    )$y
    action_weight[is.na(action_weight)] <- 0
  } else {
    action_weight <- 1
  }
  
  # Handle auto-weighting
  efficacy <- c(
    melanopic = 1/0.0013262,
    photopic = 683.0015478,
    rhodopic = 1/0.0014497,
    l_cone_opic = 1/0.0016289,
    m_cone_opic = 1/0.0014558,
    s_cone_opic = 1/0.0008173
  )
  
  if (identical(general.weight, "auto")) {
    if (!is.character(action.spectrum)) {
      stop("Auto-weighting only works with built-in action spectra")
    }
    general.weight <- efficacy[action.spectrum]
  }
  
  # Trapezoidal integration
  dx <- diff(spec$wavelength)
  avg_y <- (utils::head(spec$spd * action_weight, -1) + 
              utils::tail(spec$spd * action_weight, -1)) / 2
  (sum(dx * avg_y) * general.weight) |> unname()
}

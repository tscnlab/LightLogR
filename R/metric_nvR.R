
# Direct response ---------------------------------------------------------

#' Non-visual direct response
#' 
#' This function calculates the non-visual direct response (nvRD). It takes into account
#' the assumed response dynamics of the non-visual system and processes the light
#' exposure signal to quantify the effective direct input to the non-visual system 
#' (see Details).
#'
#' @param MEDI.vector Numeric vector containing the melanopic EDI data.
#' @param Illuminance.vector Numeric vector containing the Illuminance data.
#' @param Time.vector Vector containing the time data. Can be [POSIXct()],[hms::hms()], 
#'    [lubridate::duration()], [difftime()].
#' @param epoch The epoch at which the data was sampled. Can be either a
#'    [lubridate::duration()] or a string. If it is a string, it needs to be
#'    either `"dominant.epoch"` (the default) for a guess based on the data, or a valid
#'    [lubridate::duration()] string, e.g., `"1 day"` or `"10 sec"`.
#'
#' @return A numeric vector containing the nvRD data. The output has the same
#'    length as `Time.vector`.
#' @export
#' 
#' @family metrics
#' 
#' @details  The timeseries is assumed to be regular. Missing values in the
#'    light data will be replaced by 0.
#'
#' @references Amundadottir, M.L. (2016). Light-driven model for identifying
#'    indicators of non-visual health potential in the built environment
#'    \[Doctoral dissertation, EPFL\]. EPFL infoscience.
#'    \doi{10.5075/epfl-thesis-7146}
#'
#' @examples
#' 
#' # Dataset 1 with 24h measurement
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", 60 * 24),
#'     Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*24-1)),
#'     Illuminance = c(rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60)),
#'     MEDI = Illuminance * rep(sample(0.5:1.5, 24, replace = TRUE), each = 60)
#'   ) 
#' # Dataset 2 with 48h measurement
#' dataset2 <-
#'   tibble::tibble(
#'     Id = rep("B", 60 * 48),
#'     Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*48-1)),
#'     Illuminance = c(rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60), 
#'                     rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60)),
#'     MEDI = Illuminance * rep(sample(0.5:1.5, 48, replace = TRUE), each = 60)
#'   )
#' # Combined datasets
#' dataset.combined <- rbind(dataset1, dataset2)
#' 
#' # Calculate nvRD per ID
#' dataset.combined.nvRD <- dataset.combined %>% 
#'   dplyr::group_by(Id) %>% 
#'   dplyr::mutate(
#'     nvRD = nvRD(MEDI, Illuminance, Datetime)
#'   )
#' 
nvRD <- function(MEDI.vector,
                 Illuminance.vector,
                 Time.vector,
                 epoch = "dominant.epoch") {
  
  # Perform argument checks
  stopifnot(
    "`MEDI.vector` must be numeric!" = is.numeric(MEDI.vector),
    "`Illuminance.vector` must be numeric!" = is.numeric(Illuminance.vector),
    "`Time.vector` must be POSIXct, hms, duration, or difftime!" =
      lubridate::is.POSIXct(Time.vector) | hms::is_hms(Time.vector) | 
      lubridate::is.duration(Time.vector) | lubridate::is.difftime(Time.vector),
    "`MEDI.vector` and `Time.vector` must be same length!" = 
      length(MEDI.vector) == length(Time.vector),
    "`epoch` must either be a duration or a string" =
      lubridate::is.duration(epoch) | is.character(epoch)
  )
  
  # Replace missing values
  if (any(is.na(MEDI.vector) | is.na(Illuminance.vector))) {
    warning("Light data contains missing values! They are replaced by 0.")
    MEDI.vector[is.na(MEDI.vector)] <- 0
    Illuminance.vector[is.na(Illuminance.vector)] <- 0
  }
  
  # Spectral sensitivity
  zeff <- 310 / 683 / 118.5 / 1.42 * 0.91 # from Illuminance.vector to melanopic eff. irr (F11)
  
  # Intensity response
  EI50 <- 106 # [lx]
  slope <- 3.55
  
  # Get the epochs based on the data
  if (is.character(epoch) && epoch == "dominant.epoch") {
    epoch <- count_difftime(tibble::tibble(Datetime = Time.vector))$difftime[1]
  }
  # If the user specified an epoch, use that instead
  else {
    epoch <- lubridate::as.duration(epoch)
  }
  
  # Sampling interval in hours
  delta <- as.numeric(epoch, unit = "hours")
  
  # Convert to effective irradiance
  Ieff <- nvR_getIeff(MEDI.vector, Illuminance.vector, delta)
  
  # Filter L2
  dFL2 <- ifelse(delta < 2.3, round(2.3 / delta), 1)
  
  # Filter L1
  dFL1 <- ifelse(delta < 0.3, round(0.3 / delta), 1)
  
  # Filter LH
  dFLH <- ifelse(delta < 1.7, round(1.7 / delta), 1)
  
  # MODEL
  u <- nvR_filterSMA(dFL1, Ieff)
  v <- nvR_adaptiveResponse(u, Illuminance.vector, EI50 * zeff, slope, dFLH, FALSE)
  RD <- nvR_filterEMA(dFL2, v)
  
  return(RD)
}

#' Cumulative non-visual direct response
#' 
#' This function calculates the cumulative non-visual direct response (nvRD). This is
#' basically the integral of the nvRD over the provided time period in hours. The
#' unit of the resulting value thus is "nvRD*h".
#'
#' @param nvRD Numeric vector containing the non-visual direct response. 
#'    See \code{\link{nvRD}}.
#' @param Time.vector Vector containing the time data. Can be \link[base]{POSIXct}, \link[hms]{hms}, 
#'    \link[lubridate]{duration}, or \link[base]{difftime}.
#' @param epoch The epoch at which the data was sampled. Can be either a
#'    \link[lubridate]{duration} or a string. If it is a string, it needs to be
#'    either `"dominant.epoch"` (the default) for a guess based on the data, or a valid
#'    \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`.
#' @param as.df Logical. Should a data frame with be returned? If `TRUE`, a data
#'    frame with a single column named `nvRD_cumulative` will be returned.
#'    Defaults to `FALSE`.
#'
#' @return A numeric value or single column data frame. 
#' @export
#' 
#' @family metrics
#'
#' @references Amundadottir, M.L. (2016). Light-driven model for identifying
#'    indicators of non-visual health potential in the built environment
#'    \[Doctoral dissertation, EPFL\]. EPFL infoscience.
#'    \doi{10.5075/epfl-thesis-7146}
#'
#' @examples
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("A", 60 * 24),
#'     Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*24-1)),
#'     Illuminance = c(rep(0, 60*8), rep(sample(1:1000, 14, replace = TRUE), each = 60), rep(0, 60*2)),
#'     MEDI = Illuminance * rep(sample(0.5:1.5, 24, replace = TRUE), each = 60)
#'   ) %>%
#'   dplyr::mutate(
#'     nvRD = nvRD(MEDI, Illuminance, Datetime)
#'   ) 
#' dataset1 %>% 
#'   dplyr::summarise(
#'     "cumulative nvRD" = nvRD_cumulative_response(nvRD, Datetime)
#'   )
#' 
nvRD_cumulative_response <- function(nvRD,
                                     Time.vector,
                                     epoch = "dominant.epoch",
                                     as.df = FALSE) {
  
  # Perform argument checks
  stopifnot(
    "`nvRD` must be numeric!" = is.numeric(nvRD),
    "`Time.vector` must be POSIXct, hms, duration, or difftime!" =
      lubridate::is.POSIXct(Time.vector) | hms::is_hms(Time.vector) | 
      lubridate::is.duration(Time.vector) | lubridate::is.difftime(Time.vector),
    "`nvRD` and `Time.vector` must be same length!" = 
      length(nvRD) == length(Time.vector),
    "`epoch` must either be a duration or a string" =
      lubridate::is.duration(epoch) | is.character(epoch),
    "`as.df` must be logical!" = is.logical(as.df)
  )
  
  # Get the epochs based on the data
  if (is.character(epoch) && epoch == "dominant.epoch") {
    epoch <- count_difftime(tibble::tibble(Datetime = Time.vector))$difftime[1]
  }
  # If the user specified an epoch, use that instead
  else {
    epoch <- lubridate::as.duration(epoch)
  }
  
  # Calculate cumulative nvRD
  nvRD_CR <- (sum(nvRD) * as.numeric(epoch)) / 3600
  
  # Return as vector or data frame
  if (as.df) {
    return(tibble::tibble("nvRD_cumulative" = nvRD_CR))
  } else {
    return(nvRD_CR)
  }
}

# Circadian response ------------------------------------------------------

#' Non-visual circadian response
#' 
#' This function calculates the non-visual circadian response (nvRC). It takes into account
#' the assumed response dynamics of the non-visual system and the circadian rhythm
#' and processes the light exposure signal to quantify the effective circadian-weighted
#' input to the non-visual system (see Details).
#'
#' @param MEDI.vector Numeric vector containing the melanopic EDI data.
#' @param Illuminance.vector Numeric vector containing the Illuminance data.
#' @param Time.vector Vector containing the time data. Can be \link[base]{POSIXct},
#'    \link[hms]{hms}, \link[lubridate]{duration}, or \link[base]{difftime}.
#' @param epoch The epoch at which the data was sampled. Can be either a
#'    \link[lubridate]{duration} or a string. If it is a string, it needs to be
#'    either `"dominant.epoch"` (the default) for a guess based on the data, or a valid
#'    \link[lubridate]{duration} string, e.g., `"1 day"` or `"10 sec"`.
#' @param sleep.onset The time of habitual sleep onset. Can be HMS, numeric, or NULL.
#'    If NULL (the default), then the data is assumed to start at habitual sleep onset.
#'    If `Time.vector` is HMS or POSIXct, `sleep.onset` must be HMS. Likewise, if
#'    `Time.vector` is numeric, `sleep.onset` must be numeric.
#'
#' @return A numeric vector containing the nvRC data. The output has the same
#'    length as `Time.vector`.
#' @export
#' 
#' @family metrics
#' 
#' @details  The timeseries is assumed to be regular. Missing values in the
#'    light data will be replaced by 0.
#'    
#'
#' @references Amundadottir, M.L. (2016). Light-driven model for identifying
#'    indicators of non-visual health potential in the built environment
#'    \[Doctoral dissertation, EPFL\]. EPFL infoscience.
#'    \doi{10.5075/epfl-thesis-7146}
#'
#' @examples
#' 
#' dataset1 <-
#'   tibble::tibble(
#'     Id = rep("B", 60 * 48),
#'     Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*48-1)),
#'     Illuminance = c(rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60),
#'                     rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60)),
#'     MEDI = Illuminance * rep(sample(0.5:1.5, 48, replace = TRUE), each = 60)
#'   )
#' # Time.vector as POSIXct
#' dataset1.nvRC <- dataset1 %>%
#'   dplyr::mutate(
#'     nvRC = nvRC(MEDI, Illuminance, Datetime, sleep.onset = hms::as_hms("22:00:00"))
#'   )
#' 
#' # Time.vector as difftime
#' dataset2 <- dataset1 %>% 
#'   dplyr::mutate(Datetime = Datetime - lubridate::as_datetime(lubridate::dhours(22)))
#' dataset2.nvRC <- dataset2 %>%
#'   dplyr::mutate(
#'     nvRC = nvRC(MEDI, Illuminance, Datetime, sleep.onset = lubridate::dhours(0))
#'   )
#'   
nvRC <- function(MEDI.vector,
                 Illuminance.vector,
                 Time.vector,
                 epoch = "dominant.epoch",
                 sleep.onset = NULL) {
  
  # Perform argument checks
  stopifnot(
    "`MEDI.vector` must be numeric!" = is.numeric(MEDI.vector),
    "`Illuminance.vector` must be numeric!" = is.numeric(Illuminance.vector),
    "`Time.vector` must be POSIXct, hms, duration, or difftime!" =
      lubridate::is.POSIXct(Time.vector) | hms::is_hms(Time.vector) | 
      lubridate::is.duration(Time.vector) | lubridate::is.difftime(Time.vector),
    "`MEDI.vector` and `Time.vector` must be same length!" = 
      length(MEDI.vector) == length(Time.vector),
    "`epoch` must either be a duration or a string" =
      lubridate::is.duration(epoch) | is.character(epoch),
    "`sleep.onset` must be hms, duration, difftime, or NULL" = 
      is.null(sleep.onset) | hms::is_hms(sleep.onset) | 
      lubridate::is.duration(sleep.onset) | lubridate::is.difftime(sleep.onset)
  )
  
  # Replace missing values
  if (any(is.na(MEDI.vector) | is.na(Illuminance.vector))) {
    warning("Light data contains missing values! They are replaced by 0.")
    MEDI.vector[is.na(MEDI.vector)] <- 0
    Illuminance.vector[is.na(Illuminance.vector)] <- 0
  }
  
  # Spectral sensitivity
  zeff <- 310 / 683 / 118.5 / 1.42 * 0.91 # from Illuminance.vector to melanopic eff. irr (F11)
  
  # Intensity response
  EI50 <- 119 # [lx]
  slope <- 1.42
  
  # Get the epochs based on the data
  if (is.character(epoch) && epoch == "dominant.epoch") {
    epoch <- count_difftime(tibble::tibble(Datetime = Time.vector))$difftime[1]
  }
  # If the user specified an epoch, use that instead
  else {
    epoch <- lubridate::as.duration(epoch)
  }
  
  # Sampling interval in hours
  delta <- as.numeric(epoch, unit = "hours")
  
  # Convert to effective irradiance
  Ieff <- nvR_getIeff(MEDI.vector, Illuminance.vector, delta)
  
  if(!is.null(sleep.onset)){
    bt = as.numeric(sleep.onset)
    
    if(hms::is.hms(sleep.onset)){
      stopifnot("`sleep.onset` is HMS but `Time.vector` is not HMS or POSIXct" = 
                  hms::is_hms(Time.vector) | lubridate::is.POSIXct(Time.vector))
      # Normalise datetime vector
      dt <- as.numeric(Time.vector) - as.numeric(Time.vector)[1] +
        as.numeric(hms::as_hms(Time.vector[1]))
    }
    else{
      stopifnot("`sleep.onset` is duration or difftime but `Time.vector` is not duration or difftime" = 
                  lubridate::is.duration(Time.vector) | lubridate::is.difftime(Time.vector))
      dt <- as.numeric(Time.vector)
    }
    
    # Seconds to hours
    t = (dt - bt) / 3600
  }
  else {
    # Check whether first three hours are darkness --> timeseries should start at
    # habitual sleep onset.
    if (mean(Illuminance.vector[1:(3 / delta)]) > 10) {
      warning("Average Illuminance.vector across the first three hours is higher than 10lx. Does the timeseries really start at habitual sleep onset?")
    }
    t <- seq(0, (length(Ieff) * delta) - delta, delta)
  }
  
  
  # Get circadian modulation
  Rmax <- nvR_circadianModulator(t)
  
  # Filter L2
  dFL2 <- ifelse(delta < 5.7, round(5.7 / delta), 1)
  
  # Filter L1
  dFL1 <- ifelse(delta < 0.6, round(0.6 / delta), 1)
  
  # Filter LH
  dFLH <- ifelse(delta < 3.7, round(3.7 / delta), 1)
  
  # MODEL
  u <- nvR_filterSMA(dFL1, Ieff)
  v <- nvR_adaptiveResponse(u, Illuminance.vector, EI50 * zeff, slope, dFLH, FALSE) * Rmax
  nvRC <- nvR_filterEMA(dFL2, v)
  
  return(nvRC)
}

#' Performance metrics for circadian response
#' 
#' These functions compare the non-visual circadian response (see \code{\link{nvRC}})
#' for measured personal light exposure to the nvRC for a reference light exposure pattern,
#' such as daylight.
#'
#' @param nvRC Time series of non-visual circadian response 
#'    (see \code{\link{nvRC}}.
#' @param nvRC.ref Time series of non-visual circadian response 
#'    circadian response (see \code{\link{nvRC}} for a reference light exposure 
#'    pattern (e.g., daylight). Must be the same length as `nvRC`.
#' @param as.df Logical. Should the output be returned as a data frame? Defaults
#'    to TRUE.
#'
#' @return A numeric value or single column data frame. 
#'
#' @references Amundadottir, M.L. (2016). Light-driven model for identifying
#'    indicators of non-visual health potential in the built environment
#'    \[Doctoral dissertation, EPFL\]. EPFL infoscience.
#'    \doi{10.5075/epfl-thesis-7146}
#'
#' @name nvRC_metrics
#' @examples
#' 
#' dataset1 <- 
#'   tibble::tibble(
#'     Id = rep("B", 60 * 24),
#'     Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*24-1)),
#'     Illuminance = c(rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60)),
#'     MEDI = Illuminance * rep(sample(0.5:1.5, 24, replace = TRUE), each = 60),
#'   ) %>%
#'   dplyr::mutate(
#'     nvRC = nvRC(MEDI, Illuminance, Datetime, sleep.onset = hms::as_hms("22:00:00"))
#'   )
#' 
#' dataset.reference <-
#'   tibble::tibble(
#'     Id = rep("Daylight", 60 * 24),
#'     Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*24-1)),
#'     Illuminance = c(rep(0, 60*6), rep(10000, 12*60), rep(0, 60*6)),
#'     MEDI = Illuminance
#'   ) %>%
#'   dplyr::mutate(
#'     nvRC = nvRC(MEDI, Illuminance, Datetime, sleep.onset = hms::as_hms("22:00:00"))
#'   )
#' 
#' # Circadian disturbance
#' nvRC_circadianDisturbance(dataset1$nvRC, dataset.reference$nvRC)
#' 
#' # Circadian bias
#' nvRC_circadianBias(dataset1$nvRC, dataset.reference$nvRC)
#' 
#' # Relative amplitude error
#' nvRC_relativeAmplitudeError(dataset1$nvRC, dataset.reference$nvRC)



#' @rdname nvRC_metrics
#'
#' @details 
#'    `nvRC_circadianDisturbance()` calculates the circadian disturbance (CD).
#'    It is expressed as 
#'    
#'    \deqn{CD(i,T)=\frac{1}{T}\int_{t_{i}}^{t_{i}+T}
#'    {\lvert r_{C}(t)-r_{C}^{ref}(t)\rvert dt},}
#'    
#'    and quantifies the total difference between the measured circadian response
#'    and the circadian response to a reference profile.
#'
#' @export
#'
nvRC_circadianDisturbance <- function(nvRC, 
                                      nvRC.ref, 
                                      as.df = FALSE) {
  
  # Perform argument checks
  stopifnot(
    "`nvRC` must be numeric!" = is.numeric(nvRC),
    "`nvRC.ref` must be numeric!" = is.numeric(nvRC.ref),
    "Inputs must be same length" = length(nvRC) == length(nvRC.ref),
    "`as.df` must be logical!" = is.logical(as.df)
  )
  
  # Calculate circadian disturbance
  cd <- sum(abs(nvRC - nvRC.ref)) / length(nvRC)
  
  # Return vector or data frame
  if (as.df) {
    return(tibble::tibble("nvRC_CD" = cd))
  } else {
    return(cd)
  }
}

#' @rdname nvRC_metrics
#'
#' @details 
#'    `nvRC_circadianBias()` calculates the circadian bias (CB).
#'    It is expressed as 
#'    
#'    \deqn{CB(i,T)=\frac{1}{T}\int_{t_{i}}^{t_{i}+T}
#'    {(r_{C}(t)-r_{C}^{ref}(t))dt},}
#'    
#'    and provides a measure of the overall trend for the difference in 
#'    circadian response, i.e. positive values for overestimating and negative 
#'    for underestimating between the measured circadian response
#'    and the circadian response to a reference profile.
#'
#' @export
#'
nvRC_circadianBias <- function(nvRC, 
                               nvRC.ref, 
                               as.df = FALSE) {
  # Perform argument checks
  stopifnot(
    "`nvRC` must be numeric!" = is.numeric(nvRC),
    "`nvRC.ref` must be numeric!" = is.numeric(nvRC.ref),
    "Inputs must be same length" = length(nvRC) == length(nvRC.ref),
    "`as.df` must be logical!" = is.logical(as.df)
  )
  
  # Calculate circadian bias
  cb <- sum(nvRC - nvRC.ref) / length(nvRC)
  
  # Return vector or data frame
  if (as.df) {
    return(tibble::tibble("nvRC_CB" = cb))
  } else {
    return(cb)
  }
}

#' @rdname nvRC_metrics
#'
#' @details 
#'    `nvRC_relativeAmplitudeError()` calculates the relative amplitude error (RAE).
#'    It is expressed as
#'    
#'    \deqn{RAE(i,T)=r_{C,max}-r_{C,max}^{ref},}
#'    
#'    and quantifies the difference between the maximum response achieved in a period 
#'    to the reference signal.
#'
#' @export
#'
nvRC_relativeAmplitudeError <- function(nvRC, 
                                        nvRC.ref, 
                                        as.df = FALSE) {
  
  # Perform argument checks
  stopifnot(
    "`nvRC` must be numeric!" = is.numeric(nvRC),
    "`nvRC.ref` must be numeric!" = is.numeric(nvRC.ref),
    "Inputs must be same length" = length(nvRC) == length(nvRC.ref),
    "`as.df` must be logical!" = is.logical(as.df)
  )
  
  # Calculate RAE
  rae <- max(nvRC) - max(nvRC.ref)
  
  # Return vector or data frame
  if (as.df) {
    return(tibble::tibble("nvRC_RAE" = rae))
  } else {
    return(rae)
  }
}

# Helper Functions --------------------------------------------------------

# Adaptive response: adaptation of sensitivity due to prior light history
nvR_adaptiveResponse <- function(u, lux, sigma, n, dFLH, FLOOR) {
  H <- nvR_filterSMA(dFLH, log10(lux+1))
  H[H < 0] <- 0
  if (FLOOR) {
    H[H > 0] <- floor(H[H > 0])
  }
  sigma <- sigma * 2^(H - 1)
  R <- nvR_relativeResponse(u, sigma, n)
  return(R)
}

# Relative response: intensity-response curve based on melatonin suppression
nvR_relativeResponse <- function(u, sigma, n) {
  1 / (1 + (sigma / u)^n)
}

# Circadian modulator
nvR_circadianModulator <- function(t) {
  DAY <- 24
  phi_XcX <- -2.98 # rad - redefinition of CBTmin
  theta <- pi + phi_XcX / 2
  Cm <- (1 - 0.4 * cos(2 * pi / DAY * t + theta)) *
    (1 - 0.4 * -sin(2 * pi / DAY * t + theta))
  return(Cm)
}

# EMA filter
nvR_filterEMA <- function(d, I) {
  if(d > 1){
    alpha <- 2 / (d + 1) # calculate smoothing factor "alpha"
    coefficient <- rep(1 - alpha, d)^(d:1) # note 1-alpha
    I <- c(rep(0, d - 1), I)
    R <- slider::slide_vec(.x = I, .f = \(x) sum(x * coefficient) / sum(coefficient),
                           .before = d-1, .after = 0, .complete = TRUE)
    R <- R[-c(1:(d-1))]
  }
  else{
    R <- I
  }
  R[R < 0] <- 0
  return(R)
}

# SMA filter
nvR_filterSMA <- function(d, I) {
  if(d > 1){
    I <- c(rep(0, d - 1), I)
    R <- slider::slide_vec(.x = I, .f = mean, .before = d-1, .after = 0, 
                           .complete = TRUE)
    R <- R[-c(1:(d-1))]
  }
  else{
    R <- I
  }
  R[R < 0] <- 0
  return(R)
}

# Get effective irradiance for non-visual responses
nvR_getIeff <- function(MEDI, Illuminance, delta, shiftModel = FALSE) {
  A_mel <- 97.07 # AUC of melanopic sensitivity curve
  A_v <- 118.5 # AUC of v_lambda
  K_v <- 683 # luminous coefficient
  K_mel_D65 <- 1.3262/1000 # melanopic EDI coefficient
  
  Ieff_mel <- (MEDI * K_mel_D65 / A_mel) * 310
  Ieff_v <- (Illuminance / K_v / A_v) * 310
  
  if (shiftModel) {
    #   ON = which(I>0)
    #   Duration = length(I[ON[1]:length(I)]) * delta - delta
    #
    #   Ieff = rep(0,length(I))
    #   Ieff[ON[1]:length(I)] =
    #     I[ON[1]:length(I)] * RSE_e_v * exp(seq(0,Duration,delta) * a) +
    #     I[ON[1]:length(I)] * RSE_e_ipRGC * (1-exp(seq(0,Duration,delta) * a))
    Ieff <- Ieff_mel
  } else {
    Ieff <- Ieff_mel
  }
  
  return(Ieff)
}

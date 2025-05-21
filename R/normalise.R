#' Add a defined number to a numeric and log transform it
#'
#' Frequently, light exposure data need to be log-transformed. Because light
#' exposure data frequently also contain many zero-values, adding a small value
#' avoids losing those observations. Must be applied with care and reported.
#'
#' @param x A numeric vector
#' @param offset the amount to add to `x`, by default `0.1`
#' @param base The logarithmic base, by default `10`
#'
#' @returns a transformed numeric vector
#' @export
#' 
#' @references Johannes Zauner, Carolina Guidolin, Manuel Spitschan (2025) How to deal with darkness: Modelling and visualization of zero-inflated personal light exposure data on a logarithmic scale. bioRxiv. doi: https://doi.org/10.1101/2024.12.30.630669
#'
#' @examples
#' c(0, 1, 10, 100, 1000, 10000) |> log_zero_inflated()
#' 
#' #For use in a function
#' sample.data.environment |> 
#'   dplyr::filter(Id == "Participant") |> 
#'   dplyr::group_by(Date = lubridate::wday(Datetime, label = TRUE, week_start = 1)) |> 
#'   dplyr::summarize(
#'   TAT250 = duration_above_threshold(log_zero_inflated(MEDI), 
#'                                     Datetime, 
#'                                     threshold = log_zero_inflated(250)
#'                                     )
#'                    )
#'                    
log_zero_inflated <- function(x, offset = 0.1, base = 10) {
  if(!is.numeric(x)) stop("x must be numeric")
  log(x+offset, base = base)
}

#' Exponentiate a numeric and subtract a defined number
#'
#' [exp_zero_inflated()] is the reverse function to [log_zero_inflated()].
#'
#' @rdname log_zero_inflated
#' @export
#' 
#' @examples 
#' 
#' #Calling exp_zero_inflated on data transformed with log_zero_inflated yields to the original result
#' c(0, 1, 10, 100, 1000, 10000) |> log_zero_inflated() |> exp_zero_inflated()
exp_zero_inflated <- function(x, offset = 0.1, base = 10) {
  if(!is.numeric(x)) stop("x must be numeric")
  base^x - offset
}
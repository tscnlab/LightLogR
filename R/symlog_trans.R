#' Scale positive and negative values on a log scale
#' 
#' To create a plot with positive and negative (unscaled) values on a log-transformed axis, the values need to be scaled accordingly. R or \pkg{ggplot2} do not have a built-in function for this, but the following function can be used to create a transformation function for this purpose. The function was coded based on a [post on stack overflow](https://stackoverflow.com/a/14674703). The `symlog` transformation is the standard transformation used e.g., in [gg_day()].
#' 
#' The `symlog` transformation can be accessed either via the `trans = "symlog"` argument in a scaling function, or via `trans = symlog_trans()`. The latter allows setting the individual arguments.
#'
#' @param base Base for the logarithmic transformation. The default is 10.
#' @param thr Threshold after which a logarithmic transformation is applied. If the absolute value is below this `threshold`, the value is not transformed. The default is 1.
#' @param scale Scaling factor for logarithmically transformed values above the `threshold`. The default is 1.
#'
#' @return a transformation function that can be used in \pkg{ggplot2} or \pkg{plotly} to scale positive and negative values on a log scale.
#' @export
#'
#' @references This function`s code is a straight copy from a post on [stack overflow](https://stackoverflow.com/a/14674703).
#' The author of the answer is [Julius Vainora](https://stackoverflow.com/users/1320535/julius-vainora), and the author of the question [Brian B](https://stackoverflow.com/users/1212562/brian-b)
#'
#' @examples
#' dataset <- 
#' sample.data.environment %>%
#' filter_Date(end = "2023-08-29") %>% 
#' dplyr::mutate(MEDI = dplyr::case_when(
#'                                      Id == "Environment" ~ -MEDI,
#'                                      .default = MEDI))
#' #basic application where transformation, breaks and labels are set manually
#' dataset %>%                                     
#' gg_day(aes_col = Id) +
#' ggplot2::scale_y_continuous(
#' trans = "symlog")
#'
#' #the same plot, but with breaks and labels set manually                            
#' dataset %>%                                     
#' gg_day(aes_col = Id) +
#' ggplot2::scale_y_continuous(
#' trans = "symlog", 
#' breaks = c(-10^(5:0), 0, 10^(0:5)),
#' labels = function(x) format(x, scientific = FALSE, big.mark = " "))
#' 
#' #setting individual arguments of the symlog function manually allows
#' #e.g., to emphasize values smaller than 1
#' dataset %>%                                     
#' gg_day(aes_col = Id) +
#' ggplot2::scale_y_continuous(
#' trans = symlog_trans(thr = 0.01),
#' breaks = c(-10^(5:-1), 0, 10^(-1:5)),
#' labels = function(x) format(x, scientific = FALSE, big.mark = " "))
#' 
symlog_trans <- function(base = 10, thr = 1, scale = 1){
  trans <- function(x)
    ifelse(abs(x) < thr, x, sign(x) * 
             (thr + scale * (log(sign(x) * x / thr, base))))
  
  inv <- function(x)
    ifelse(abs(x) < thr, x, sign(x) * 
             base^((sign(x) * x - thr) / scale) * thr)
  
  breaks <- function(x){
    sgn <- sign(x[which.max(abs(x))])
    if(all(abs(x) < thr))
      scales::pretty_breaks()(x)
    else if(prod(x) >= 0){
      if(min(abs(x)) < thr)
        sgn * unique(c(scales::pretty_breaks()(c(min(abs(x)), thr)),
                       scales::log_breaks(base)(c(max(abs(x)), thr))))
      else
        sgn * scales::log_breaks(base)(sgn * x)
    } else {
      if(min(abs(x)) < thr)
        unique(c(sgn * scales::log_breaks()(c(max(abs(x)), thr)),
                 scales::pretty_breaks()(c(sgn * thr, x[which.min(abs(x))]))))
      else
        unique(c(-scales::log_breaks(base)(c(thr, -x[1])),
                 scales::pretty_breaks()(c(-thr, thr)),
                 scales::log_breaks(base)(c(thr, x[2]))))
    }
  }
  scales::trans_new(paste("symlog", thr, base, scale, sep = "-"), trans, inv, breaks)
}
#' Double Plots
#'
#' [gg_doubleplot()] is a wrapper function for [gg_days()], combined with an internal function to duplicate and reorganize dates in a dataset for a *double plot* view. This means that the same day is displayed multiple times within the plot in order to reveal pattern across days. 
#' 
#' @description
#' The function is by default opinionated, and will automatically select the best way to display the double date plot. However, the user can also manually select the type of double date plot to be displayed: repeating each day (default when there is only one day in all of the groups), or displaying consecutive days (default when there are multiple days in the groups).
#' 
#' @inheritParams cut_Datetime
#' @param type One of "auto", "repeat", or "next". The default is "auto", which will automatically select the best way to display the double date plot based on the amount of days in the dataset (`all = 1 >> "repeat", else "next`). "repeat" will repeat each day in the plot, and "next" will display consecutive days.
#' @param geom The type of geom to be used in the plot. The default is "ribbon".
#' @param alpha,linewidth The alpha and linewidth setting of the geom. The default is 0.5 and 0.4, respectively.
#' @param col,fill The color and fill of the geom. The default is "#EFC000FF". If the parameters `aes_col` or `aes_fill` are used through `...`, these will override the respective `col` and `fill` parameters.
#' @param x.axis.breaks.next,x.axis.breaks.repeat Datetime breaks when consecutive days are displayed (`type = "next"`) or days are repeated (`type = "repeat"`). Must be a function. The default for `next` is a label at 12:00 am of each day, and for `repeat` is a label every 6 hours.
#' @param x.axis.format.next,x.axis.format.repeat Datetime label format when consecutive days are displayed (`type = "next"`) or days are repeated (`type = "repeat"`). The default for `next` is `"%a %D"`, showing the date, and for `repeat` ist `"%H:%M"`, showing hours and minutes. See [base::strptime()] for more options.
#' @param ... Arguments passed to [gg_days()]. When the arguments `aes_col` and `aes_fill` are used, they will invalidate the `col` and `fill` parameters.
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' #take only the Participant data from sample data, and three days
#' library(dplyr)
#' library(lubridate)
#' library(ggplot2)
#' sample.data <- 
#' sample.data.environment %>% 
#' dplyr::filter(Id == "Participant") %>% 
#' filter_Date(length = ddays(3))
#' 
#' #create a double plot with the default settings
#' sample.data %>% gg_doubleplot()
#' 
#' #repeat the same day in the plot
#' sample.data %>% gg_doubleplot(type = "repeat")
#' 
#' #use the function with more than one Id
#' sample.data.environment %>% 
#' filter_Date(length = ddays(3)) %>% 
#' gg_doubleplot(aes_fill = Id, aes_col = Id) +
#' facet_wrap(~ Date.data, ncol = 1, scales = "free_x", strip.position = "left")
#' 
#' #if data is already grouped by days, type = "repeat" will be automatic
#' sample.data.environment %>% 
#' dplyr::group_by(Date = date(Datetime), .add = TRUE) %>% 
#' filter_Date(length = ddays(3)) %>% 
#' gg_doubleplot(aes_fill = Id, aes_col = Id) + 
#' guides(fill = "none", col = "none") + #remove the legend
#' facet_wrap(~ Date.data, ncol = 1, scales = "free_x", strip.position = "left")
#' 
#' #combining `aggregate_Date()` with `gg_doubleplot()` easily creates a good
#' #overview of the data
#' sample.data.environment %>%
#' aggregate_Date() %>%
#' gg_doubleplot()

gg_doubleplot <- function(dataset,
                          Datetime.colname = Datetime,
                          type = c("auto", "repeat", "next"),
                          geom = "ribbon",
                          alpha = 0.5,
                          col = "grey40",
                          fill = "#EFC000FF",
                          linewidth = 0.4,
                          x.axis.breaks.next = Datetime_breaks,
                          x.axis.format.next = "%a %D",
                          x.axis.breaks.repeat = 
                            ~Datetime_breaks(
                              .x, by = "6 hours",
                              shift = lubridate::duration(0, "hours")
                            ),
                          x.axis.format.repeat = "%H:%M",
                          ...) {
  
  # Initial Checks ----------------------------------------------------------
  # Match input arguments
  type <- match.arg(type)
  
  x <- rlang::enexpr(Datetime.colname) 
  stopifnot(
    "The given dataset is not a dataframe" = is.data.frame(dataset),
    "The Datetime column must be part of the dataset" = 
      rlang::as_string(x) %in% names(dataset),
    "The given Datetime column is not a Datetime format" =
      lubridate::is.POSIXct(dataset[[rlang::as_string(x)]])
    )
  
  # Function ----------------------------------------------------------

  #select the type of double date
  type <- double_selector(dataset, {{ Datetime.colname }}, type)
  
  x.axis.breaks <- x.axis.breaks.next
  x.axis.format <- x.axis.format.next
  
  #Change the axis type in case of a repeat
  if(type == "repeat") {
    x.axis.breaks <- x.axis.breaks.repeat
    x.axis.format <-  x.axis.format.repeat
  }
  
  dots2 <- rlang::enexprs(...)
  
  #potential color data
  aesthetics <- list(
    col = if(!("aes_col" %in% names(dots2))) col,
    fill = if(!("aes_fill" %in% names(dots2))) fill
  )

  #remove NULL values from aesthetics
  aesthetics <- aesthetics[!sapply(aesthetics, is.null)]
  
  #Plot generation
  rlang::inject(
  dataset %>% 
    double_date(Datetime.colname = {{ Datetime.colname }}, type = type) %>% 
    gg_days(
    geom = geom,
    alpha = alpha,  
    !!!aesthetics,
    linewidth = linewidth, 
    x.axis.breaks = x.axis.breaks,
    x.axis.format = x.axis.format,
    x.axis.limits = 
      \(x) Datetime_limits(
        x, length = lubridate::ddays(1), midnight.rollover = TRUE
        ),
    ...
  ))
}
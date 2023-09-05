
#' Create a simple plot of light logger data, facetted by Day
#'
#' `gg_day` will create a simple ggplot for every data in a dataset. The result
#' can further be manipulated like any ggplot. This will be sensible to refine
#' styling or guides.
#'
#' @param dataset light logger dataset. Expects a `dataframe`.
#' @param x.axis column name that contains the timedate. Expects a `symbol`.
#'   Needs to be part of the `dataset`.
#' @param y.axis column name that contains the lightlevel. Expects a `symbol`.
#'   Needs to be part of the `dataset`.
#' @param x.axis.label label for the x-axis. Expects a `character`.
#' @param y.axis.label label for the y-axis. Expects a `character`.
#' @param format.day Label for each day. Default is `%d/%m`, which shows the day
#'   and month. Expects a `character`. For an overview of sensible options look
#'   at [strptime]
#' @param title Plot title. Expects a `character`.
#' @param subtitle Plot subtitle. Expects a `character`.
#' @param start.date Choose an optional start date within your `dataset`.
#'   Expects a `date`, which can also be a `character` that is interpretable as
#'   a date, e.g., `"2023-06-03"`. Defaults to `NULL`, which means that the plot
#'   starts with the earliest date within the `dataset`.
#' @param end.date Choose an optional end date within your `dataset`. see
#'   `start.date` for additional details.
#' @param scales For [ggplot2::facet_wrap], should scales be "fixed", "free" or
#'   free in one dimension ("free_y" is the default). Expects a `character`.
#' @param y.scale.log10 Should `y` be scaled on a log10 basis? Expects a
#'   `logical`.
#' @param key optional column name that defines separate sets. Expects a
#'   `symbol`. Needs to be part of the `dataset`. By default, the
#'   [ggplot2::scale_color_viridis_d] is used. This can be overwritten outside
#'   the function (see examples).
#' @param y.axis.breaks Where should breaks occur on the y.axis? Expects a
#'   `numeric vector` with all the breaks. If you want to activate the default
#'   behaviour of `ggplot2`, you need to put in `ggplot2::waiver()`.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' filepath <- system.file("extdata/sample_data_LYS.csv", package = "LightLogR")
#' sampledata <- import.LYS(filepath, file.id = TRUE)
#' gg_day(sampledata, x.axis = Datetime, y.axis = mEDI, y.axis.label = "mEDI (lx)", title = "Nonvisual Stimulus for 2 Days")
#'
#' #use `key`for separation of different sets
#' plot <- gg_day(
#' sample.data.environment,
#' scales = "fixed",
#' x.axis = Datetime,
#' y.axis = mEDI,
#' y.axis.label = "mEDI (lx)",
#' key = Source)
#' plot
#'
#' #you can easily overwrite the color scale afterwards
#' plot + ggplot2::scale_color_discrete()

gg_day <- function(dataset,
                   start.date = NULL,
                   end.date = NULL,
                   x.axis = Time,
                   y.axis,
                   key = NULL,
                   scales = "free_y",
                   y.axis.breaks = 10^(0:5),
                   y.scale.log10 = TRUE,
                   x.axis.label = "Time of Day",
                   y.axis.label = "Lightlevel",
                   format.day = "%d/%m",
                   title = NULL,
                   subtitle = NULL) {
  
  #initial checks
  x <- rlang::enexpr(x.axis)
  y <- rlang::enexpr(y.axis)
  axis_columns <- (purrr::map_chr(c(x,y), rlang::as_string))
  stopifnot(
    is.data.frame(dataset),
    axis_columns %in% names(dataset),
    is.character(scales),
    is.character(format.day),
    is.character(x.axis.label),
    is.character(y.axis.label),
    is.logical(y.scale.log10)
    )
  
  #Data preparation
  if(!is.null(start.date)) {
    dataset <-
      dataset %>% dplyr::filter(!!x >= start.date)
  }
  if(!is.null(end.date)) {
    dataset <-
      dataset %>% dplyr::filter(!!x <= as.Date(end.date) + lubridate::days())
  }
  dataset <-
    dataset %>%
    dplyr::mutate(
      Day.data =
        !!x %>% format(format = format.day),     
      # factoring is necessary for the correct order of days in the plot:
      Day.data = factor(Day.data, levels = unique(Day.data)), 
      Time.data = !!x %>% hms::as_hms()
    )  
  
  #Plot creation
  Plot <- 
    dataset %>% 
    #basic setup
    ggplot2::ggplot(ggplot2::aes(x=Time.data, y = !!y)) +
    ggplot2::geom_point(
      ggplot2::aes(
        col = {{ key }}
      ), na.rm = FALSE) +
    #indication of explicit missing values
    ggplot2::geom_vline(
      data = 
        dataset %>% 
        dplyr::filter(is.na(!!y)), 
      ggplot2::aes(xintercept = Time.data), 
      alpha = 0.2, lwd = 0.05
    )+
    #facetting
    ggplot2::facet_wrap(
      ~Day.data, 
      ncol=1, 
      scales = scales, 
      strip.position = "left") +
    #scales
    ggplot2::scale_color_viridis_d() + 
    ggplot2::scale_x_time(breaks = hms::hms(hours = seq(0, 24, by = 3)), 
                          labels = scales::label_time(format = "%H:%M")) + 
    {if(y.scale.log10){
      ggplot2::scale_y_log10(
        breaks = y.axis.breaks
        ) 
    }
      else ggplot2::scale_y_continuous(breaks = y.axis.breaks)
    }+
    #styling
    ggplot2::labs(
      y= y.axis.label, 
      x= x.axis.label, 
      title = title,
      subtitle = subtitle
      )+
    cowplot::theme_cowplot()+
    ggplot2::theme(
      plot.title.position = "plot",
      panel.grid.major.x = 
        ggplot2::element_line(colour = "grey", linewidth = 0.25),
      strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(face = "bold",),
      strip.placement = "outside"
    )
  
  #return the plot
  Plot
}

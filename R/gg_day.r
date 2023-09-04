
#' Create a simple plot of light logger data, facetted by Day
#' 
#' `gg_day` will create a simple ggplot for every data in a dataset. The result can further be manipulated like any ggplot.
#'
#' @param dataset light logger dataset. Expects a *dataframe*.
#' @param x.axis column name that contains the timedate. Expects a *symbol*.
#' @param y.axis column name that contains the lightlevel. Expects a *symbol*.
#' @param x.axis.label label for the x-axis. Expects a *character*.
#' @param y.axis.label label for the y-axis. Expects a *character*.
#' @param title Plot title. Expects a *character*.
#' @param subtitle Plot subtitle. Expects a *character*.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' filepath <- system.file("extdata/sample_data_LYS.csv", package = "LightLogR")
#' sampledata <- importLYS(filepath, file.id = TRUE)
#' gg_day(
#' sampledata, 
#' x.axis = Datetime, 
#' y.axis.label = "mEDI (lx)",
#' title = "Nonvisual Stimulus for 2 Days"
#' )
gg_day <- function(dataset,
                   x.axis = Time,
                   y.axis = mEDI,
                   x.axis.label = "Time of Day",
                   y.axis.label = "Lightlevel",
                   title = NULL,
                   subtitle = NULL) {
  
  #Data preparation
  dataset <- 
    dataset %>% dplyr::mutate(Day.data = lubridate::day({{ x.axis }}),
                              Time.data = {{ x.axis }} %>% hms::as_hms())
  
  #Plot creation
  Plot <- 
    dataset %>% 
    #basic setup
    ggplot2::ggplot(ggplot2::aes(x=Time.data, y ={{ y.axis }})) +
    ggplot2::geom_point(ggplot2::aes(), na.rm = FALSE) +
    #indication of explicit missing values
    ggplot2::geom_vline(
      data = 
        dataset %>% 
        dplyr::filter(is.na({{ y.axis }})), 
      ggplot2::aes(xintercept = Time.data), 
      alpha = 0.2, lwd = 0.05
    )+
    #facetting
    ggplot2::facet_wrap(
      ~Day.data, ncol=1, scales = "free_y", strip.position = "left") +
    #scales
    ggplot2::scale_color_viridis_b() + 
    ggplot2::scale_x_time(breaks = hms::hms(hours = seq(0, 24, by = 3)), 
                          labels = scales::label_time(format = "%H:%M")) + 
    ggplot2::scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 80000)) +
    # ggplot2::coord_trans(y = "log10", ylim = c(0, 10000))+
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
  
  Plot
}

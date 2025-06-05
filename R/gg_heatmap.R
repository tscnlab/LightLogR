#' Plot a heatmap across days and times of day
#'
#' This function plots a heatmap of binned values across the day over all days
#' in a group. It also allows doubleplot functionality. **[gg_heatmap()] does
#' not work with the additive functions [gg_photoperiod()] and [gg_state()].
#'
#' The function uses [ggplot2::scale_fill_viridis_c()] for the fill scale. The
#' scale can be substituted by any other scale via the standard `+` command of
#' ggplot2. It is recommended to set `fill.remove = TRUE` to reduce warnings.
#'
#' @param dataset A light dataset
#' @param Variable.colname The column name of the variable to display. Defaults
#'   to `MEDI`. Expects a symbol.
#' @param Datetime.colname The column name of the datetime column. Defaults to
#'   `Datetime`. Expects a symbol.
#' @param unit level of aggregation for `Variable.colname`. Defaults to `"1
#'   hour"`. Expects a duration or duration-coercible value
#' @param date.title Title text of the y-axis. Defaults to `Date`
#' @param date.breaks Spacing of date breaks. Defaults to `1` (every day)
#' @param date.labels Formatting code of the date labels
#' @param time.title Title text of the x-axis. Defaults to `Local time (HH:MM)`
#' @param time.breaks Spacing of time breaks. Defauls to every six hours.
#' @param time.labels Formatting code of the time labels
#' @param fill.title Title text of the value (fill) scale.
#' @param fill.scale Scaling of the value (fill) scale. Defaults to `"symlog"`
#'   (see [symlog_trans()])
#' @param fill.labels Formula to format the label values.
#' @param fill.breaks Breaks in the fill scale
#' @param fill.limits Limits of the fill scale. A length-2 numeric with the
#'   lower and upper scale. If one is replaced with `NA`, this limit will be
#'   based on the data.
#' @param ... Other arguments to provide to the underlying
#'   [ggplot2::geom_raster()]
#' @param doubleplot Should the data be plotted as a doubleplot. Default is
#'   "no". "next" will plot the respective next day after the first, "same" will
#'   plot the same day twice.
#' @param fill.remove Logical. Should the fill scale be removed? Handy when the
#'   fill scale is to be replaced by another scale without the console messages
#'   warning about existing scale
#'
#' @returns A ggplot object
#' @export
#'
#' @examples
#' sample.data.environment |> gg_heatmap()
#'
#' #heatmap with doubleplot
#' sample.data.environment |> gg_heatmap(doubleplot = "next")
#'
#' #change the unit of aggregation
#' sample.data.environment |> gg_heatmap(unit = "5 mins")
#'
#' #change the limits of the fill scale
#' sample.data.environment |> gg_heatmap(fill.limits = c(0, 10^4))


gg_heatmap <- function(dataset,
                       Variable.colname = MEDI,
                       Datetime.colname = Datetime,
                       unit = "1 hour",
                       doubleplot = c("no", "same", "next"),
                       date.title = "Date",
                       date.breaks = 1,
                       date.labels = "%d/%m",
                       # date.height = 1.5*3600,
                       time.title = "Local time (HH:MM)",
                       time.breaks = hms::hms(hours = seq(0, 48, by = 6)),
                       time.labels = "%H:%M",
                       fill.title = "Illuminance\n(lx, mel EDI)",
                       fill.scale = "symlog",
                       fill.labels = \(x) format(x, scientific = FALSE, big.mark = " "),
                       fill.breaks = c(-10^(5:0), 0, 10^(0:5)),
                       fill.limits = c(0, 10^5),
                       fill.remove = FALSE,
                       ...
                       ){
  
  doubleplot <- match.arg(doubleplot)
  
  limits <- c(0,24*3600)
  
  stopifnot(
    "The given dataset is not a dataframe" = is.data.frame(dataset),
    "The given column for X is not in the Dataset. If you did not specify X, you are working with data not originating from LightLogR. Please specify an appropriate Datetime column" = 
      colname.defused({{ Datetime.colname }}) %in% names(dataset),
    "The given column for X is not a Datetime" =
      lubridate::is.POSIXct(dataset[[colname.defused({{ Datetime.colname }})]]),
    "The X axis label must be a string" = is.character(time.title),
    "The Y axis label must be a string" = is.character(date.title),
    "The Fill label must be a string" = is.character(fill.title)
    # paste("Unsupported geom:", geom)
  )
  
  dat <- 
    dataset |> 
    dplyr::mutate(max_date = lubridate::date(max({{Datetime.colname}})), .before = 1) |> 
    aggregate_Datetime(unit = unit,
                       max_date = dplyr::first(max_date) + 1) |> 
    dplyr::mutate(date = lubridate::date({{ Datetime.colname }})) |>
    dplyr::filter(date < max_date) |>
    add_Time_col({{ Datetime.colname }})
  
  dat2 <- NULL
  
  if(doubleplot != "no"){
    dat2 <- 
      dat |> dplyr::mutate(
                           Time = Time + 24*3600)
    
    if(doubleplot == "next") {
      dat2 <- 
        dat2 |> dplyr::mutate(
          date = date -1) |> 
        dplyr::filter(date > min(date))
    }
    
    limits <- c(0,48*3600)
  }
  
  dat <- 
    rbind(dat,dat2)
  
  scale_fill <- list(NULL)
  
  if(!fill.remove) {
    scale_fill <- list(
      ggplot2::scale_fill_viridis_c(trans = fill.scale, 
                                    labels = fill.labels, 
                                    breaks = fill.breaks,
                                    limits = fill.limits)
    )
  }
  
  plot <- 
  dat |> 
    ggplot2::ggplot(ggplot2::aes(x = Time, y = (date))) +
    ggplot2::geom_raster(ggplot2::aes(fill = {{ Variable.colname }}),
                         hjust = 1,...) +
    ggplot2::scale_x_time(breaks = time.breaks, 
                          labels = scales::label_time(format = time.labels),
                          expand = c(0,0),
                          limits = limits) +
    ggplot2::facet_wrap(dplyr::group_vars(dat), strip.position = "left", scales = "free_y") + 
    scale_fill +
    ggplot2::scale_y_continuous(
    transform = c("date", "reverse2"),
    breaks = \(x) seq(from = x[1]-0.5, to = x[2]-0.5, by = -date.breaks),
    labels = scales::label_date(format = date.labels)
    ) +
    # ggplot2::scale_y_date(date_breaks = date.breaks,
    #                       labels = scales::label_date(format = date.labels)) +
    ggplot2::coord_cartesian(expand = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_legend(
      title = fill.title,
      label.position = "left",
      label.hjust = 1, 
      )) +
    ggplot2::labs(y = date.title, x = time.title) +
    cowplot::theme_cowplot()+
    ggplot2::theme(
      plot.title.position = "plot",
      panel.grid.major.y = ggplot2::element_line("grey95"),
      panel.grid.major.x = 
        ggplot2::element_line(colour = "grey", linewidth = 0.25),
      strip.text.y = ggplot2::element_text(face = "bold",),
      strip.placement = "outside",
      plot.margin = ggplot2::margin(10, 20, 10, 10, "pt")
    ) + 
    ggplot2::theme(
      # panel.background = ggplot2::element_rect(fill = NA),
      # panel.ontop = TRUE,
      panel.grid.major.y = ggplot2::element_blank(),
      # panel.grid.major.x = ggplot2::element_line(colour = "black", linewidth = 0.5, linetype = "dashed")
    )
  
  plot
}


#' Create a simple plot of light logger data, facetted by Day
#'
#' `gg_day` will create a simple ggplot for every data in a dataset. The result
#' can further be manipulated like any ggplot. This will be sensible to refine
#' styling or guides.
#'
#' Besides plotting, the function creates two new variables from the given
#' `Datetime`:
#' * `Day.data` is a factor that is used for facetting with [ggplot2::facet_wrap()]. Make sure to use this variable, if you change the facetting manually.
#' * `Time.data` is an `hms` created with [hms::as_hms()] that is used for the x.axis
#'
#' @param dataset A light logger dataset. Expects a `dataframe`. If not imported
#'   by [LightLogR], take care to choose a sensible variable for the `x.axis.`.
#' @param x.axis,y.axis column name that contains the datetime (x, defaults to
#'   `"Datetime"` which is automatically correct for data imported with
#'   [LightLogR]) and the dependent variable (y). Expects a `symbol`. Needs to
#'   be part of the `dataset`.
#' @param x.axis.label,y.axis.label labels for the x- and y-axis. Expects a
#'   `character`.
#' @param format.day Label for each day. Default is `%d/%m`, which shows the day
#'   and month. Expects a `character`. For an overview of sensible options look
#'   at [base::strptime()]
#' @param title Plot title. Expects a `character`.
#' @param subtitle Plot subtitle. Expects a `character`.
#' @param start.date,end.date Choose an optional start or end date within your
#'   `dataset`. Expects a `date`, which can also be a `character` that is
#'   interpretable as a date, e.g., `"2023-06-03"`. Can also be a Datetime if
#'   you want to cut off intraday values, e.g., `"2023-06-03 12:00:00"`.
#'   Defaults to `NULL`, which means that the plot starts/ends with the
#'   earliest/latest date within the `dataset`.
#' @param scales For [ggplot2::facet_wrap()], should scales be "fixed", "free"
#'   or free in one dimension ("free_y" is the default). Expects a `character`.
#' @param y.scale.log10 Should `y` be scaled on a log10 basis? Expects a
#'   `logical`.
#' @param col optional column name that defines separate sets and colors them.
#'   Expects anything that works with the layer data [ggplot2::aes()]. By
#'   default, the [ggplot2::scale_color_viridis_d()] is used for coloring. This
#'   can be overwritten outside the function (see examples).
#' @param y.axis.breaks Where should breaks occur on the y.axis? Expects a
#'   `numeric vector` with all the breaks. If you want to activate the default
#'   behaviour of [ggplot2], you need to put in [ggplot2::waiver()].
#' @param y.scale.sc `logical` for whether scientific notation shall be used.
#'   Defaults to `FALSE`.
#' @param geom What geom should be used for visualization? Expects a `character`
#' * `"point"` for [ggplot2::geom_point()] (the default)
#' * `"line"`  for [ggplot2::geom_line()]
#' * as the value is just input into the `geom_` function from [ggplot2], other variants might work as well, but are not tested.
#' @param group Optional column name that defines separate sets. Useful for
#'   certain geoms like `boxplot`.Expects anything that works with the layer
#'   data [ggplot2::aes()]
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' #use `col`for separation of different sets
#' plot <- gg_day(
#' sample.data.environment,
#' scales = "fixed",
#' end.date = "2023-08-16",
#' x.axis = Datetime,
#' y.axis = `MELANOPIC EDI`,
#' y.axis.label = "mEDI (lx)",
#' col = Source)
#' plot
#'
#' #you can easily overwrite the color scale afterwards
#' plot + ggplot2::scale_color_discrete()
#' 
#' #or change the facetting
#' plot + ggplot2::facet_wrap(~Day.data + Source)

gg_day <- function(dataset,
                   start.date = NULL,
                   end.date = NULL,
                   x.axis = Datetime,
                   y.axis,
                   col = NULL,
                   group = NULL,
                   geom = "point",
                   scales = "free_y",
                   y.axis.breaks = 10^(0:5),
                   y.scale.log10 = TRUE,
                   y.scale.sc = FALSE,
                   x.axis.label = "Time of Day",
                   y.axis.label = "Lightlevel",
                   format.day = "%d/%m",
                   title = NULL,
                   subtitle = NULL) {
  
# Initial Checks ----------------------------------------------------------

  x <- rlang::enexpr(x.axis)
  y <- rlang::enexpr(y.axis)
  axis_columns <- (purrr::map_chr(c(x,y), rlang::as_string))
  stopifnot(
    "The given dataset is not a dataframe" = is.data.frame(dataset),
    "The given column for X is not in the Dataset. If you did not specify X, you are working with data not originating from LightLogR. Please specify an appropriate Datetime column" = 
      rlang::as_string(x) %in% names(dataset),
    "The given column for X is not a Datetime" =
      lubridate::is.POSIXct(dataset[[rlang::as_string(x)]]),
    "The given column for Y is not in the Dataset" = 
      rlang::as_string(y) %in% names(dataset),
    "scales must be one of `fixed`, `free_x`, `free_y`, or `free`" = 
      scales %in% c("free_y", "free_x", "fixed", "free"),
    "format.day must be a character. Please make shure it is of type `base::strptime`" = 
      is.character(format.day),
    "The X axis label must be a string" = is.character(x.axis.label),
    "The Y axis label must be a string" = is.character(y.axis.label),
    "y.scale.log10 must be a logical, i.e., TRUE or FALSE" = 
      is.logical(y.scale.log10)
    # paste("Unsupported geom:", geom)
    )

# Data Preparation --------------------------------------------------------

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
  
  #give the user the chance to use whatever geom they want
  geom_function_expr <- rlang::parse_expr(paste0("ggplot2::geom_", geom))

# Plot Creation -----------------------------------------------------------
  
  Plot <- 
    dataset %>% 
    #basic setup
    ggplot2::ggplot(ggplot2::aes(x=Time.data, y = !!y)) +
    eval(geom_function_expr)(
    # ggplot2::geom_line(
      ggplot2::aes(
        group = {{ group }},
        col = {{ col }},
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
        breaks = y.axis.breaks,
        labels = function(x) format(x, scientific = y.scale.sc)
        ) 
    }
      else ggplot2::scale_y_continuous(
        breaks = y.axis.breaks,
        labels = function(x) format(x, scientific = y.scale.sc)
        )
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


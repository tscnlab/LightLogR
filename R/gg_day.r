
#' Create a simple plot of light logger data, facetted by Day
#'
#' `gg_day` will create a simple ggplot for every data in a dataset. The result
#' can further be manipulated like any ggplot. This will be sensible to refine
#' styling or guides.
#'
#' Besides plotting, the function creates two new variables from the given
#' `Datetime`:
#' * `Day.data` is a factor that is used for facetting with [ggplot2::facet_wrap()]. Make sure to use this variable, if you change the faceting manually. Also, the function checks, whether this variable already exists. If it does, it will only convert it to a factor and do the faceting on that variable.
#' * `Time.data` is an `hms` created with [hms::as_hms()] that is used for the x.axis
#'
#' @param dataset A light logger dataset. Expects a `dataframe`. If not imported
#'   by [LightLogR], take care to choose a sensible variable for the `x.axis.`.
#' @param x.axis,y.axis column name that contains the datetime (x, defaults to
#'   `"Datetime"` which is automatically correct for data imported with
#'   [LightLogR]) and the dependent variable (y, defaults to `"MEDI"`, or
#'   melanopic EDI, which is a standard measure of stimulus strength for the
#'   nonvisual effects of light). Expects a `symbol`. Needs to be part of the
#'   `dataset`.
#' @param x.axis.label,y.axis.label labels for the x- and y-axis. Expects a
#'   `character`.
#' @param format.day Label for each day. Default is `%d/%m`, which shows the day
#'   and month. Expects a `character`. For an overview of sensible options look
#'   at [base::strptime()]
#' @param title Plot title. Expects a `character`.
#' @param subtitle Plot subtitle. Expects a `character`.
#' @param start.date,end.date Choose an optional start or end date within your
#'   `dataset`. Expects a `date`, which can also be a `character` that is
#'   interpretable as a date, e.g., `"2023-06-03"`. If you need a Datetime or
#'   want to cut specific times of each day, use the [filter_Datetime] function.
#'   Defaults to `NULL`, which means that the plot starts/ends with the
#'   earliest/latest date within the `dataset`.
#' @param scales For [ggplot2::facet_wrap()], should scales be "fixed", "free"
#'   or free in one dimension ("free_y" is the default). Expects a `character`.
#' @param y.scale.log10 Should `y` be scaled on a log10 basis? Expects a
#'   `logical`.
#' @param col optional column name that defines separate sets and colors them.
#'   Expects anything that works with the layer data [ggplot2::aes()]. The
#'   default color palette can be overwritten outside the function (see
#'   examples).
#' @param x.axis.breaks,y.axis.breaks Where should breaks occur on the x and
#'   y.axis? Expects a `numeric vector` with all the breaks. If you want to
#'   activate the default behaviour of [ggplot2], you need to put in
#'   [ggplot2::waiver()].
#' @param y.scale.sc `logical` for whether scientific notation shall be used.
#'   Defaults to `FALSE`.
#' @param geom What geom should be used for visualization? Expects a `character`
#' * `"point"` for [ggplot2::geom_point()] (the default)
#' * `"line"`  for [ggplot2::geom_line()]
#' * as the value is just input into the `geom_` function from [ggplot2], other variants might work as well, but are not tested.
#' @param group Optional column name that defines separate sets. Useful for
#'   certain geoms like `boxplot`.Expects anything that works with the layer
#'   data [ggplot2::aes()]
#' @param ... Other options that get passed to the main geom function. Can be
#'   used to adjust to adjust size or linetype.
#' @param interactive Should the plot be interactive? Expects a `logical`.
#'   Defaults to `FALSE`.
#' @param facetting Should an automated facet by day be applie? Default is `TRUE` and uses the `Day.data` variable that the function also creates if not present.
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
                   y.axis = MEDI,
                   col = NULL,
                   group = NULL,
                   geom = "point",
                   scales = "fixed",
                   x.axis.breaks = hms::hms(hours = seq(0, 24, by = 3)),
                   y.axis.breaks = 10^(0:5),
                   y.scale.log10 = TRUE,
                   y.scale.sc = FALSE,
                   x.axis.label = "Time of Day",
                   y.axis.label = "Light exposure (lx, MEDI)",
                   format.day = "%d/%m",
                   title = NULL,
                   subtitle = NULL,
                   interactive = FALSE,
                   facetting = TRUE,
                   ...) {
  
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
      is.logical(y.scale.log10),
    "interactive must be a logical" = is.logical(interactive)
    # paste("Unsupported geom:", geom)
    )

# Data Preparation --------------------------------------------------------

  #filter by start and end date
  if(!is.null(start.date)) {
    dataset <-
      dataset %>% dplyr::filter(!!x >= as.Date(start.date))
  }
  if(!is.null(end.date)) {
    dataset <-
      dataset %>% dplyr::filter(!!x <= as.Date(end.date) + lubridate::days())
  }
  if(!is.null(start.date) | !is.null(end.date)) {
    message("Only Dates will be used from start.date and end.date input. If you also want to set Datetimes or Times, consider using the `filter_Datetime()` function instead.")
  }
  
  #create or choose the facet variable
  if(!"Day.data" %in% names(dataset)) {
    dataset <-
      dataset %>%
      dplyr::mutate(
        Day.data =
          !!x %>% format(format = format.day))
  }
  
  dataset <- dataset %>% create_Time.data(Datetime.colname = !!x)
  
  dataset <-
    dataset %>%
    dplyr::mutate(
      # factoring is necessary for the correct order of days in the plot:
      Day.data = factor(Day.data, levels = unique(Day.data))
    )  
  
  #give the user the chance to use whatever geom they want
  geom_function_expr <- rlang::parse_expr(paste0("ggplot2::geom_", geom))

# Plot Creation -----------------------------------------------------------
  
  Plot <- 
    dataset %>% 
    #basic setup
    ggplot2::ggplot(ggplot2::aes(x=Time.data, y = !!y)) +
    eval(geom_function_expr)(
      ggplot2::aes(
        group = {{ group }},
        col = {{ col }},
      ), ...) +
    # Scales --------------------------------------------------------------
    ggsci::scale_color_jco()+
    ggplot2::scale_x_time(breaks = x.axis.breaks, 
                          labels = scales::label_time(format = "%H:%M")) + 
    {if(y.scale.log10){
      ggplot2::scale_y_log10(
        breaks = y.axis.breaks,
        labels = function(x) format(x, scientific = y.scale.sc, big.mark = " ")
        ) 
    }
      else ggplot2::scale_y_continuous(
        breaks = y.axis.breaks,
        labels = function(x) format(x, scientific = y.scale.sc, big.mark = " ")
        )
    }+
    # Styling --------------------------------------------------------------
    ggplot2::labs(
      y= y.axis.label, 
      x= x.axis.label, 
      title = title,
      subtitle = subtitle
      )+
    cowplot::theme_cowplot()+
    ggplot2::theme(
      plot.title.position = "plot",
      panel.grid.major.y = ggplot2::element_line("grey95"),
      panel.grid.major.x = 
        ggplot2::element_line(colour = "grey", linewidth = 0.25),
      # strip.background = ggplot2::element_blank(),
      strip.text.y = ggplot2::element_text(face = "bold",),
      strip.placement = "outside"
    ) +
    # Facetting ------------------------------------------------------------
  if(facetting) {
    ggplot2::facet_wrap(
      ~Day.data, 
      ncol=1, 
      scales = scales, 
      strip.position = "left")
  }
    
  
  # Return --------------------------------------------------------------
  if(interactive) {
    Plot %>% plotly::ggplotly()
  }
  else Plot
}


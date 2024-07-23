#' Create a simple datetime plot of light logger data, faceted by group
#'
#' [gg_days()] will create a simple ggplot along the timeline. The result can
#' further be manipulated like any ggplot. This will be sensible to refine
#' styling or guides. Through the `x.axis.limits` arguments, the plot can be
#' much refined to align several groups of differing datetime ranges. It uses
#' the [Datetime_limits()] function to calculate the limits of the x-axis.
#' Another notable functions that are used are [Datetime_breaks()] to calculate
#' the breaks of the x-axis.
#'
#' The default scaling of the y-axis is a `symlog` scale, which is a logarithmic
#' scale that only starts scaling after a given threshold (default = 0). This
#' enables values of 0 in the plot, which are common in light logger data, and
#' even enables negative values, which might be sensible for non-light data. See
#' [symlog_trans()] for details on tweaking this scale. The scale can also be
#' changed to a normal or logarithmic scale - see the y.scale argument for more.
#' @inheritParams gg_day
#' @param aes_col,aes_fill optional input that defines separate sets and colors
#'   or fills them. Expects anything that works with the layer data
#'   [ggplot2::aes()].
#' @param y.axis.breaks Where should breaks occur on the y.axis? Expects a
#'   `numeric vector` with all the breaks or a function that calculates them
#'   based on the limits. If you want to activate the default behaviour of
#'   \pkg{ggplot2}, you need to put in [ggplot2::waiver()].
#' @param x.axis.breaks The (major) breaks of the x-axis. Defaults to
#'   [Datetime_breaks()]. The function has several options for adjustment. The
#'   default setting place a major break every 12 hours, starting at 12:00 of
#'   the first day.
#' @param x.axis.limits The limits of the x-axis. Defaults to
#'   [Datetime_limits()]. Can and should be adjusted to shift the x-axis to
#'   align different groups of data.
#' @param x.axis.format The format of the x-axis labels. Defaults to `"%a %D"`,
#'   which is the weekday and date. See [base::strptime()] for more options.
#' @param facetting Should an automated facet by grouping be applied? Default is
#'   `TRUE`.
#' @param scales For [ggplot2::facet_wrap()], should scales be `"fixed"`,
#'   `"free"` or `"free"` in one dimension (`"free_x"` is the default). Expects
#'   a `character`.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' dataset <-
#' sample.data.environment %>%
#' aggregate_Datetime(unit = "5 mins")
#'
#' dataset %>% gg_days()
#' #restrict the x-axis to 3 days
#' dataset %>%
#' gg_days(
#' x.axis.limits = \(x) Datetime_limits(x, length = lubridate::ddays(3))
#' )
gg_days <- function(dataset,
                   x.axis = Datetime,
                   y.axis = MEDI,
                   aes_col = NULL,
                   aes_fill = NULL,
                   group = NULL,
                   geom = "line",
                   scales = c("free_x", "free_y", "fixed", "free"),
                   x.axis.breaks = Datetime_breaks,
                   y.axis.breaks = c(-10^(5:0), 0, 10^(0:5)),
                   y.scale = "symlog",
                   y.scale.sc = FALSE,
                   x.axis.label = "Datetime",
                   y.axis.label = "Illuminance (lx, MEDI)",
                   x.axis.limits = Datetime_limits,
                   x.axis.format = "%a %D",
                   title = NULL,
                   subtitle = NULL,
                   interactive = FALSE,
                   facetting = TRUE,
                   jco_color = FALSE,
                   ...) {
  
  # Initial Checks ----------------------------------------------------------
  
  # Match input arguments
  scales <- match.arg(scales)
  
  x <- rlang::enexpr(x.axis) 
  y <- rlang::enexpr(y.axis)
  axis_columns <- (purrr::map_chr(c(x,y), deparse1))
  stopifnot(
    "The given dataset is not a dataframe" = is.data.frame(dataset),
    "The given column for X is not in the Dataset. If you did not specify X, you are working with data not originating from LightLogR. Please specify an appropriate Datetime column" = 
      rlang::as_string(x) %in% names(dataset),
    "The given column for X is not a Datetime" =
      lubridate::is.POSIXct(dataset[[rlang::as_string(x)]]),
    "The X axis label must be a string" = is.character(x.axis.label),
    "The Y axis label must be a string" = is.character(y.axis.label),
    "interactive must be a logical" = is.logical(interactive)
  )
  
  # Data Preparation --------------------------------------------------------
  
  #dots
  dots <- rlang::list2(...)
  
  #special case for geom = "ribbon"
  ribbon <- list()
  if(geom == "ribbon") {
    geom <- "blank"
    ribbon <- 
      rlang::inject(list(
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin = 0, ymax = !!y,
            group = {{ group }},
            fill = {{ aes_fill }},
            col = {{ aes_col }}
            ),
          outline.type = "upper",
          !!!dots
        )
      ))
    
  }
  
  #grouping_vars
  grouping_var <- dplyr::group_vars(dataset)

  #give the user the chance to use whatever geom they want
  geom_function_expr <- rlang::parse_expr(paste0("ggplot2::geom_", geom))
  
  if(geom == "blank") {
    dots <- NULL
  }
  
  #jco color palette
  jco_color_scheme <- list()
  if(jco_color) {
    jco_color_scheme <- 
      list(
        ggsci::scale_color_jco(),
        ggsci::scale_fill_jco()
      )
  }
  
  # Plot Creation -----------------------------------------------------------
  
  Plot <- 
    dataset %>% 
    #basic setup
    ggplot2::ggplot(ggplot2::aes(x=!!x, y = !!y)) +
    rlang::inject(eval(geom_function_expr)(
        ggplot2::aes(
        group = {{ group }},
        fill = {{ aes_fill }},
        col = {{ aes_col }},
      ), !!!dots )) +
    ribbon +
    # Scales --------------------------------------------------------------
    jco_color_scheme +
    ggplot2::scale_y_continuous(
      trans = y.scale,
      breaks = y.axis.breaks,
      labels = function(x) format(x, scientific = y.scale.sc, big.mark = " ")
    )+
    ggplot2::scale_x_datetime(
      breaks = x.axis.breaks,
      date_minor_breaks = "24 hours",
      date_labels = x.axis.format,
      limits = x.axis.limits,
      expand = c(0,0)
    )+
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
      panel.grid.major.y = ggplot2::element_line("grey98"),
      panel.grid.major.x = 
        ggplot2::element_line(color = "grey80", linetype = 2, linewidth = 0.25),
      panel.grid.minor.x = 
        ggplot2::element_line(color = "grey80", linewidth = 0.25),
      strip.text.y = ggplot2::element_text(face = "bold",),
      strip.placement = "outside",
      plot.margin = ggplot2::margin(10, 20, 10, 10, "pt")
    ) +
    # Facetting ------------------------------------------------------------
  if(facetting) {
    ggplot2::facet_wrap(
      grouping_var, 
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

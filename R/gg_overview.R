#' Plot an overview of dataset intervals with implicit missing data
#'
#'
#'
#' @inheritParams gg_day
#' @param Datetime.colname column name that contains the datetime. Defaults to
#'   `"Datetime"` which is automatically correct for data imported with
#'   [LightLogR]. Expects a `symbol`. Needs to be part of the `dataset`.
#' @param Id.colname The column name of the Id column (default is `Id`), needs
#'   to be in the `dataset`. This is also used as the y-axis variable and is the
#'   minimum grouping variable.
#' @param gap.data Optionally provide a `tibble` with `start` and `end`
#'   `Datetimes` of gaps per group. If not provided, the function uses
#'   [gap_finder()] to calculate implicit missing data. This might be
#'   computationally intensive for large datasets and many missing data. In
#'   these cases it can make sense to calculate those gaps beforehand and
#'   provide them to the function. If an empty `tibble` ([tibble::tibble()]) is
#'   provided, the function will just plot the start and end dates of the
#'   dataset, which is computationally very fast at the cost of additional info.
#' @param ... Additional arguments given to the main [ggplot2::aes()] used for
#'   styling depending on data within the `dataset`
#'
#' @return A `ggplot` object
#' @export
#'
#' @examples
#' sample.data.environment %>% gg_overview()
gg_overview <- function(dataset,
                        Datetime.colname = Datetime,
                        Id.colname = Id,
                        gap.data = NULL,
                        ...,
                        interactive = FALSE) {
  
  # Initial Checks ----------------------------------------------------------
  
  #dataset must be a dataset with a posixct datetime column
  
  Datetime.colname.str <- colname.defused({{ Datetime.colname }})
  Id.colname.str <- colname.defused({{ Id.colname }})
  
  stopifnot(
    "The given dataset is not a dataframe" = is.data.frame(dataset),
    "The Datetime.colname is not in the dataset." =
      Datetime.colname.str %in% names(dataset),
    "The Id.colname is not in the dataset." =
      Id.colname.str %in% names(dataset),
    "The Datetime.colname is not a Datetime" =
      lubridate::is.POSIXct(dataset[[Datetime.colname.str]]),
    "interactive must be a logical" = is.logical(interactive)
  )
  
  # Function ----------------------------------------------------------
  
  #make sure the dataset is at least grouped by the Id.colname
  dataset <- dataset %>% dplyr::group_by({{ Id.colname }}, .add = TRUE)
  
  #calculate the ranges of gap data if none is provided
  if(is.null(gap.data)) {
    gap.data <- 
      dataset %>% 
      gap_finder(
        Datetime.colname = {{ Datetime.colname }},
        gap.data = TRUE, silent = TRUE) %>% 
      dplyr::group_by(gap.id, .add = TRUE) %>% 
      dplyr::filter(dplyr::n() > 1)
    
    there_are_gaps <- nrow(gap.data) > 0
    
    if(there_are_gaps) {
      gap.data <- 
        gap.data %>% 
        dplyr::summarize(
          start = min({{ Datetime.colname }}), end = max({{ Datetime.colname }}))
    }
  }
  
  #are there missing data
  there_are_gaps <- nrow(gap.data) > 0
  
  #only add missing data when there is some
  implicit_data <- 
    if(there_are_gaps) {
      list(
        ggplot2::geom_linerange(
          data = gap.data,
          ggplot2::aes(xmin = start, xmax = end, y = {{ Id.colname}}),
          col = "grey", linewidth = 1),
      ggplot2::labs(
        caption = 
          "<b>Dataset intervals</b>, with times of <b style = 'color:grey'>implicit missing data </b>."
      ))
    } else list()
  
  #calculate the ranges within the dataset per grouping
  dataset_range <- 
    dataset %>%
    dplyr::summarize(
      start = min({{ Datetime.colname}}), end = max({{ Datetime.colname}}))
  
  Plot <- 
    #setup the plot
    dataset_range %>% 
    ggplot2::ggplot(
      ggplot2::aes(xmin = start, xmax = end, y = {{ Id.colname }},...)
      ) + 
    #add the lineranges
    ggplot2::geom_linerange(linewidth = 1) +
    implicit_data +
    #add start and stop points
    ggplot2::geom_point(
      ggplot2::aes(x = start, y = {{ Id.colname}}), size = 2) +
    ggplot2::geom_point(
      ggplot2::aes(x = end, y = {{ Id.colname}}), size = 2) +
    #general information
    ggplot2::labs(x = "Datetime")+
    #theming and styling
    ggplot2::scale_x_datetime()+
    cowplot::theme_cowplot()+
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(colour = "grey98"), 
      plot.caption = ggtext::element_markdown(),
      plot.margin = ggplot2::margin(10, 20, 10, 10, "pt")
      )
  
  # Return ----------------------------------------------------------
  if(interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop("Package 'plotly' is required for interactive use. Please install it.", call. = FALSE)
    }
    Plot %>% plotly::ggplotly()
  } else Plot
  
}

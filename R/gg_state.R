#' Add states to gg_day() or gg_days() plots
#'
#' [gg_state()] is a helper function to add state information to plots generated
#' with [gg_day()], [gg_days()], or [gg_doubleplot()]. The function can draw on
#' any column in the dataset, but factor-like or logical columns make the most
#' sense. The time series must be based on a column called `Datetime`.
#'
#' @param ggplot_obj A `ggplot` object generated with [gg_day()] or [gg_days()]
#'   (or [gg_doubleplot()]. The dataset used to create these **must** have a
#'   `Datetime` column.
#' @param alpha A numerical value between 0 and 1 representing the transparency
#'   of the states. Default is 0.2.
#' @param ... Additional arguments given to the [ggplot2::geom_rect()] used to
#'   construct the state shading. Can be used to change the fill color or other
#'   aesthetic properties.
#' @param on.top Logical scalar. If `TRUE`, the states will be plotted on top of
#'   the existing plot. If `FALSE`, the states will be plotted underneath the
#'   existing plot. Default is `FALSE`.
#' @param State.colname The colname of the state to add to the plot. Must be
#'   part of the dataset. Expects a `symbol`.
#' @param aes_fill,aes_col conditional aesthetics for [ggplot2::geom_rect()].
#'   The default (`NULL`) will be ignored, so that `col` and `fill` arguments
#'   can be set through the `...` arguments. As the states work from a
#'   summarized dataset, only a few columns are available for filling/coloring:
#'   The `State.colname`, Grouping variables, and variables created by using
#'   [extract_states()].
#' @param ignore.FALSE Logical that drops `FALSE` values of a logical state
#'   column, so that only `TRUE` values are recognized as a state. Is only
#'   relevant for logical state columns and will be ignored otherwise. Default
#'   is `TRUE`.
#'
#' @returns a modified `ggplot` object with the states added.
#' @export
#'
#' @examples
#' #creating a simple TRUE/FALSE state in the sample data: Light above 250 lx mel EDI
#' #and a second state that cuts data into chunks relating to the Brown et al. 2022 thresholds
#' #(+aggregating Data to 5 minute intervals & reducing it to three days)
#' state_data <-
#'   sample.data.environment |>
#'    dplyr::mutate(state = MEDI > 250) |>
#'    Brown_cut(MEDI, state2) |> 
#'    aggregate_Datetime(unit = "5 mins") |>
#'    filter_Datetime(length = "3 days")
#'
#' state_data |>
#'  gg_days() |>
#'  gg_state(state)
#'
#' #state 2 has more than one valid state, thus we need to assign a fill aesthetic
#' state_data |>
#'  gg_days() |>
#'  gg_state(state2, aes_fill = state2) +
#'  ggplot2::scale_fill_manual(values=c("#868686FF", "#EFC000FF", "#0073C2FF"))
#'  #this line is simply for sensible colors
#'
#' #same, but with gg_day()
#' state_data |>
#'  dplyr::filter(Id == "Participant") |>
#'  gg_day(geom = "line") |>
#'  gg_state(state, fill = "red")
#'
#'  #more complex state
#'  state_data |>
#'  dplyr::filter(Id == "Participant") |>
#'  gg_day(geom = "line") |>
#'  gg_state(state2, aes_fill = state2)
#'
#'  #with gg_doubleplot
#'  state_data |>
#'  dplyr::filter(Id == "Participant") |>
#'  gg_doubleplot() |>
#'  gg_state(state2, aes_fill = state2)
#' 
gg_state <- function(ggplot_obj, 
                      State.colname,
                      aes_fill = NULL,
                      aes_col = NULL,
                      alpha = 0.2,
                      on.top = FALSE,
                      ignore.FALSE = TRUE,
                      ...) {
  
  # Initial Checks ----------------------------------------------------------
  
  #ggplot must be a ggplot object
  stopifnot(
    "ggplot_obj must be a ggplot object" = inherits(ggplot_obj, "gg")
  )
  
  #determine whether the ggplot has a hms or a POSIXct x-axis
  x_axis_type <- 
    get_ggplot_axis_type(ggplot_obj, "x")
  
  # Function ----------------------------------------------------------
  
  #remove FALSE states, if argument is set
  
  state_logical <- 
    ggplot_obj$data |> dplyr::pull({{ State.colname }}) |> is.logical()
  
  if(ignore.FALSE & state_logical) {
    ggplot_obj$data <- 
      ggplot_obj$data |> 
      dplyr::mutate({{ State.colname }} := dplyr::na_if({{ State.colname }},0))
  }
  
  #if the y_axis_type is time
  if(x_axis_type == "time") {
    #create a table of states, by date
    state_data <-
      ggplot_obj$data |>
      # dplyr::group_by(date.grouper = lubridate::date(Datetime), .add = TRUE) |>
      extract_states({{ State.colname }}) |> 
      extract_metric(ggplot_obj$data,
                     midnight.before =
                       min(Datetime) |> lubridate::floor_date("day"),
                     midnight.after =
                       max(Datetime) |> lubridate::ceiling_date("day")
      ) |> 
      tidyr::drop_na({{ State.colname }})
    
    state_data <-
      state_data |>
      dplyr::mutate(
        start = dplyr::case_when(midnight.before > start ~ midnight.before,
                                .default = start),
        end = dplyr::case_when(midnight.after < (end+epoch/2) ~ midnight.after,
                                .default = end)
      )
  }
  
  #if the x_axis_type is hms
  if(x_axis_type == "hms") {
    
    state_data <-
      ggplot_obj$data |>
      dplyr::group_by(Day.data, .add = TRUE) |>
      extract_states({{ State.colname }}) |> 
      tidyr::drop_na({{ State.colname }})
    
    state_data <-
      state_data |>
      dplyr::mutate(
        dplyr::across(c(start, end), hms::as_hms),
        start = ifelse(end < start, 0, start) |> hms::as_hms(),
        end = ifelse(end < start, 24*3600, end) |> hms::as_hms()
      )
  }

  #create the geoms for the states
  state_geoms <- 
    list(
      ggplot2::geom_rect(
        inherit.aes = FALSE,
        data = state_data,
        ggplot2::aes(
          xmin = start,
          xmax = end,
          ymin = -Inf,
          ymax = Inf, 
          fill = {{ aes_fill }},
          col = {{ aes_col }}
        ),
        alpha = alpha,
        ...
      )
    )
  
  #add the geoms to the ggplot
  new_plot_obj <- ggplot_obj + state_geoms
  
  if(on.top) {
    return(new_plot_obj)
  }
  
  #reorder the layers so that the new geoms are at the very bottom
  new_plot_obj$layers <-
    c(new_plot_obj$layers |> utils::tail(1), 
      new_plot_obj$layers |> utils::head(-1))
  
  #return
  new_plot_obj
  
}

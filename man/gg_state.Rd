% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_state.R
\name{gg_state}
\alias{gg_state}
\title{Add states to gg_day() or gg_days() plots}
\usage{
gg_state(
  ggplot_obj,
  State.colname,
  aes_fill = NULL,
  aes_col = NULL,
  alpha = 0.2,
  on.top = FALSE,
  ignore.FALSE = TRUE,
  ...
)
}
\arguments{
\item{ggplot_obj}{A \code{ggplot} object generated with \code{\link[=gg_day]{gg_day()}} or \code{\link[=gg_days]{gg_days()}}
(or \code{\link[=gg_doubleplot]{gg_doubleplot()}}. The dataset used to create these \strong{must} have a
\code{Datetime} column.}

\item{State.colname}{The colname of the state to add to the plot. Must be
part of the dataset. Expects a \code{symbol}.}

\item{aes_fill, aes_col}{conditional aesthetics for \code{\link[ggplot2:geom_tile]{ggplot2::geom_rect()}}.
The default (\code{NULL}) will be ignored, so that \code{col} and \code{fill} arguments
can be set through the \code{...} arguments. As the states work from a
summarized dataset, only a few columns are available for filling/coloring:
The \code{State.colname}, Grouping variables, and variables created by using
\code{\link[=extract_states]{extract_states()}}.}

\item{alpha}{A numerical value between 0 and 1 representing the transparency
of the states. Default is 0.2.}

\item{on.top}{Logical scalar. If \code{TRUE}, the states will be plotted on top of
the existing plot. If \code{FALSE}, the states will be plotted underneath the
existing plot. Default is \code{FALSE}.}

\item{ignore.FALSE}{Logical that drops \code{FALSE} values of a logical state
column, so that only \code{TRUE} values are recognized as a state. Is only
relevant for logical state columns and will be ignored otherwise. Default
is \code{TRUE}.}

\item{...}{Additional arguments given to the \code{\link[ggplot2:geom_tile]{ggplot2::geom_rect()}} used to
construct the state shading. Can be used to change the fill color or other
aesthetic properties.}
}
\value{
a modified \code{ggplot} object with the states added.
}
\description{
\code{\link[=gg_state]{gg_state()}} is a helper function to add state information to plots generated
with \code{\link[=gg_day]{gg_day()}}, \code{\link[=gg_days]{gg_days()}}, or \code{\link[=gg_doubleplot]{gg_doubleplot()}}. The function can draw on
any column in the dataset, but factor-like or logical columns make the most
sense. The time series must be based on a column called \code{Datetime}.
}
\examples{
#creating a simple TRUE/FALSE state in the sample data: Light above 250 lx mel EDI
#and a second state that cuts data into chunks relating to the Brown et al. 2022 thresholds
#(+aggregating Data to 5 minute intervals & reducing it to three days)
state_data <-
  sample.data.environment |>
   dplyr::mutate(state = MEDI > 250) |>
   Brown_cut(MEDI, state2) |> 
   aggregate_Datetime(unit = "5 mins") |>
   filter_Datetime(length = "3 days")

state_data |>
 gg_days() |>
 gg_state(state)

#state 2 has more than one valid state, thus we need to assign a fill aesthetic
state_data |>
 gg_days() |>
 gg_state(state2, aes_fill = state2) +
 ggplot2::scale_fill_manual(values=c("#868686FF", "#EFC000FF", "#0073C2FF"))
 #this line is simply for sensible colors

#same, but with gg_day()
state_data |>
 dplyr::filter(Id == "Participant") |>
 gg_day(geom = "line") |>
 gg_state(state, fill = "red")

 #more complex state
 state_data |>
 dplyr::filter(Id == "Participant") |>
 gg_day(geom = "line") |>
 gg_state(state2, aes_fill = state2)

 #with gg_doubleplot
 state_data |>
 dplyr::filter(Id == "Participant") |>
 gg_doubleplot() |>
 gg_state(state2, aes_fill = state2)

}

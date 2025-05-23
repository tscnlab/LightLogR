% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clusters.R
\name{extract_clusters}
\alias{extract_clusters}
\alias{add_clusters}
\title{Find and extract clusters from a dataset}
\usage{
extract_clusters(
  data,
  Variable,
  Datetime.colname = Datetime,
  cluster.duration = "30 mins",
  duration.type = c("min", "max"),
  interruption.duration = 0,
  interruption.type = c("max", "min"),
  cluster.colname = state.count,
  return.only.clusters = TRUE,
  handle.gaps = FALSE
)

add_clusters(
  data,
  Variable,
  Datetime.colname = Datetime,
  cluster.duration = "30 mins",
  duration.type = c("min", "max"),
  interruption.duration = 0,
  interruption.type = c("max", "min"),
  cluster.colname = state,
  handle.gaps = FALSE
)
}
\arguments{
\item{data}{A light logger dataset. Expects a dataframe.}

\item{Variable}{The variable or condition to be evaluated for clustering. Can
be a column name or an expression.}

\item{Datetime.colname}{Column name that contains the datetime. Defaults to
"Datetime" which is automatically correct for data imported with LightLogR.
Expects a symbol.}

\item{cluster.duration}{The minimum or maximum duration of a cluster.
Defaults to 30 minutes. Expects a lubridate duration object (or a numeric
in seconds).}

\item{duration.type}{Type of the duration requirement for clusters. Either
"min" (minimum duration) or "max" (maximum duration). Defaults to "min".}

\item{interruption.duration}{The duration of allowed interruptions within a
cluster. Defaults to 0 (no interruptions allowed).}

\item{interruption.type}{Type of the interruption duration. Either "max"
(maximum interruption) or "min" (minimum interruption). Defaults to "max".}

\item{cluster.colname}{Name of the column to use for the cluster
identification. Defaults to "state.count". Expects a symbol.}

\item{return.only.clusters}{Whether to return only the identified clusters
(TRUE) or also include non-clusters (FALSE). Defaults to TRUE.}

\item{handle.gaps}{Logical whether the data shall be treated with
\code{\link[=gap_handler]{gap_handler()}}. Is set to \code{FALSE} by default, due to computational costs.}
}
\value{
For \code{extract_clusters()} a dataframe containing the identified
clusters or all time periods, depending on \code{return.only.clusters}.

For \code{add_clusters()} a dataframe containing the original data with an additional column
for cluster identification.
}
\description{
\code{extract_clusters()} searches for and summarizes clusters where
data meets a certain condition. Clusters have a specified duration and can
be interrupted while still counting as one cluster. The variable can either
be a column in the dataset or an expression that gets evaluated in a
\code{\link[dplyr:mutate]{dplyr::mutate()}} call.

Cluster start and end times are shifted by half of the epoch each. E.g., a
state lasting for 4 measurement points will have a duration of 4
measurement intervals, and a state only occuring once, of one interval.
This deviates from simply using the time difference between the first and
last occurance, which would be one epoch shorter (e.g., the start and end
points for a state lasting a single point is identical, i.e., zero
duration)

\strong{For correct cluster identification, there can be no gaps in the data!}
Gaps can inadvertently be introduced to a gapless dataset through grouping.
E.g., when grouping by photoperiod (day/night) within a participant, this
introduces gaps between the individual days and nights that together form
the group. To avoid this, either group by individual days and nights (e.g.,
by using \code{\link[=number_states]{number_states()}} before grouping), which will make sure a cluster
cannot extend beyond any grouping. Alternatively, you can set \code{handle.gaps = TRUE} (at computational cost).

\code{add_clusters()} identifies clusters and adds them back into the
dataset through a rolling join. This is a convenience function built on \code{\link[=extract_clusters]{extract_clusters()}}.
}
\examples{

dataset <-
sample.data.environment |>
dplyr::filter(Id == "Participant")

# Extract clusters with minimum duration of 1 hour and interruptions of up to 5 minutes
dataset |>
 extract_clusters(
  MEDI > 1000,
  cluster.duration = "1 hour",
  interruption.duration = "5 mins"
)

# Add clusters to a dataset where lux values are above 1000 for at least 30 minutes
dataset_with_clusters <- 
sample.data.environment \%>\% add_clusters(MEDI > 1000)

dataset_with_clusters |> dplyr::count(state)

}
\concept{Clusters}

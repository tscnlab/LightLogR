% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data2reference.R
\name{data2reference}
\alias{data2reference}
\title{Create reference data from other data}
\usage{
data2reference(
  dataset,
  Reference.data = dataset,
  Datetime.column = Datetime,
  Data.column = MEDI,
  Id.column = Id,
  Reference.column = Reference,
  overwrite = FALSE,
  filter.expression.reference = NULL,
  across.id = FALSE,
  shift.start = FALSE,
  length.restriction.seconds = 60,
  shift.intervals = "auto",
  Reference.label = NULL
)
}
\arguments{
\item{dataset}{A light logger dataset}

\item{Reference.data}{The data that should be used as reference. By default
the \code{dataset} will be used as reference.}

\item{Datetime.column}{Datetime column of the \code{dataset} and \code{Reference.data}.
Need to be the same in both sets. Default is \code{Datetime}.}

\item{Data.column}{Data column in the \code{Reference.data} that is then converted
to a reference. Default is \code{MEDI}.}

\item{Id.column}{Name of the \code{Id.column} in both the \code{dataset} and the
\code{Reference.data}.}

\item{Reference.column}{Name of the reference column that will be added to
the \code{dataset}. Default is \code{Reference}. Cannot be the same as any other
column in the \code{dataset} and will throw an error if it is.}

\item{overwrite}{If \code{TRUE} (defaults to \code{FALSE}), the function will
overwrite the \code{Reference.colname} column if it already exists.}

\item{filter.expression.reference}{Expression that is used to filter the
\code{Reference.data} before it is used as reference. Default is \code{NULL}. See}

\item{across.id}{Grouping variables that should be ignored when creating the
reference data. Default is \code{FALSE}. If \code{TRUE}, all grouping variables are
ignored. If \code{FALSE}, no grouping variables are ignored. If a vector of
grouping variables is given, these are ignored.}

\item{shift.start}{If \code{TRUE}, the reference data is shifted to the start of
the respective group. Default is \code{FALSE}. The shift ignores the groups
specified in \code{across.id}.}

\item{length.restriction.seconds}{Restricts the application of reference data
to a maximum length in seconds. Default is \code{60} seconds. This is useful to
avoid reference data being applied to long periods of time, e.g., when
there are gaps in the reference data}

\item{shift.intervals}{Time shift in seconds, that is applied to every data
point in the reference data. Default is \code{"auto"}. If \code{"auto"}, the shift is
calculated by halving the most frequent time difference between two data
points in the reference data. If a number is given, this number in seconds
is used as the shift. Can also use \code{\link[lubridate:duration]{lubridate::duration()}} to specify the
shift.}

\item{Reference.label}{Label that is added to the reference data. If \code{NULL},
no label is added.}
}
\value{
A \code{dataset} with a new column \code{Reference} that contains the reference
data.
}
\description{
Create reference data from almost any other data that has a datetime column
and a data column. The reference data can even be created from subsets of the
same data. Examples are that one participant can be used as a reference for
all other participants, or that the first (second,...) day of every
participant data is the reference for any other day. \strong{This function needs to
be carefully handled, when the reference data time intervals are shorter than
the data time intervals. In that case, use \code{aggregate_Datetime()} on the
reference data beforehand to lengthen the interval.}
}
\details{
To use subsets of data, use the \code{filter.expression.reference} argument to
specify the subsets of data. The \code{across.id} argument specifies whether the
reference data should be used across all or some grouping variables (e.g.,
across participants). The \code{shift.start} argument enables a shift of the
reference data start time to the start of the respective group.

and @examples for more information. The expression is evaluated
within \code{\link[dplyr:filter]{dplyr::filter()}}.
}
\examples{
library(dplyr)
library(lubridate)
library(ggplot2)

gg_reference <- function(dataset) {
dataset \%>\%
ggplot(aes(x = Datetime, y = MEDI, color = Id)) +
geom_line(linewidth = 1) +
geom_line(aes(y = Reference), color = "black", size = 0.25, linetype = "dashed") +
theme_minimal() + facet_wrap(~ Id, scales = "free_y")
}

#in this example, each data point is its own reference
sample.data.environment \%>\%
  data2reference() \%>\%
  gg_reference()

#in this example, the first day of each ID is the reference for the other days
#this requires grouping of the Data by Day, which is then specified in across.id
#also, shift.start needs to be set to TRUE, to shift the reference data to the
#start of the groupings
sample.data.environment \%>\% group_by(Id, Day = as_date(Datetime)) \%>\%
data2reference(
  filter.expression.reference =  as_date(Datetime) == min(as_date(Datetime)),
  shift.start = TRUE,
  across.id = "Day") \%>\%
  gg_reference()

#in this example, the Environment Data will be used as a reference
sample.data.environment \%>\%
data2reference(
  filter.expression.reference =  Id == "Environment",
  across.id = TRUE) \%>\%
  gg_reference()
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cut_Datetime.R
\name{cut_Datetime}
\alias{cut_Datetime}
\title{Create Datetime bins for visualization and calculation}
\usage{
cut_Datetime(
  dataset,
  unit = "3 hours",
  type = c("round", "floor", "ceiling"),
  Datetime.colname = Datetime,
  New.colname = Datetime.rounded,
  group_by = FALSE,
  ...
)
}
\arguments{
\item{dataset}{A light logger dataset. Expects a \code{dataframe}. If not imported
by \link{LightLogR}, take care to choose a sensible variable for the
\code{Datetime.colname}.}

\item{unit}{Unit of binning. See \code{\link[lubridate:round_date]{lubridate::round_date()}} for examples. The default is \code{"3 hours"}.}

\item{type}{One of \code{"round"}(the default), \code{"ceiling"} or \code{"floor"}. Setting
chooses the relevant function from \pkg{lubridate}.}

\item{Datetime.colname}{column name that contains the datetime. Defaults to
\code{"Datetime"} which is automatically correct for data imported with
\link{LightLogR}. Expects a \code{symbol}. Needs to be part of the \code{dataset}. Must
be of type \code{POSIXct}.}

\item{New.colname}{Column name for the added column in the \code{dataset}.}

\item{group_by}{Should the data be grouped by the new column? Defaults to \code{FALSE}}

\item{...}{Parameter handed over to \code{\link[lubridate:round_date]{lubridate::round_date()}} and siblings}
}
\value{
a \code{data.frame} object identical to \code{dataset} but with the added
column of binned datetimes.
}
\description{
\code{cut_Datetime} is a wrapper around \code{\link[lubridate:round_date]{lubridate::round_date()}} (and friends)
combined with \code{\link[dplyr:mutate]{dplyr::mutate()}}, to create a new column in a light logger
dataset with a specified binsize. This can be \code{"3 hours"}, \code{"15 secs"}, or
\code{"0.5 days"}. It is a useful step between a dataset and a visualization or
summary step.
}
\examples{
#compare Datetime and Datetime.rounded
sample.data.environment \%>\%
  cut_Datetime() \%>\%
  dplyr::slice_sample(n = 5)
}

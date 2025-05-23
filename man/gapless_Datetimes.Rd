% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gaps.R
\name{gapless_Datetimes}
\alias{gapless_Datetimes}
\title{Create a gapless sequence of Datetimes}
\usage{
gapless_Datetimes(
  dataset,
  Datetime.colname = Datetime,
  epoch = "dominant.epoch",
  full.days = FALSE
)
}
\arguments{
\item{dataset}{A light logger dataset. Needs to be a dataframe.}

\item{Datetime.colname}{The column that contains the datetime. Needs to be a
\code{POSIXct} and part of the dataset.}

\item{epoch}{The epoch to use for the gapless sequence. Can be either a
\code{lubridate::duration()} or a string. If it is a string, it needs to be
either '"dominant.epoch"' (the default) for a guess based on the data or a
valid \code{lubridate::duration()} string, e.g., \code{"1 day"} or \code{"10 sec"}.}

\item{full.days}{If \code{TRUE}, the gapless sequence will include the whole first
and last day where there is data.}
}
\value{
A \code{tibble} with a gapless sequence of \code{Datetime} as specified by
\code{epoch}.
}
\description{
Create a gapless sequence of Datetimes. The Datetimes are determined by the
minimum and maximum Datetime in the dataset and an epoch. The epoch can
either be guessed from the dataset or specified by the user.
}
\examples{
  dataset <-
  tibble::tibble(Id = c("A", "A", "A", "B", "B", "B"),
                 Datetime = lubridate::as_datetime(1) +
                 lubridate::days(c(0:2, 4, 6, 8))) \%>\%
                 dplyr::group_by(Id)

  dataset \%>\% gapless_Datetimes()
  dataset \%>\% dplyr::ungroup() \%>\%  gapless_Datetimes()
  dataset \%>\% gapless_Datetimes(epoch = "1 day")
}
\seealso{
Other regularize: 
\code{\link{dominant_epoch}()},
\code{\link{extract_gaps}()},
\code{\link{gap_finder}()},
\code{\link{gap_handler}()},
\code{\link{has_gaps}()},
\code{\link{has_irregulars}()}
}
\concept{regularize}

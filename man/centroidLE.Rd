% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_centroidLE.R
\name{centroidLE}
\alias{centroidLE}
\title{Centroid of light exposure}
\usage{
centroidLE(
  Light.vector,
  Time.vector,
  bin.size = NULL,
  na.rm = FALSE,
  as.df = FALSE
)
}
\arguments{
\item{Light.vector}{Numeric vector containing the light data.}

\item{Time.vector}{Vector containing the time data. Can be numeric, HMS or POSIXct.}

\item{bin.size}{Value specifying size of bins to average the light data over.
If \code{Time.vector} is of type POSIXct or HMS, \code{bin.size} must be either a
\code{lubridate::duration()} or a \code{lubridate::duration()} string, e.g.,
\code{"1 day"} or \code{"10 sec"}. Otherwise, if \code{Time.vector} is numeric, \code{bin.size}
must be also numeric. If nothing is provided, no binning will be performed.}

\item{na.rm}{Logical. Should missing values be removed for the calculation?
Defaults to \code{FALSE}.}

\item{as.df}{Logical. Should the output be returned as a data frame? Defaults
to \code{FALSE}.}
}
\value{
Single column data frame or vector.
}
\description{
This function calculates the centroid of light exposure as the mean of the
time vector weighted in proportion to the corresponding binned light intensity.
}
\examples{
# Dataset with POSIXct time vector
dataset1 <-
  tibble::tibble(
    Id = rep("A", 24),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )
dataset1 \%>\%
  dplyr::reframe(
    "Centroid of light exposure" = centroidLE(MEDI, Datetime, "2 hours")
  )

# Dataset with HMS time vector
dataset2 <-
  tibble::tibble(
    Id = rep("A", 24),
    Time = hms::as_hms(lubridate::as_datetime(0) + lubridate::hours(0:23)),
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )
dataset2 \%>\%
  dplyr::reframe(
    "Centroid of light exposure" = centroidLE(MEDI, Time, "2 hours")
  )

# Dataset with numeric time vector
dataset3 <-
  tibble::tibble(
    Id = rep("A", 24),
    Hour = 0:23,
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )
dataset3 \%>\%
  dplyr::reframe(
    "Centroid of light exposure" = centroidLE(MEDI, Hour, 2)
  )

}
\references{
Phillips, A. J. K., Clerx, W. M., O’Brien, C. S., Sano, A., Barger,
L. K., Picard, R. W., Lockley, S. W., Klerman, E. B., & Czeisler, C. A. (2017).
Irregular sleep/wake patterns are associated with poorer academic performance
and delayed circadian and sleep/wake timing. \emph{Scientific Reports},
7(1), 3216. \url{https://doi.org/10.1038/s41598-017-03171-4}

Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
Quantification metrics. \emph{Lighting Research & Technology}.
\url{https://doi.org/10.1177/14771535231170500}
}
\seealso{
Other metrics: 
\code{\link{bright_dark_period}()},
\code{\link{disparity_index}()},
\code{\link{duration_above_threshold}()},
\code{\link{interdaily_stability}()},
\code{\link{intradaily_variability}()},
\code{\link{midpointCE}()},
\code{\link{nvRC}()},
\code{\link{nvRD}()},
\code{\link{nvRD_cumulative_response}()},
\code{\link{timing_above_threshold}()}
}
\concept{metrics}

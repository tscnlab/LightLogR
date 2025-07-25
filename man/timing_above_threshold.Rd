% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_timing_above_threshold.R
\name{timing_above_threshold}
\alias{timing_above_threshold}
\title{Mean/first/last timing above/below threshold.}
\usage{
timing_above_threshold(
  Light.vector,
  Time.vector,
  comparison = c("above", "below"),
  threshold,
  na.rm = FALSE,
  as.df = FALSE
)
}
\arguments{
\item{Light.vector}{Numeric vector containing the light data.}

\item{Time.vector}{Vector containing the time data. Can be \link[base]{POSIXct},
\link[hms]{hms}, \link[lubridate]{duration}, or \link[base]{difftime}.}

\item{comparison}{String specifying whether the time above or below threshold
should be calculated. Can be either \code{"above"} (the default) or \code{"below"}. If
two values are provided for \code{threshold}, this argument will be ignored.}

\item{threshold}{Single numeric value or two numeric values specifying the
threshold light level(s) to compare with. If a vector with two values is provided,
the timing corresponding to light levels between the two thresholds will be
calculated.}

\item{na.rm}{Logical. Should missing values be removed for the calculation?
Defaults to \code{FALSE}.}

\item{as.df}{Logical. Should a data frame be returned? If \code{TRUE}, a data
frame with three columns (MLiT, FLiT, LLiT) and the threshold (e.g., \verb{MLiT_\{threshold\}})
will be returned. Defaults to \code{FALSE}.}
}
\value{
List or dataframe with the three values: \code{mean}, \code{first}, and \code{last} timing
above threshold. The output type corresponds to the type of \code{Time.vector},
e.g., if \code{Time.vector} is HMS, the timing metrics will be also
HMS, and vice versa for POSIXct and numeric.
}
\description{
This function calculates the mean, first, and last timepoint (MLiT, FLiT, LLiT)
where light levels are above or below a given threshold intensity within the given
time interval.
}
\examples{
# Dataset with light > 250lx between 06:00 and 18:00
dataset1 <-
  tibble::tibble(
    Id = rep("A", 24),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
    MEDI = c(rep(1, 6), rep(250, 13), rep(1, 5))
  )

# Above threshold
dataset1 \%>\%
  dplyr::reframe(timing_above_threshold(MEDI, Datetime, "above", 250, as.df = TRUE))

# Below threshold
dataset1 \%>\%
  dplyr::reframe(timing_above_threshold(MEDI, Datetime, "below", 10, as.df = TRUE))

# Input = HMS -> Output = HMS
dataset1 \%>\%
  dplyr::reframe(timing_above_threshold(MEDI, hms::as_hms(Datetime), "above", 250, as.df = TRUE))

}
\references{
Reid, K. J., Santostasi, G., Baron, K. G., Wilson, J., Kang, J.,
& Zee, P. C. (2014). Timing and Intensity of Light Correlate with Body Weight
in Adults. \emph{PLOS ONE}, 9(4), e92251.
\doi{10.1371/journal.pone.0092251}

Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
Quantification metrics. \emph{Lighting Research & Technology}.
\doi{10.1177/14771535231170500}
}
\seealso{
Other metrics: 
\code{\link{bright_dark_period}()},
\code{\link{centroidLE}()},
\code{\link{disparity_index}()},
\code{\link{dose}()},
\code{\link{duration_above_threshold}()},
\code{\link{exponential_moving_average}()},
\code{\link{frequency_crossing_threshold}()},
\code{\link{interdaily_stability}()},
\code{\link{intradaily_variability}()},
\code{\link{midpointCE}()},
\code{\link{nvRC}()},
\code{\link{nvRD}()},
\code{\link{nvRD_cumulative_response}()},
\code{\link{period_above_threshold}()},
\code{\link{pulses_above_threshold}()},
\code{\link{threshold_for_duration}()}
}
\concept{metrics}

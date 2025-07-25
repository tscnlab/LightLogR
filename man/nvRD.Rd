% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_nvR.R
\name{nvRD}
\alias{nvRD}
\title{Non-visual direct response}
\usage{
nvRD(MEDI.vector, Illuminance.vector, Time.vector, epoch = "dominant.epoch")
}
\arguments{
\item{MEDI.vector}{Numeric vector containing the melanopic EDI data.}

\item{Illuminance.vector}{Numeric vector containing the Illuminance data.}

\item{Time.vector}{Vector containing the time data. Can be \code{\link[=POSIXct]{POSIXct()}},\code{\link[hms:hms]{hms::hms()}},
\code{\link[lubridate:duration]{lubridate::duration()}}, \code{\link[=difftime]{difftime()}}.}

\item{epoch}{The epoch at which the data was sampled. Can be either a
\code{\link[lubridate:duration]{lubridate::duration()}} or a string. If it is a string, it needs to be
either \code{"dominant.epoch"} (the default) for a guess based on the data, or a valid
\code{\link[lubridate:duration]{lubridate::duration()}} string, e.g., \code{"1 day"} or \code{"10 sec"}.}
}
\value{
A numeric vector containing the nvRD data. The output has the same
length as \code{Time.vector}.
}
\description{
This function calculates the non-visual direct response (nvRD). It takes into account
the assumed response dynamics of the non-visual system and processes the light
exposure signal to quantify the effective direct input to the non-visual system
(see Details).
}
\details{
The timeseries is assumed to be regular. Missing values in the
light data will be replaced by 0.
}
\examples{

# Dataset 1 with 24h measurement
dataset1 <-
  tibble::tibble(
    Id = rep("A", 60 * 24),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*24-1)),
    Illuminance = c(rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60)),
    MEDI = Illuminance * rep(sample(0.5:1.5, 24, replace = TRUE), each = 60)
  ) 
# Dataset 2 with 48h measurement
dataset2 <-
  tibble::tibble(
    Id = rep("B", 60 * 48),
    Datetime = lubridate::as_datetime(0) + lubridate::minutes(0:(60*48-1)),
    Illuminance = c(rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60), 
                    rep(0, 60*8), rep(sample(1:1000, 16, replace = TRUE), each = 60)),
    MEDI = Illuminance * rep(sample(0.5:1.5, 48, replace = TRUE), each = 60)
  )
# Combined datasets
dataset.combined <- rbind(dataset1, dataset2)

# Calculate nvRD per ID
dataset.combined.nvRD <- dataset.combined \%>\% 
  dplyr::group_by(Id) \%>\% 
  dplyr::mutate(
    nvRD = nvRD(MEDI, Illuminance, Datetime)
  )

}
\references{
Amundadottir, M.L. (2016). Light-driven model for identifying
indicators of non-visual health potential in the built environment
[Doctoral dissertation, EPFL]. EPFL infoscience.
\doi{10.5075/epfl-thesis-7146}
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
\code{\link{nvRD_cumulative_response}()},
\code{\link{period_above_threshold}()},
\code{\link{pulses_above_threshold}()},
\code{\link{threshold_for_duration}()},
\code{\link{timing_above_threshold}()}
}
\concept{metrics}

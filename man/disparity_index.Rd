% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_disparity_index.R
\name{disparity_index}
\alias{disparity_index}
\title{Disparity index}
\usage{
disparity_index(Light.vector, na.rm = FALSE, as.df = FALSE)
}
\arguments{
\item{Light.vector}{Numeric vector containing the light data.}

\item{na.rm}{Logical. Should missing values be removed? Defaults to FALSE}

\item{as.df}{Logical. Should the output be returned as a data frame? Defaults
to FALSE}
}
\value{
Single column data frame or vector.
}
\description{
This function calculates the continuous disparity index as described in
Fernández-Martínez et al. (2018).
}
\examples{
dataset1 <-
  tibble::tibble(
    Id = rep("A", 24),
    Datetime = lubridate::as_datetime(0) + lubridate::hours(0:23),
    MEDI = sample(0:1000, 24),
  )
dataset1 \%>\%
  dplyr::reframe(
    "Disparity index" = disparity_index(MEDI)
  )

}
\references{
Fernández-Martínez, M., Vicca, S., Janssens, I. A., Carnicer, J.,
Martín-Vide, J., & Peñuelas, J. (2018).
The consecutive disparity index, D: A measure of temporal variability in
ecological studies. \emph{Ecosphere}, 9(12), e02527.
\url{https://doi.org/10.1002/ecs2.2527}

Hartmeyer, S.L., Andersen, M. (2023). Towards a framework for light-dosimetry studies:
Quantification metrics. \emph{Lighting Research & Technology}.
\url{https://doi.org/10.1177/14771535231170500}
}
\seealso{
Other metrics: 
\code{\link{bright_dark_period}()},
\code{\link{centroidLE}()},
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

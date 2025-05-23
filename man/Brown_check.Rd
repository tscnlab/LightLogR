% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Brown.R
\name{Brown_check}
\alias{Brown_check}
\title{Check whether a value is within the recommended illuminance/MEDI levels by
Brown et al. (2022)}
\usage{
Brown_check(
  value,
  state,
  Brown.day = "day",
  Brown.evening = "evening",
  Brown.night = "night",
  Brown.day.th = 250,
  Brown.evening.th = 10,
  Brown.night.th = 1
)
}
\arguments{
\item{value}{Illuminance value to check against the recommendation. needs to
be numeric, can be a vector.}

\item{state}{The state from Brown et al. (2022). Needs to be a character
vector with the same length as \code{value}.}

\item{Brown.day, Brown.evening, Brown.night}{The names of the states from Brown
et al. (2022). These are the default values (\code{"day"}, \code{"evening"},
\code{"night"}), but can be changed if the names in \code{state} are different. Needs
to be a character scalar.}

\item{Brown.day.th, Brown.evening.th, Brown.night.th}{The thresholds for the
states from Brown et al. (2022). These are the default values (\code{250}, \code{10},
\code{1}), but can be changed if the thresholds should be different. Needs to be
a numeric scalar.}
}
\value{
A logical vector with the same length as \code{value} that indicates
whether the value is within the recommended illuminance levels.
}
\description{
This is a lower level function. It checks a given value against a threshold
for the states given by Brown et al. (2022). The function is vectorized. For
\code{day} the threshold is a lower limit, for \code{evening} and \code{night} the threshold
is an upper limit.
}
\examples{
states <- c("day", "evening", "night", "day")
values <- c(100, 10, 1, 300)
Brown_check(values, states)
Brown_check(values, states, Brown.day.th = 100)

}
\references{
https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001571
}
\seealso{
Other Brown: 
\code{\link{Brown2reference}()},
\code{\link{Brown_cut}()},
\code{\link{Brown_rec}()},
\code{\link{sleep_int2Brown}()}
}
\concept{Brown}

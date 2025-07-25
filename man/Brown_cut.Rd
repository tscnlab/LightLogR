% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Brown.R
\name{Brown_cut}
\alias{Brown_cut}
\title{Create a state column that cuts light levels into sections by Brown et al.
(2022)}
\usage{
Brown_cut(
  dataset,
  MEDI.colname = MEDI,
  New.state.colname = state,
  vector_cuts = c(-Inf, 1, 10, 250, Inf),
  vector_labels = "default",
  overwrite = TRUE
)
}
\arguments{
\item{dataset}{A light exposure dataframe}

\item{MEDI.colname}{The colname containing melanopic EDI values (or,
alternatively, Illuminance). Defaults to \code{MEDI}. Expects a symbol.}

\item{New.state.colname}{Name of the new column that will contain the cut
data. Expects a symbol.}

\item{vector_cuts}{Numeric vector of breaks for the cuts.}

\item{vector_labels}{Vector of labels for the cuts. Must be one entry shorter
than \code{vector_cuts}. \code{"default"} will produce nice labels for the default
setting of \code{vector_cuts} (and throw an error otherwise).}

\item{overwrite}{Logical. Should the \code{New.state.colname} overwrite a
preexisting column in the dataset}
}
\value{
The input dataset with an additional (or overwritten) column
containing a cut light vector
}
\description{
This is a convenience wrapper arount \code{\link[=cut]{cut()}} and \code{\link[dplyr:mutate]{dplyr::mutate()}}. It
creates a state column dividing a light column into recommended levels by
Brown et al. (2022). Cuts can be adjusted or extended with \code{vector_cuts} and
\code{vector_labels}
}
\examples{
sample.data.environment |>
Brown_cut(vector_labels = c("0-1lx", "1-10lx", "10-250lx", "250lx-Inf")) |>
dplyr::count(state)

}
\references{
https://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.3001571
}
\seealso{
Other Brown: 
\code{\link{Brown2reference}()},
\code{\link{Brown_check}()},
\code{\link{Brown_rec}()},
\code{\link{sleep_int2Brown}()}
}
\concept{Brown}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/track_bridge.R
\name{obscureUpper}
\alias{obscureUpper}
\title{Obscure the upper triangular portion of a frameMatrix.}
\usage{
obscureUpper(frameMatrix)
}
\arguments{
\item{frameMatrix}{}
}
\value{
A frameMatrix with the diagonal and all entries above it obscured.
}
\description{
Obscure the upper triangular portion of a frameMatrix.
}
\examples{
sbc007 = undupeLayers(sbc007, "trail", "name")
sbc007 = addFrameMatrix(sbc007)
obscureUpper(frameMatrix(sbc007))[1:10, 1:12]

}

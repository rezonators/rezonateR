% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rezrDF.R
\name{undupeSingle}
\alias{undupeSingle}
\alias{undupeLayers}
\title{De-duplicate a rezrObj entity.}
\usage{
undupeSingle(rezrObj, entity, colname)

undupeLayers(rezrObj, entity, colname)
}
\arguments{
\item{rezrObj}{The rezrObj you would like to de-duplicate.}

\item{entity}{The entity you would like to de-duplicate.}

\item{colname}{The name of a (character) column.}
}
\value{
The rezrObj, with the modified column de-duplicated so that entries with the same name will come with numbers starting from the second appearance.
}
\description{
De-duplicate a rezrObj entity.
}
\examples{
sbc007 = undupeLayers(sbc007, "trail", "name")
}

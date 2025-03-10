% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stack.R
\name{stackToUnit}
\alias{stackToUnit}
\alias{stackToToken}
\title{Add stack sequence information to tokens and units}
\usage{
stackToUnit(rezrObj, layers = character(0))

stackToToken(rezrObj, layers = character(0))
}
\arguments{
\item{rezrObj}{The \code{rezrObj} to modify.}

\item{layers}{Stack layers to be added to tokens and units.}
}
\value{
A \code{rezrObj} with stack sequence information added. The name of the column will be the column name followed by "Seq". If \code{stackToToken()} is called, the information will be added to \code{unitDF} as well (if the information is not already there). Both methods will add the information to \code{cardDF} (again, if the information is not already there.
}
\description{
Add stack sequence information to tokens and units
}

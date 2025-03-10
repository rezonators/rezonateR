% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rezrDF_mutate.R
\name{createUpdateFunction}
\alias{createUpdateFunction}
\title{Create an update function.}
\usage{
createUpdateFunction(field, x, df, groupField = "")
}
\arguments{
\item{field}{The field for which you want to create an update function.}

\item{x}{An R expression. For example, if you want to column2 to be updated to always be three times column3, then x should be column3 * 3.}

\item{df}{The rezrDF for which you want to create an update function.}
}
\value{
An update function with automatically generated dependency information. I will figure out the dependency information for you, so you don't have to define it yourself.
}
\description{
Create an update function for 'auto' fields only. 'foreign' fields take \link{createLowerToHigherUpdate} or \link{createLeftJoinUpdate}. Normally does not need to be called by the user; it is automatically called by \link{addFieldLocal} or \link{rez_mutate}.
}

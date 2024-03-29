% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io_csv.R
\name{updateFromDF}
\alias{updateFromDF}
\title{Update a rezrDF using data from another data frame}
\usage{
updateFromDF(
  targetDF,
  changeDF,
  changeCols = NULL,
  changeType = "flex",
  renameCols = F,
  colCorr = list(),
  delRows = F,
  addRows = F,
  addCols = F,
  reloadAfterCorr = T,
  rezrObj = NULL
)
}
\arguments{
\item{targetDF}{The target rezrDF.}

\item{changeDF}{A data frame, not necessarily a rezrDF, from which changes will be pulled.}

\item{changeCols}{Columns to be changed. This should include names of columns to be added.}

\item{changeType}{Which types of columns (in field access terms) will you change?}

\item{renameCols}{Will you rename columns according to the new data frame?}

\item{colCorr}{If renameCols = T, then a list where names are the new names and values are the old names. If renameCols = F, then the opposite.}

\item{delRows}{Will you delete rows from targetDF if not present in changeDF?}

\item{addRows}{Will you add rows to targetDF if not present in targetDF?}

\item{addCols}{Will you add columns present in the changeDF but not in the targetDF?}

\item{reloadAfterCorr}{Would you like to do a local reload on the rezrDF afterwards (if a rezrObj is not supplied) or a full reload (if a rezrObj is supplied)?}

\item{rezrObj}{The rezrObj, if you would like to do a full reload.}
}
\value{
The updated rezrDF.
}
\description{
Update a rezrDF using data from another data frame
}
\note{
Most often used for updating a rezrDF using data from a CSV used for manual annotation.
}
\examples{
inpath = system.file("extdata", "rez007_refexpr_edited.csv", package = "rezonateR")
changeDF = rez_read_csv(inpath, origDF = rez007$trackDF$default)
sbc007$trackDF$default = sbc007$trackDF$default \%>\% updateFromDF(changeDF, addCols = T)
}

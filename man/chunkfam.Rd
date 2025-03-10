% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree.R
\name{getChildrenOfEntry}
\alias{getChildrenOfEntry}
\alias{getChildrenOfChunk}
\alias{getChildrenOfChunkIf}
\alias{getSiblingsOfEntry}
\alias{getSiblingsOfChunk}
\alias{getSiblingOfChunkIf}
\title{Get children and siblings of tree entries and chunks.}
\usage{
getChildrenOfEntry(treeEntry, treeEntryDF)

getChildrenOfChunk(chunkID, chunkDF, treeEntryDF)

getChildrenOfChunkIf(chunkID, chunkDF, treeEntryDF, cond = expr(TRUE))

getSiblingsOfEntry(treeEntry, treeEntryDF)

getSiblingsOfChunk(chunkID, chunkDF, treeEntryDF)

getSiblingOfChunkIf(chunkID, chunkDF, treeEntryDF, cond = expr(TRUE))
}
\arguments{
\item{treeEntry}{A \code{treeEntry} ID.}

\item{treeEntryDF}{A \code{treeEntryDF} from a \code{rezrObj}. Must be a \code{rezrDF}, not a list; \code{combineLayers()} can be used to combine multiple \code{rezrDF}s.}

\item{chunkID}{A chunk ID.}

\item{chunkDF}{A \code{chunkDF}from a \code{rezrObj},  Must be a \code{rezrDF}, not a list; \code{combineChunks()} or \code{combineTokenChunk()} can be used to combine multiple \code{rezrDF}s. You must have run \code{getAllTreeCorrespondences()} on it beforehand, i.e. a \code{treeEntry} column must exist. Also, both parents and children (for child-related functions) and all relevant siblings (for sibling-related functions) must reside in the \code{chunkDF}.}

\item{cond}{A condition using columns from the \code{chunkDF}.}
}
\value{
A list of IDs of siblings or children
}
\description{
Get children and siblings of tree entries and chunks.
}
\examples{
sbc007 = getAllTreeCorrespondences(sbc007, "chunk")
getSiblingsOfChunk("33EAD4C986974", sbc007$chunkDF$refexpr, sbc007$treeEntryDF$default)
getChildrenOfChunk("13AACBE0BB38A", combineTokenChunk(sbc007), sbc007$treeEntryDF$default)
}

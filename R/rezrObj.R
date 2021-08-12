#This is a series of functions for rezrObj objects!
#Table of contents:
#1) The constructor function: new_rezrObj


#' Constructor function for rezrObj.
#'
#' @param list
#'
#' @return The rezObj.
#' @export
new_rezrObj = function(list){
  stopifnot("nodeMap" %in% names(list))
  stopifnot("tokenDF" %in% names(list))
  stopifnot("unitDF" %in% names(list))

  structure(list, class = "rezrObj")
}


#' Combine different layers of an entity, and similar entities
#'
#' @rdname combineLayer
#' @param rezrObj A rezrObj object
#' @param entity The Rezonator entity with multiple layers (e.g. track, rez, chunk) that you want to combine
#' @param type Do you want the resultant rezrDF to contain the intersection of the columns in all the components, or the union (with absent fields becoming NA)?
#'
#' @return A rezrDF containing the required material
#' @export
combineLayers = function(rezrObj, entity, type = "intersect"){
  layers = names(rezrObj[[entity %+% "DF"]])
  toRun = parse_expr("rez_rbind(" %+% paste0("rezrObj$" %+% entity %+% "DF$", layers, collapse = ", ") %+% ", type = \"" %+% type %+% "\")")
  eval(toRun)
}

#' @rdname combineLayer
#' @export
combineChunks = function(rezrObj, ...){
  combineLayers(rezrObj, "chunk", ...)
}

#' @rdname combineLayer
#' @export
combineTokenChunk = function(rezrObj, type = "intersect"){
  chunkDF = combineLayers(rezrObj, "chunk", type)

  tokenDF = tokenDF %>% rez_mutate(tokenSeqFirst = tokenSeq, tokenSeqLast = tokenSeq, discourseTokenSeqFirst = discourseTokenSeq, discousreTokenSeqLast = discourseTokenSeq)

  rez_rbind(tokenDF, chunkDF)
}



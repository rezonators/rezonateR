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

combineChunks = function(rezrObj, ...){
  combineLayers(rezrObj, "chunk", ...)
}

combineLayers = function(rezrObj, entity, type = "intersect"){
  layers = names(rezrObj[[entity %+% "DF"]])
  toRun = parse_expr("rez_rbind(" %+% paste0("rezrObj$" %+% entity %+% "DF$", layers, collapse = ", ") %+% ", type = \"" %+% type %+% "\")")
  eval(toRun)
}

combineTokenChunk = function(rezrObj, type = "intersect"){
  chunkDF = combineLayers(rezrObj, "chunk", type)

  tokenDF = tokenDF %>% rez_mutate(tokenSeqFirst = tokenSeq, tokenSeqLast = tokenSeq, discourseTokenSeqFirst = discourseTokenSeq, discousreTokenSeqLast = discourseTokenSeq)

  rez_rbind(tokenDF, chunkDF)
}



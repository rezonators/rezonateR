#This is a series of functions for rezrObj objects!
#Table of contents:
#1) The constructor function: new_rezrObj
#2) Combine different tables in the rezrObj: combineLayers, combineChunks, combineTokenChunk
#3) Add unit sequence to different Rezonator objects: addUnitSeq


#' Constructor function for rezrObj.
#'
#' Not normally called by users; automatically created in [rezonateR::importRez].
#'
#' @param list A node map that has been organised into nodeLists, one for each entity.
#'
#' @return The rezrObj, consisting of a series of rezrDFs and a nodeMap.
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
#' @inheritParams rez_bind_rows
#' @param rezrObj A rezrObj object
#' @param entity The Rezonator entity with multiple layers (e.g. track, rez, chunk) that you want to combine
#'
#' @return A rezrDF containing the required material.
#' @note This is mainly used when you need to draw information from more than one rezrDF for the purpose of functions like [rezonateR::addFieldForeign] or [rezonateR::rez_left_join].
#' @export
combineLayers = function(rezrObj, entity, type = "intersect"){
  layers = names(rezrObj[[entity %+% "DF"]])
  toRun = parse_expr("rez_bind_rows(" %+% paste0("rezrObj$" %+% entity %+% "DF$", layers, collapse = ", ") %+% ", type = \"" %+% type %+% "\")")
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
  if("chunkDF" %in% names(rezrObj)){
      chunkDF = combineChunks(rezrObj, type)

      tokenDF = rezrObj$tokenDF %>% rez_mutate(tokenSeqFirst = tokenSeq, tokenSeqLast = tokenSeq, discourseTokenSeqFirst = discourseTokenSeq, discousreTokenSeqLast = discourseTokenSeq)

      otherSeqHeaders = c("word", "discourseWord", "unit")
      for(header in otherSeqHeaders){
        seq = header %+% "Seq"
        last = header %+% "SeqLast"
        first = header %+% "SeqFirst"
        if((seq %in% names(tokenDF)) & (last %in% names(chunkDF)) & (first %in% names(chunkDF))){
          tokenDF = tokenDF %>% rez_mutate(!!last := !!parse_expr(seq), !!first := !!parse_expr(seq))
        }
      }

      rez_bind_rows(tokenDF, chunkDF)
  } else {
    rezrObj$tokenDF
  }
}

#' Save and load rezrObj and rezrDF objects.
#'
#' @rdname saveload
#' @param obj The object you would like to save.
#' @param filename The filename, ending in .Rdata, you would like to save to.
#'
#' @return For rez_load, the saved rezrObj or rezrDF.
#' @export
rez_save = function(obj, filename){
  stopifnot("rezrObj" %in% class(obj) | "rezrDF" %in% class(obj))
  if("rezrObj" %in% class(obj)) message("Saving rezrObj ...") else message("Saving rezrDF ...")
  save(obj, file = filename, eval.promises = F)
}

#' @rdname saveload
#' @export
rez_load = function(filename){
  rezEx = load(filename)
  if("rezrObj" %in% class(obj)) message("Loading rezrObj ...") else message("Loading rezrDF ...")
  obj
}

getChunkAddresses = function(rezrObj){
  "chunkDF/" %+% names(rezrObj$chunkDF)
}


getTokenChunkAddresses = function(rezrObj){
  c("tokenDF", getChunkAddresses(rezrObj))
}

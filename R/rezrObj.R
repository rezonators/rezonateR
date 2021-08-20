#This is a series of functions for rezrObj objects!
#Table of contents:
#1) The constructor function: new_rezrObj
#2) Combine different tables in the rezrObj: combineLayers, combineChunks, combineTokenChunk
#3) Add unit sequence to different Rezonator objects: addUnitSeq


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

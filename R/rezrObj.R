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
}

#' Add unit sequence information to different levels of rezrObj.
#'
#' @param rezrObj The rezrObj objec
#' @inheritParams addFieldLocal.rezrObj
#' @param layers The layers to which unit sequence information is to be added.
#'
#' @return The rezrObj object with unit sequences in the entity desired, plus all levels below. For example, if your entity is 'track', you will see unitSeq information on token and chunk too, but not rez.
#' @export
addUnitSeq = function(rezrObj, entity, layers = ""){
  #If no layers are specified, just grab them all
  if(layers == ""){
    layers = names(rezrObj[[entity %+% "DF"]])
  }

  if(entity == "token"){
    rezrObj = rezrObj %>% addFieldForeign("token", "", "unit", "", "unit", "unitSeq", "unitSeq", fieldaccess = "foreign") #add unitSeq column to tokenDF
  } else if(entity == "chunk"){
    if(!("unitSeq" %in% names(rezrObj$tokenDF))){
      rezrObj = rezrObj %>% addUnitSeq("token")
    }
    for(layer in layers){
      rezrObj = rezrObj %>% addFieldForeign("chunk", layer, "token", "", "tokenList", "unitSeqFirst", "unitSeq", type = "complex", fieldaccess = "foreign", complexAction = min) #add unitSeq column to tokenDF
      rezrObj = rezrObj %>% addFieldForeign("chunk", layer, "token", "", "tokenList", "unitSeqLast", "unitSeq", type = "complex", fieldaccess = "foreign", complexAction = max) #add unitSeq column to tokenDF
    }
  } else if(entity %in% c("track", "rez")){
    chunkLayers =  names(rezrObj$chunkDF)
    for(layer in chunkLayers){
      if(!("unitSeqFirst" %in% names(rezrObj$chunkDF[[layer]])) | !("unitSeqLast" %in% names(rezrObj$chunkDF[[layer]]))){
        rezrObj$chunkDF[[layer]] = killIfPresent(rezrObj$chunkDF[[layer]], c("unitSeqFirst", "unitSeqLast"))
        rezrObj = rezrObj %>% addUnitSeq("chunk", layer)
      }
    }

    sourceAddress = c("track", "chunk/" %+% chunkLayers)
    for(layer in layers){
      rezrObj[[entity %+% "DF"]][[layer]] = suppressMessages(rezrObj[[entity %+% "DF"]][[layer]] %>% rez_left_join(combineTokenChunk(rezrObj) %>% rez_select(id, unitSeqFirst, unitSeqLast), df2Address = sourceAddress, rezrObj = rezrObj, fkey = "token"))
    }
  }
  rezrObj
}



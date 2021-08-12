#A group of functions for dealing with issues like coreference resolution, reference production, referentiality, and more!

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

lastAppearance = function(unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  if(is.null(unitSeq)) unitSeq = parent(environment())[["unitSeq"]]
  if(is.null(chain)) chain = parent(environment())[["chain"]]

  for(i in 1:length(unitSeq)){

  }
}

lastAppearanceEntry = function(unitSeq, chain){

}

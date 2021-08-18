#Features for dealing with the temporal dimension of language.

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
    if("chunkDF" %in% rezrObj){
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
    } else {
      if(!("unitSeq" %in% names(rezrObj$tokenDF))){
        rezrObj = rezrObj %>% addUnitSeq("token")
      }
      rezrObj[[entity %+% "DF"]][[layer]] = suppressMessages(rezrObj[[entity %+% "DF"]][[layer]] %>% rez_left_join(rezrObj$tokenDF %>% rez_select(id, unitSeqFirst, unitSeqLast), df2Address = sourceAddress, rezrObj = rezrObj, fkey = "token"))
    }
  }
  rezrObj
}


#' Add a field on whether something is a word or not.
#'
#' @rdname addIsWordField
#' @param x The rezrDF or rezrObj to be edited.
#' @param cond The wordhood condition.
#' @param addWordSeq True if you want to add wordSeq and discourseWordSeq?
#'
#' @return The modified rezrDF or rezrObj.
#' @note If used on a rezrObj and addWordSeq = T, wordSeq and discourseWordSeq are automatically added to entry, unit, chunk and track tables. Rez and stack tables coming soon.
#' @export
addIsWordField.rezrDF = function(x, cond, addWordSeq = T){
  if("isWord" %in% x) message("This action overrides the original isWord field in your tokenDF.")
  cond = enexpr(cond)
  result = rez_mutate(x, isWord = !!cond, fieldaccess = "auto")
  if(addWordSeq){
    wordSeq = numeric(nrow(result))
    result = rez_mutate(result, wordSeq = getWordSeq(isWord, unit, tokenSeq), fieldaccess = "auto")
    result = rez_mutate(result, discourseWordSeq = getDiscourseWordSeq(isWord, discourseTokenSeq), fieldaccess = "auto")
  }
  result
}

#' @rdname addIsWordField
#' @export
addIsWordField.rezrObj = function(x, cond, addWordSeq = T){
  x$tokenDF = addIsWordField(x$tokenDF, !!enexpr(cond), addWordSeq)
  if(addWordSeq){
    #Chunk and dependencies
    if("chunkDF" %in% rezrObj){
      for(layer in names(x$chunkDF)){
        x$chunkDF[[layer]] = getSeqBounds(x$tokenDF, x$chunkDF[[layer]], x$nodeMap$chunk, c("wordSeq", "discourseWordSeq"), simpleDFAddress = "tokenDF", complexNodeMapAddress = "chunk")
      }
    }

    for(layer in names(x$trackDF)){
      x$trackDF[[layer]] = suppressMessages(x$trackDF[[layer]] %>% rez_left_join(combineTokenChunk(x) %>% rez_select(id, wordSeqFirst, wordSeqLast, discourseWordSeqFirst, discourseWordSeqLast), df2Address = "trackDF", rezrObj = x, fkey = "token"))
    }


    #Entry and dependencies
    x$entryDF = x$entryDF %>% rez_left_join(x$tokenDF %>% select(id, wordSeq, discourseWordSeq), by = c(token = "id"), df2Address = "token", fkey = "token", rezrObj = x)
    x$unitDF = getSeqBounds(x$entryDF, x$unitDF, x$nodeMap$unit, "discourseWordSeq", tokenListName = "entryList", simpleDFAddress = "entryDF", complexNodeMapAddress = "unit")

  }
  x
}

getWordSeq = function(isWord, unit, tokenSeq){
  result = numeric(length(isWord))
  for(currUnit in unique(unit)){
    isWordSubset = isWord[unit == currUnit]
    tokenSeqSubset = tokenSeq[unit == currUnit]

    noWord = length(isWordSubset[isWordSubset])
    isWordSubsetOrdered = sapply(1:length(isWordSubset), function(i) isWordSubset[which(rank(tokenSeqSubset) == i)])

    resultSubsetOrdered = numeric(length(isWordSubset))
    resultSubsetOrdered[isWordSubsetOrdered] = 1:noWord
    result[unit == currUnit] = resultSubsetOrdered[rank(tokenSeqSubset)]
  }
  result
}

getDiscourseWordSeq = function(isWord, discourseTokenSeq){
  noWord = length(isWord[isWord])
  isWordOrdered = sapply(1:length(isWord), function(i) isWord[which(rank(discourseTokenSeq) == i)])
  resultOrdered = numeric(length(isWord))
  resultOrdered[isWordOrdered] = 1:noWord
  resultOrdered[rank(discourseTokenSeq)]
}

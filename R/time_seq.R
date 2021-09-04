#Features for dealing with the temporal dimension of language.

#' Add unit sequence information to different levels of rezrObj.
#'
#' @param rezrObj The rezrObj object
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

    if("chunkDF" %in% names(rezrObj)){
      chunkLayers =  names(rezrObj$chunkDF)
      for(layer in chunkLayers){
        if(!("unitSeqFirst" %in% names(rezrObj$chunkDF[[layer]])) | !("unitSeqLast" %in% names(rezrObj$chunkDF[[layer]]))){
          rezrObj$chunkDF[[layer]] = killIfPresent(rezrObj$chunkDF[[layer]], c("unitSeqFirst", "unitSeqLast"))
          rezrObj = rezrObj %>% addUnitSeq("chunk", layer)
        }
      }

      for(layer in layers){
        rezrObj[[entity %+% "DF"]][[layer]] = suppressMessages(rezrObj[[entity %+% "DF"]][[layer]] %>% rez_left_join(combineTokenChunk(rezrObj) %>% rez_select(id, unitSeqFirst, unitSeqLast), df2Address = "tokenChunkDF", rezrObj = rezrObj, fkey = "token"))
      }
    } else {
      if(!("unitSeq" %in% names(rezrObj$tokenDF))){
        rezrObj = rezrObj %>% addUnitSeq("token")
      }
      for(layer in layers){
        rezrObj[[entity %+% "DF"]][[layer]] = suppressMessages(rezrObj[[entity %+% "DF"]][[layer]] %>% rez_left_join(rezrObj$tokenDF %>% rez_select(id, unitSeqFirst, unitSeqLast), df2Address = sourceAddress, rezrObj = rezrObj, fkey = "token"))
      }
    }
  }
  rezrObj
}


#' Add a field on whether something is a word or not.
#'
#' @rdname addIsWordField
#' @param x The rezrDF or rezrObj to be edited.
#' @param cond The wordhood condition. For example, if your word column is called 'word', and you wish to exclude zeroes, you may write 'x == "<0>"'.
#' @param addWordSeq If TRUE, the columns wordOrder and docWordSeq will be added.
#'
#' @return The modified rezrDF or rezrObj. If addWordSeq is set to TRUE, the columns wordOrder and docWordSeq will be added to tokenDF and entryDF, and the columns wordOrderFirst, wordOrderLast, docWordSeqFirst and docWordSeqLast will be added to unitDF, chunkDF and trackDF.
#' @note If used on a rezrObj and addWordSeq = T, wordOrder and docWordSeq are automatically added to entry, unit, chunk and track tables. Rez and stack tables coming soon.
#' @export
addIsWordField.rezrDF = function(x, cond, addWordSeq = T){
  if("isWord" %in% x) message("This action overrides the original isWord field in your tokenDF.")
  cond = enexpr(cond)
  result = rez_mutate(x, isWord = !!cond, fieldaccess = "auto")
  if(addWordSeq){
    wordOrder = numeric(nrow(result))
    result = rez_mutate(result, wordOrder = getWordSeq(isWord, unit, tokenOrder), fieldaccess = "auto")
    result = rez_mutate(result, docWordSeq = getDiscourseWordSeq(isWord, docTokenSeq), fieldaccess = "auto")
  }
  result
}

#' @rdname addIsWordField
#' @export
addIsWordField.rezrObj = function(x, cond, addWordSeq = T){
  x$tokenDF = addIsWordField(x$tokenDF, !!enexpr(cond), addWordSeq)
  if(addWordSeq){
    #Chunk and dependencies
    if("chunkDF" %in% names(x)){
      for(layer in names(x$chunkDF)){
        x$chunkDF[[layer]] = getSeqBounds(x$tokenDF, x$chunkDF[[layer]], x$nodeMap$chunk, c("wordOrder", "docWordSeq"), simpleDFAddress = "tokenDF", complexNodeMapAddress = "chunk")
      }
    }


    for(layer in names(x$trackDF)){
      x$trackDF[[layer]] = suppressMessages(x$trackDF[[layer]] %>% rez_left_join(combineTokenChunk(x) %>% rez_select(id, wordOrderFirst, wordOrderLast, docWordSeqFirst, docWordSeqLast), df2Address = "tokenChunkDF", rezrObj = x, fkey = "token"))
    }


    #Entry and dependencies
    x$entryDF = x$entryDF %>% rez_left_join(x$tokenDF %>% select(id, wordOrder, docWordSeq), by = c(token = "id"), df2Address = "token", fkey = "token", rezrObj = x)
    x$unitDF = getSeqBounds(x$entryDF, x$unitDF, x$nodeMap$unit, "docWordSeq", tokenListName = "entryList", simpleDFAddress = "entryDF", complexNodeMapAddress = "unit")

  }
  x
}

getWordSeq = function(isWord, unit, tokenOrder){
  result = numeric(length(isWord))
  for(currUnit in unique(unit)){
    isWordSubset = isWord[unit == currUnit]
    tokenOrderSubset = tokenOrder[unit == currUnit]

    noWord = length(isWordSubset[isWordSubset])
    isWordSubsetOrdered = sapply(1:length(isWordSubset), function(i) isWordSubset[which(rank(tokenOrderSubset) == i)])

    resultSubsetOrdered = numeric(length(isWordSubset))
    resultSubsetOrdered[isWordSubsetOrdered] = 1:noWord
    result[unit == currUnit] = resultSubsetOrdered[rank(tokenOrderSubset)]
  }
  result
}

getDiscourseWordSeq = function(isWord, docTokenSeq){
  noWord = length(isWord[isWord])
  isWordOrdered = sapply(1:length(isWord), function(i) isWord[which(rank(docTokenSeq) == i)])
  resultOrdered = numeric(length(isWord))
  resultOrdered[isWordOrdered] = 1:noWord
  resultOrdered[rank(docTokenSeq)]
}

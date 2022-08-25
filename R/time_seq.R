#Features for dealing with the temporal dimension of language.

#' Add unit sequence information to different levels of rezrObj.
#'
#' @param rezrObj The rezrObj object
#' @inheritParams addFieldLocal.rezrObj
#' @param layers The layers to which unit sequence information is to be added.
#'
#' @return The rezrObj object with unit sequences in the entity desired, plus all levels below. For example, if your entity is 'track', you will see unitSeq information on token and chunk too, but not rez.
#' @examples sbc007_withunits = addUnitSeq(sbc007, "track")
#' head(sbc007_withunits$trackDF$default %>% select(id, text, unitSeqFirst, unitSeqLast))
#' sbc007_withunits = addUnitSeq(sbc007, "stack")
#' head(sbc007_withunits$stackDF %>% select(id, unitSeqFirst, unitSeqLast))
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
  } else if (entity == "card"){
    rezrObj = rezrObj %>% addFieldForeign("card", "", "unit", "", "unit", "unitSeq", "unitSeq", fieldaccess = "foreign")
  } else if (entity == "stack"){
    if(!("unitSeq" %in% names(rezrObj$cardDF))){
      rezrObj = rezrObj %>% addUnitSeq("card")
    }
    #FUTURE TODO: After layers are implemented for stacks, rewrite this to take layers into account
    rezrObj = rezrObj %>% addFieldForeign("stack", "", "card", "", "setIDList", "unitSeqFirst", "unitSeq", type = "complex", fieldaccess = "foreign", complexAction = min)
    rezrObj = rezrObj %>% addFieldForeign("stack", "", "card", "", "setIDList", "unitSeqLast", "unitSeq", type = "complex", fieldaccess = "foreign", complexAction = max)
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
#' @return The modified `rezrDF` or `rezrObj.` If `addWordSeq` is set to `TRUE`, the columns `wordOrder` and `docWordSeq` will be added to `tokenDF` and `entryDF`, and the columns `wordOrderFirst`, `wordOrderLast`, `docWordSeqFirst` and `docWordSeqLast` will be added to `unitDF`, `chunkDF`, `rezDF` and `trackDF.`
#' @note If used on a `rezrObj` and `addWordSeq = T`, `wordOrder` and `docWordSeq` are automatically added to `entry`, `unit`, `chunk`, `rez` and `track` tables.
#' @examples sbc007_withword = addIsWordField(sbc007, kind == "Word")
#' head(sbc007_withword$chunkDF$refexpr %>% select(id, text, tokenOrderFirst, wordOrderFirst, docTokenSeqLast, docWordSeqLast))
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
        x$chunkDF[[layer]] = suppressWarnings(getSeqBounds(x$tokenDF, x$chunkDF[[layer]], x$nodeMap$chunk, c("wordOrder", "docWordSeq"), simpleDFAddress = "tokenDF", complexNodeMapAddress = "chunk"))
      }
    }

    for(layer in names(x$trackDF)){
      x$trackDF[[layer]] = suppressMessages(x$trackDF[[layer]] %>% rez_left_join(combineTokenChunk(x) %>% rez_select(id, wordOrderFirst, wordOrderLast, docWordSeqFirst, docWordSeqLast), df2Address = "tokenChunkDF", rezrObj = x, fkey = "token"))
    }

    for(layer in names(x$rezDF)){
      x$rezDF[[layer]] = suppressMessages(x$rezDF[[layer]] %>% rez_left_join(combineTokenChunk(x) %>% rez_select(id, wordOrderFirst, wordOrderLast, docWordSeqFirst, docWordSeqLast), df2Address = "tokenChunkDF", rezrObj = x, fkey = "token"))
    }



    #Entry and dependencies
    x$entryDF = x$entryDF %>% rez_left_join(x$tokenDF %>% select(id, wordOrder, docWordSeq), by = c(token = "id"), df2Address = "token", fkey = "token", rezrObj = x)
    x$unitDF = suppressWarnings(getSeqBounds(x$entryDF, x$unitDF, x$nodeMap$unit, "docWordSeq", tokenListName = "entryList", simpleDFAddress = "entryDF", complexNodeMapAddress = "unit"))

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


#' Functions for getting information on whether something is located at the beginning or end of a larger structure.
#'
#' @rdname initfin
#' @inheritparams complexActions
#' @param seq Name of the column containing the sequence value to be taken into account.
#'
#'
#' @export
isInitial = function(seq){
  as.integer(seq) == 1
}

#' @rdname initfin
#' @param length Name of the column containing the maximum sequence value.
#' @export
isFinal = function(seq, length){
  as.integer(seq) == length
}


#' Get BILUO (begin-intermediate-last-unique-outside) values from a sequence value.
#'
#' @param seq The sequence number of the entity within a larger structure.
#' For example, `tokenOrder` for the position of a token within a unit.
#' @param length The length of the sequence, which you can get from `inLength()`
#'
#' @return A vector of BILUO values corresponding to the sequence values. B = beginning of the larger structure,
#' L = last element of the larger structure, I = intermediate element of the larger structure,
#' U = only element of the larger structure, O = not within the larger structure.
#' @export
#'
#' @examples
getBiluoFromOrder = function(seq, length){
  init = isInitial(seq)
  fin = isFinal(seq, length)
  case_when(init & fin ~ "U",
            init ~ "B",
            fin ~ "L",
            as.numeric(seq) == "0" ~ "O",
            is.na(seq) | seq == 0 | seq == "" ~ "O",
            T ~ "I")
}

#' Get sequence number (position) within a larger structure (turn, prosodic sentence, etc.) from the ID of that structure, or the other way around
#' @rdname orderseq
#' @param id The ID of the larger structure within which you would like to find the position of an individual component.
#' For example, if you want to find the position of a word within a prosodic sentence from the prosodic sentence ID.
#'
#' @return A vector of sequence values within that structure. For example, if you are working with the tokenDF and `id`
#' gives the prosodic sentence ID, then this function will return the position of a token within a prosodic sentence.
#' @export
#'
#' @examples
getOrderFromSeq = function(id){
  ids = unique(id[!is.na(id) & id != 0])
  ord = integer(length(id))
  for(currID in ids){
    ord[id == currID] = seq(1,length(id[id == currID]))
  }
  ord
}


#' @rdname orderseq
#' @param id The ID of the larger structure within which you would like to find the position of an individual component.
#' For example, if you want to find the position of a word within a prosodic sentence from the prosodic sentence ID.
#' @param order The position of an individual component within a larger structure.
#'
#' @return A vector of either order values within the structure . For example, if you are working with the `tokenDF` and `id`
#' gives the prosodic sentence ID, then `getOrderFormSeq` will return the position of a token within a prosodic sentence. If `order` gives the position within the prosodic sentence, then `getSeqFromOrder` will return the prosodic sentence sequence.
#' @export
#'
#' @examples
getSeqFromOrder = function(order){
  structBegins = which(order == 1)
  result = integer(length(order))

  for(i in 1:(length(structBegins) - 1)){
    result[structBegins[i]:(structBegins[i+1]-1)] = i
  }
  result[structBegins[length(structBegins)]:length(order)] = i + 1
  result[order==0 | order == "" | is.na(order)] = 0
  result
}


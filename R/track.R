#A group of functions for dealing with issues like coreference resolution, reference production, referentiality, and more!
#1) Important, purely internal function: If a certain argument isn't specified, grab it from the DF with the default column name
#2) Functions related to previous context:
#  a) (Distance to) last mention: lastMention, distToLastMention
#

grabFromDF = function(...){
  funct_env = caller_env()
  df_env = env_parent(caller_env(n = 2))
  args = list(...)
  for(var in names(args)){
    if(!(var %in% names(funct_env)) | is.null(funct_env[[var]])){
      funct_env[[var]] = df_env[[args[[var]]]]
    }
  }
}

#' Functions related to previous context in track chains.
#'
#' @rdname trackPrevContext
#' @param unitSeq The vector of units where the mentions appeared.
#' @param chain The chain that each mention belongs to.
#' @export
lastMentionUnit = function(unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain")

  result = numeric(length(unitSeq))
  for(currChain in unique(chain)){
    chainUnits = unitSeq[chain == currChain] #Grab all the PRE's units
    unitsOrdered = sort(chainUnits) #Put them in the right order
    unitsOrderedLagged = lag(unitsOrdered) #Get the previous unit
    result[chain == currChain] = unitsOrderedLagged[rank(chainUnits)] #Put it back in the wrong order
  }
  result
}

#' @rdname trackPrevContext
#' @param unitSeq The vector of units where the mentions appeared.
#' @export
unitsToLastMention = function(unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain")

  unitSeq - lastMentionUnit(unitSeq, chain)
}

#' @rdname trackPrevContext
#' @param unitSeq The vector of tokenSeq values where the mentions appeared. You can choose tokenSeqFirst, tokenSeqLast, or maybe an average of the two. By default it's tokenSeqLast.
#' @export
lastMentionToken = function(tokenSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(tokenSeq = "tokenSeqLast", chain = "chain")

  result = numeric(length(tokenSeq))
  for(currChain in unique(chain)){
    chainTokens = tokenSeq[chain == currChain] #Grab all the PRE's units
    tokensOrdered = sort(chainTokens) #Put them in the right order
    tokensOrderedLagged = lag(tokensOrdered) #Get the previous unit
    result[chain == currChain] = tokensOrderedLagged[rank(chainTokens)] #Put it back in the wrong order
  }
  result
}

#' @rdname trackPrevContext
#' @export
tokensToLastMention = function(tokenSeq = NULL, chain = NULL, zeroProtocol = "literal", zeroCond = NULL, unitSeq = NULL, unitDF = NULL){
  #Get the default column names from the rezrDF environment if unspecified
  grabFromDF(tokenSeq = "discourseTokenSeqLast", chain = "chain")

  if(zeroProtocol == "literal"){
    result = tokenSeq - lastMentionToken(tokenSeq, chain)
  } else if(zeroProtocol == "unitLast"){
    #Validation
    stopifnot(!is.null(zeroCond))
    grabFromDF(unitSeq = "unitSeqLast")
    stopifnot(!is.null(unitSeq))
    stopifnot(!is.null(unitDF))

    prevUnit = unitSeq - lastMentionUnit(unitSeq, chain) #Prev units
    prevToken = sapply(prevUnit, function(x){
      if(!is.na(x)){
        (unitDF %>% filter(unitSeq == x) %>% pull(discourseTokenSeqLast))[1]
      } else NA
    })
    result = tokenSeq - prevUnit

  }
  result
}

#' @rdname trackPrevContext
#' @export
noPrevMentions = function(windowSize, unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain")

  sapply(1:length(unitSeq), function(i){
    window = unitSeq[chain == chain[i] & unitSeq < unitSeq[i] & unitSeq >= unitSeq[i] - windowSize]
    length(window)
  })
}

noPrevMentionsIf = function(windowSize, cond, unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain")

  condition = eval_bare(enexpr(cond), env = env_parent(caller_env()))

  sapply(1:length(unitSeq), function(i){
    window = unitSeq[chain == chain[i] & unitSeq < unitSeq[i] & unitSeq >= unitSeq[i] - windowSize & condition]
    length(window)
  })

}

noPrevMentionsMatch = function(windowSize, field, unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain")

  sapply(1:length(unitSeq), function(i){
    window = unitSeq[chain == chain[i] & unitSeq < unitSeq[i] & unitSeq >= unitSeq[i] - windowSize & field == field[i]]
    length(window)
  })

}

getPrevMentionField = function(field, tokenSeq = NULL, chain = NULL){
  grabFromDF(tokenSeq = "discourseTokenSeqLast", chain = "chain")
  lastMentionPos = lastMentionToken(tokenSeq = tokenSeq, chain = chain)
  sapply(1:length(field), function(i) field[which(tokenSeq == lastMentionPos[i])]) %>% zeroEntryToNA
}


addIsWordField.rezrDF = function(x, cond, addWordSeq = T){
  if("isWord" %in% x) message("This action overrides the original isWord field in your tokenDF.")
  cond = enexpr(cond)
  result = rez_mutate(x, isWord = !!cond, fieldaccess = "auto")
  if(addWordSeq){
    wordSeq = numeric(nrow(result))
    print("wordSeq" %in% names(result))
    result = rez_mutate(result, wordSeq = getWordSeq(isWord, unit, tokenSeq), fieldaccess = "auto")
    result = rez_mutate(result, discourseWordSeq = getDiscourseWordSeq(isWord, discourseTokenSeq), fieldaccess = "auto")
  }
  result
}

addIsWordField.rezrObj = function(x, cond, addWordSeq = T){
  x$tokenDF = addIsWordField(x$tokenDF, !!enexpr(cond), addWordSeq)
  if(addWordSeq){
    #Chunk and dependencies
    for(layer in names(x$chunkDF)){
      x$chunkDF[[layer]] = getSeqBounds(x$tokenDF, x$chunkDF[[layer]], x$nodeMap$chunk, c("wordSeq", "discourseWordSeq"), simpleDFAddress = "tokenDF", complexNodeMapAddress = "chunk")
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


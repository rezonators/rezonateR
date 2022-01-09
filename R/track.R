#A group of functions for dealing with issues like coreference resolution, reference production, referentiality, and more!
#1) Important, purely internal function: If a certain argument isn't specified, grab it from the DF with the default column name
#2) Functions related to previous context:
#  a) (Distance to) last mention: lastMentionUnit, distToLastMention
# (complete this tmr I want to focus on showing Jack)

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

#' Functions related to mentions of the same entity in previous/following context in track chains.
#'
#' See also [rezonateR::countCompetitors].
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
#' @param tokenOrder The vector of sequence values values where the mentions appeared. Common choices are docTokenSeqFirst, docTokenSeqLast, wordTokenSeqFirst and wordTokenseqLast (the last two are available after running [rezonateR::addIsWordField] on a rezrObj. By default it's docTokenSeqLast.
#' @export
lastMentionToken = function(tokenOrder = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(tokenOrder = "docTokenSeqLast", chain = "chain")

  result = numeric(length(tokenOrder))
  for(currChain in unique(chain)){
    chainTokens = tokenOrder[chain == currChain] #Grab all the PRE's units
    tokensOrdered = sort(chainTokens) #Put them in the right order
    tokensOrderedLagged = lag(tokensOrdered) #Get the previous token
    result[chain == currChain] = tokensOrderedLagged[rank(chainTokens)] #Put it back in the wrong order
  }
  result
}

#' @rdname trackPrevContext
#' @param zeroProtocol If 'literal', I will take the seq values of the zeroes at face value. (If you set zeros as non-words and use docWordSeqFirst or discourseWordSeLast as your tokenOrder, this will lead to meaningless values.) If 'unitFinal', I will treat zeroes as if they were the final word of the unit. If 'unitFirst', I will treat zeroes as if they were the first word of the unit.
#' @param zeroCond A condition for determining whether a token is zero. For most people, this should be (word column) == "<0>".
#' @export
tokensToLastMention = function(tokenOrder = NULL, chain = NULL, zeroProtocol = "literal", zeroCond = NULL, unitSeq = NULL, unitDF = NULL){
  #Get the default column names from the rezrDF environment if unspecified
  grabFromDF(tokenOrder = "docTokenSeqLast", chain = "chain")

  if(zeroProtocol == "literal"){
    result = tokenOrder - lastMentionToken(tokenOrder, chain)
  } else if(zeroProtocol == "unitFinal"){
    #Validation
    stopifnot(!is.null(zeroCond))
    grabFromDF(unitSeq = "unitSeqLast")
    stopifnot(!is.null(unitSeq))
    stopifnot(!is.null(unitDF))

    prevUnit = lastMentionUnit(unitSeq, chain) #Prev units
    prevToken = sapply(prevUnit, function(x){
      if(!is.na(x)){
        unitDF %>% filter(unitSeq == x) %>% slice(1) %>% pull(docTokenSeqLast)
      } else NA
    })
    result = sapply(tokenOrder - prevToken, function(x) max(x, 0))
  } else if(zeroProtocol == "unitInitial"){
    #Validation
    stopifnot(!is.null(zeroCond))
    grabFromDF(unitSeq = "unitSeqFirst")
    stopifnot(!is.null(unitSeq))
    stopifnot(!is.null(unitDF))

    prevUnit = lastMentionUnit(unitSeq, chain) #Prev units
    prevToken = sapply(prevUnit, function(x){
      if(!is.na(x)){
        unitDF %>% filter(unitSeq == x) %>% slice(1) %>% pull(docTokenSeqFirst)
      } else NA
    })
    result = sapply(tokenOrder - prevToken, function(x) max(x, 0))

  }
  result
}

#' @rdname trackPrevContext
#' @param windowSize The size of the window in which you will be counting.
#' @export
countPrevMentions = function(windowSize, unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain")
  sapply(1:length(unitSeq), function(i){
    window = unitSeq[chain == chain[i] & unitSeq < unitSeq[i] & unitSeq >= unitSeq[i] - windowSize]
    length(window)
  })
}

#' @rdname trackPrevContext
#' @param cond For if functions, the condition that the previous / next mention must satisfy. It cannot refer to the current mention.
#' @export
countPrevMentionsIf = function(windowSize, cond, unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain")

  condition = eval_bare(enexpr(cond), env = env_parent(caller_env()))

  sapply(1:length(unitSeq), function(i){
    window = unitSeq[chain == chain[i] & unitSeq < unitSeq[i] & unitSeq >= unitSeq[i] - windowSize & condition]
    length(window)
  })

}

#' @rdname trackPrevContext
#' @param field The field whose value you want to match or extract.
#' @export
countPrevMentionsMatch = function(windowSize, field, unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain")

  sapply(1:length(unitSeq), function(i){
    window = unitSeq[chain == chain[i] & unitSeq < unitSeq[i] & unitSeq >= unitSeq[i] - windowSize & field == field[i]]
    length(window)
  })

}

#' @rdname trackPrevContext
#' @param field The field whose value you want to match or extract.
#' @export
getPrevMentionField = function(field, tokenOrder = NULL, chain = NULL){
  grabFromDF(tokenOrder = "docTokenSeqLast", chain = "chain")
  lastMentionPos = lastMentionToken(tokenOrder = tokenOrder, chain = chain)
  sapply(1:length(field), function(i) field[which(tokenOrder == lastMentionPos[i])]) %>% zeroEntryToNA
}



#' @rdname trackPrevContext
#' @export
nextMentionUnit = function(unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqFirst", chain = "chain")

  result = numeric(length(unitSeq))
  for(currChain in unique(chain)){
    chainUnits = unitSeq[chain == currChain] #Grab all the PRE's units
    unitsOrdered = sort(chainUnits) #Put them in the right order
    unitsOrderedLagged = lag(unitsOrdered) #Get the nextious unit
    result[chain == currChain] = unitsOrderedLagged[rank(chainUnits)] #Put it back in the wrong order
  }
  result
}

#' @rdname trackPrevContext
#' @param unitSeq The vector of units where the mentions appeared.
#' @export
unitsToNextMention = function(unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqFirst", chain = "chain")

  nextMentionUnit(unitSeq, chain) - unitSeq
}

#' @rdname trackPrevContext
#' @param unitSeq The vector of tokenOrder values where the mentions appeared. You can choose tokenOrderFirst, tokenOrderFirst, or maybe an average of the two. By default it's tokenOrderFirst.
#' @export
nextMentionToken = function(tokenOrder = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(tokenOrder = "tokenOrderFirst", chain = "chain")

  result = numeric(length(tokenOrder))
  for(currChain in unique(chain)){
    chainTokens = tokenOrder[chain == currChain] #Grab all the PRE's units
    tokensOrdered = sort(chainTokens) #Put them in the right order
    tokensOrderedLagged = lag(tokensOrdered) #Get the nextious unit
    result[chain == currChain] = tokensOrderedLagged[rank(chainTokens)] #Put it back in the wrong order
  }
  result
}

#' @rdname trackPrevContext
#' @export
tokensToNextMention = function(tokenOrder = NULL, chain = NULL, zeroProtocol = "literal", zeroCond = NULL, unitSeq = NULL, unitDF = NULL){
  #Get the default column names from the rezrDF environment if unspecified
  grabFromDF(tokenOrder = "docTokenSeqFirst", chain = "chain")

  if(zeroProtocol == "literal"){
    result = nextMentionToken(tokenOrder, chain) - tokenOrder
  } else if(zeroProtocol == "unitFinal"){
    #Validation
    stopifnot(!is.null(zeroCond))
    grabFromDF(unitSeq = "unitSeqLast")
    stopifnot(!is.null(unitSeq))
    stopifnot(!is.null(unitDF))

    nextUnit = nextMentionUnit(unitSeq, chain) #Next units
    nextToken = sapply(nextUnit, function(x){
      if(!is.na(x)){
        unitDF %>% filter(unitSeq == x) %>% slice(1) %>% pull(docTokenSeqLast)
      } else NA
    })
    result = nextToken - tokenOrder

  } else if(zeroProtocol == "unitInitial"){
    #Validation
    stopifnot(!is.null(zeroCond))
    grabFromDF(unitSeq = "unitSeqFirst")
    stopifnot(!is.null(unitSeq))
    stopifnot(!is.null(unitDF))

    nextUnit = nextMentionUnit(unitSeq, chain) #Next units
    nextToken = sapply(nextUnit, function(x){
      if(!is.na(x)){
        unitDF %>% filter(unitSeq == x) %>% slice(1) %>% pull(docTokenSeqFirst)
      } else NA
    })
    result = nextToken - tokenOrder

  }
  result
}

#' @rdname trackPrevContext
#' @export
countNextMentions = function(windowSize, unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqFirst", chain = "chain")

  sapply(1:length(unitSeq), function(i){
    window = unitSeq[chain == chain[i] & unitSeq > unitSeq[i] & unitSeq <= unitSeq[i] + windowSize]
    length(window)
  })
}

#' @rdname trackPrevContext
#' @export
countNextMentionsIf = function(windowSize, cond, unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqFirst", chain = "chain")

  condition = eval_bare(enexpr(cond), env = env_parent(caller_env()))

  sapply(1:length(unitSeq), function(i){
    window = unitSeq[chain == chain[i] & unitSeq > unitSeq[i] & unitSeq <= unitSeq[i] + windowSize & condition]
    length(window)
  })

}

#' @rdname trackPrevContext
#' @export
countNextMentionsMatch = function(windowSize, field, unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqFirst", chain = "chain")

  sapply(1:length(unitSeq), function(i){
    window = unitSeq[chain == chain[i] & unitSeq > unitSeq[i] & unitSeq <= unitSeq[i] + windowSize & field == field[i]]
    length(window)
  })
}

#' @rdname trackPrevContext
#' @export
getNextMentionField = function(field, tokenOrder = NULL, chain = NULL){
  grabFromDF(tokenOrder = "docTokenSeqFirst", chain = "chain")
  prevMentionPos = lastMentionToken(tokenOrder = tokenOrder, chain = chain)
  sapply(1:length(field), function(i) field[which(tokenOrder == prevMentionPos[i])]) %>% zeroEntryToNA
}

#' Count the number of competing referents to the current mention
#'
#' This may either be counted within a window of units from the current one, or all referents competing with the current one may be counted, or a mix of both conditions. By default, we count referents intervening between the current and previous mention. Despite its name, tokenOrder can be set as unitSeqLast or similar.
#'
#' @param cond The condition under which something counts as a competitor. Leave blank if anything goes.
#' @inheritParams lastMentionToken
#' @return
#' @export
countCompetitors = function(cond = NULL, window = Inf, tokenOrder = NULL, chain = NULL, between = T){
  grabFromDF(tokenOrder = "docTokenSeqLast", chain = "chain")
  if(between) lastMentionPos = lastMentionToken(tokenOrder, chain)
  if(is.null(cond)){
    condition = T
  } else {
    condition = eval_bare(enexpr(cond), env = env_parent(caller_env()))
  }

  sapply(1:length(tokenOrder), function(x){
    if(between){
      result = sum(tokenOrder < tokenOrder[x] & tokenOrder > lastMentionPos[x] & condition & tokenOrder > tokenOrder[x] - window & chain[x] != chain)
    } else {
      result = sum(tokenOrder < tokenOrder[x] & condition & tokenOrder > tokenOrder[x] - window & chain[x] != chain)
    }
    #if(is.na(result)) result = 0
    result
  })
}

#' Count the number of competitors intervening between the previous mention and the current mention
#'
#' @param matchCol The column for which a value is to be matched.
#' @inheritParams lastMentionToken
#' @return
#' @export
countMatchingCompetitors = function(matchCol, window = Inf, tokenOrder = NULL, chain = NULL){
  grabFromDF(tokenOrder = "docTokenSeqLast", chain = "chain")
  lastMentionPos = lastMentionToken(tokenOrder, chain)
  if(is.null(cond)){
    condition = T
  } else {
    condition = eval_bare(enexpr(cond), env = env_parent(caller_env()))
  }

  sapply(1:length(tokenOrder), function(x){
    sum(tokenOrder < tokenOrder[x] & tokenOrder > lastMentionPos[x] & matchCol[x] == matchCol & tokenOrder > tokenOrder[x] - window)
  })

}


getLastMentionProp = function(column, chain = NULL, tokenOrder = NULL, inclRelations = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain", tokenOrder = "docTokenSeqLast")

  result = numeric(length(tokenOrder))

  for(currChain in unique(chain)){
    currTokens = tokenOrder[chain == currChain] #Grab all the token positions of the PREs in the chain
    currColumn = column[chain == currChain]
    chainTokenSort = sort(currTokens, index.return = T)
    columnOrdered = column[chainTokenSort$ix]
    columnOrderedLagged = lag(columnOrdered) #Get the previous val
    result[chain == currChain] = columnOrderedLagged[rank(currTokens)] #Put it back in the wrong order
  }
  result

  result
}

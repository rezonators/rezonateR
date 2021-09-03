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
#' @param tokenSeq The vector of sequence values values where the mentions appeared. Common choices are discourseTokenSeqFirst, discourseTokenSeqLast, wordTokenSeqFirst and wordTokenseqLast (the last two are available after running [rezonateR::addIsWordField] on a rezrObj. By default it's discourseTokenSeqLast.
#' @export
lastMentionToken = function(tokenSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(tokenSeq = "tokenSeqLast", chain = "chain")

  result = numeric(length(tokenSeq))
  for(currChain in unique(chain)){
    chainTokens = tokenSeq[chain == currChain] #Grab all the PRE's units
    tokensOrdered = sort(chainTokens) #Put them in the right order
    tokensOrderedLagged = lag(tokensOrdered) #Get the previous token
    result[chain == currChain] = tokensOrderedLagged[rank(chainTokens)] #Put it back in the wrong order
  }
  result
}

#' @rdname trackPrevContext
#' @param zeroProtocol If 'literal', I will take the seq values of the zeroes at face value. (If you set zeros as non-words and use discourseWordSeqFirst or discourseWordSeLast as your tokenSeq, this will lead to meaningless values.) If 'unitFinal', I will treat zeroes as if they were the final word of the unit. If 'unitFirst', I will treat zeroes as if they were the first word of the unit.
#' @param zeroCond A condition for determining whether a token is zero. For most people, this should be (word column) == "<0>".
#' @export
tokensToLastMention = function(tokenSeq = NULL, chain = NULL, zeroProtocol = "literal", zeroCond = NULL, unitSeq = NULL, unitDF = NULL){
  #Get the default column names from the rezrDF environment if unspecified
  grabFromDF(tokenSeq = "discourseTokenSeqLast", chain = "chain")

  if(zeroProtocol == "literal"){
    result = tokenSeq - lastMentionToken(tokenSeq, chain)
  } else if(zeroProtocol == "unitFinal"){
    #Validation
    stopifnot(!is.null(zeroCond))
    grabFromDF(unitSeq = "unitSeqLast")
    stopifnot(!is.null(unitSeq))
    stopifnot(!is.null(unitDF))

    prevUnit = lastMentionUnit(unitSeq, chain) #Prev units
    prevToken = sapply(prevUnit, function(x){
      if(!is.na(x)){
        unitDF %>% filter(unitSeq == x) %>% slice(1) %>% pull(discourseTokenSeqLast)
      } else NA
    })
    result = sapply(tokenSeq - prevToken, function(x) max(x, 0))
  } else if(zeroProtocol == "unitInitial"){
    #Validation
    stopifnot(!is.null(zeroCond))
    grabFromDF(unitSeq = "unitSeqFirst")
    stopifnot(!is.null(unitSeq))
    stopifnot(!is.null(unitDF))

    prevUnit = lastMentionUnit(unitSeq, chain) #Prev units
    prevToken = sapply(prevUnit, function(x){
      if(!is.na(x)){
        unitDF %>% filter(unitSeq == x) %>% slice(1) %>% pull(discourseTokenSeqFirst)
      } else NA
    })
    result = sapply(tokenSeq - prevToken, function(x) max(x, 0))

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
getPrevMentionField = function(field, tokenSeq = NULL, chain = NULL){
  grabFromDF(tokenSeq = "discourseTokenSeqLast", chain = "chain")
  lastMentionPos = lastMentionToken(tokenSeq = tokenSeq, chain = chain)
  sapply(1:length(field), function(i) field[which(tokenSeq == lastMentionPos[i])]) %>% zeroEntryToNA
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
#' @param unitSeq The vector of tokenSeq values where the mentions appeared. You can choose tokenSeqFirst, tokenSeqFirst, or maybe an average of the two. By default it's tokenSeqFirst.
#' @export
nextMentionToken = function(tokenSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(tokenSeq = "tokenSeqFirst", chain = "chain")

  result = numeric(length(tokenSeq))
  for(currChain in unique(chain)){
    chainTokens = tokenSeq[chain == currChain] #Grab all the PRE's units
    tokensOrdered = sort(chainTokens) #Put them in the right order
    tokensOrderedLagged = lag(tokensOrdered) #Get the nextious unit
    result[chain == currChain] = tokensOrderedLagged[rank(chainTokens)] #Put it back in the wrong order
  }
  result
}

#' @rdname trackPrevContext
#' @export
tokensToNextMention = function(tokenSeq = NULL, chain = NULL, zeroProtocol = "literal", zeroCond = NULL, unitSeq = NULL, unitDF = NULL){
  #Get the default column names from the rezrDF environment if unspecified
  grabFromDF(tokenSeq = "discourseTokenSeqFirst", chain = "chain")

  if(zeroProtocol == "literal"){
    result = nextMentionToken(tokenSeq, chain) - tokenSeq
  } else if(zeroProtocol == "unitFinal"){
    #Validation
    stopifnot(!is.null(zeroCond))
    grabFromDF(unitSeq = "unitSeqLast")
    stopifnot(!is.null(unitSeq))
    stopifnot(!is.null(unitDF))

    nextUnit = nextMentionUnit(unitSeq, chain) #Next units
    nextToken = sapply(nextUnit, function(x){
      if(!is.na(x)){
        unitDF %>% filter(unitSeq == x) %>% slice(1) %>% pull(discourseTokenSeqLast)
      } else NA
    })
    result = nextToken - tokenSeq

  } else if(zeroProtocol == "unitInitial"){
    #Validation
    stopifnot(!is.null(zeroCond))
    grabFromDF(unitSeq = "unitSeqFirst")
    stopifnot(!is.null(unitSeq))
    stopifnot(!is.null(unitDF))

    nextUnit = nextMentionUnit(unitSeq, chain) #Next units
    nextToken = sapply(nextUnit, function(x){
      if(!is.na(x)){
        unitDF %>% filter(unitSeq == x) %>% slice(1) %>% pull(discourseTokenSeqFirst)
      } else NA
    })
    result = nextToken - tokenSeq

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
getNextMentionField = function(field, tokenSeq = NULL, chain = NULL){
  grabFromDF(tokenSeq = "discourseTokenSeqFirst", chain = "chain")
  prevMentionPos = lastMentionToken(tokenSeq = tokenSeq, chain = chain)
  sapply(1:length(field), function(i) field[which(tokenSeq == prevMentionPos[i])]) %>% zeroEntryToNA
}

#' Count the number of competitors intervening between the previous mention and the current mention
#'
#' @param cond The condition under which something counts as a competitor. Leave blank if anything goes.
#' @inheritParams lastMentionToken
#' @return
#' @export
countCompetitors = function(cond = NULL, window = Inf, tokenSeq = NULL, chain = NULL){
  grabFromDF(tokenSeq = "discourseTokenSeqLast", chain = "chain")
  lastMentionPos = lastMentionToken(tokenSeq, chain)
  if(is.null(cond)){
    condition = T
  } else {
    condition = eval_bare(enexpr(cond), env = env_parent(caller_env()))
  }

  sapply(1:length(tokenSeq), function(x){
    sum(tokenSeq < tokenSeq[x] & tokenSeq > lastMentionPos[x] & condition & tokenSeq > tokenSeq[x] - window)
  })

}

#' Count the number of competitors intervening between the previous mention and the current mention
#'
#' @param matchCol The column for which a value is to be matched.
#' @inheritParams lastMentionToken
#' @return
#' @export
countMatchingCompetitors = function(matchCol, window = Inf, tokenSeq = NULL, chain = NULL){
  grabFromDF(tokenSeq = "discourseTokenSeqLast", chain = "chain")
  lastMentionPos = lastMentionToken(tokenSeq, chain)
  if(is.null(cond)){
    condition = T
  } else {
    condition = eval_bare(enexpr(cond), env = env_parent(caller_env()))
  }

  sapply(1:length(tokenSeq), function(x){
    sum(tokenSeq < tokenSeq[x] & tokenSeq > lastMentionPos[x] & matchCol[x] == matchCol & tokenSeq > tokenSeq[x] - window)
  })

}


getLastMentionProp = function(column, chain = NULL, tokenSeq = NULL, inclRelations = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain", tokenSeq = "discourseTokenSeqLast")

  result = numeric(length(tokenSeq))

  for(currChain in unique(chain)){
    currTokens = tokenSeq[chain == currChain] #Grab all the token positions of the PREs in the chain
    currColumn = column[chain == currChain]
    chainTokenSort = sort(currTokens, index.return = T)
    columnOrdered = column[chainTokenSort$ix]
    columnOrderedLagged = lag(columnOrdered) #Get the previous val
    result[chain == currChain] = columnOrderedLagged[rank(currTokens)] #Put it back in the wrong order
  }
  result

  result
}

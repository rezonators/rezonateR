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

    prevUnit = lastMentionUnit(unitSeq, chain) #Prev units
    prevToken = sapply(prevUnit, function(x){
      if(!is.na(x)){
        (unitDF %>% filter(unitSeq == x) %>% pull(discourseTokenSeqLast))[1]
      } else NA
    })
    result = tokenSeq - prevToken

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

#' @rdname trackPrevContext
#' @param cond For if functions, the condition that the previous / next mention must satisfy. It cannot refer to the current mention.
#' @export
noPrevMentionsIf = function(windowSize, cond, unitSeq = NULL, chain = NULL){
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
noPrevMentionsMatch = function(windowSize, field, unitSeq = NULL, chain = NULL){
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
  } else if(zeroProtocol == "unitFirst"){
    #Validation
    stopifnot(!is.null(zeroCond))
    grabFromDF(unitSeq = "unitSeqFirst")
    stopifnot(!is.null(unitSeq))
    stopifnot(!is.null(unitDF))

    nextUnit = nextMentionUnit(unitSeq, chain) #Next units
    nextToken = sapply(nextUnit, function(x){
      if(!is.na(x)){
        (unitDF %>% filter(unitSeq == x) %>% pull(discourseTokenSeqFirst))[1]
      } else NA
    })
    result = nextToken - tokenSeq

  }
  result
}

#' @rdname trackPrevContext
#' @export
noNextMentions = function(windowSize, unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqFirst", chain = "chain")

  sapply(1:length(unitSeq), function(i){
    window = unitSeq[chain == chain[i] & unitSeq > unitSeq[i] & unitSeq <= unitSeq[i] + windowSize]
    length(window)
  })
}

#' @rdname trackPrevContext
#' @export
noNextMentionsIf = function(windowSize, cond, unitSeq = NULL, chain = NULL){
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
noNextMentionsMatch = function(windowSize, field, unitSeq = NULL, chain = NULL){
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
noCompetitors = function(cond = NULL, tokenSeq = NULL, chain = NULL){
  grabFromDF(tokenSeq = "discourseTokenSeqLast", chain = "chain")
  lastMentionPos = lastMentionToken(tokenSeq, chain)
  if(is.null(cond)){
    condition = T
  } else {
    condition = eval_bare(enexpr(cond), env = env_parent(caller_env()))
  }

  sapply(1:length(tokenSeq), function(x){
    sum(tokenSeq < tokenSeq[x] & tokenSeq > lastMentionPos[x] & condition)
  })

}

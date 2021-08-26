#A group of functions for dealing with bridging and frame relations.
#1) Important, purely internal function: If a certain argument isn't specified, grab it from the DF with the default column name
#2) Functions related to previous context:

addFrameMatrix = function(rezrObj){
  attr(rezrObj$trackChainDF, "frameMatrix") = createFrameMatrix(rezrObj)
  rezrObj
}

createFrameMatrix = function(rezrObj){
  trackChainDF = combineLayers(rezrObj, "trackChain")
  ids = trackChainDF %>% pull(id)
  names = trackChainDF %>% pull(name)
  result = as_tibble(matrix("", nrow = nrow(trackChainDF), ncol = nrow(trackChainDF) + 2))
  result[,1] = ids
  result[,2] = names
  names(result) = c("id", "name", names)
  new_frameMatrix(result)
}

new_frameMatrix = function(m){
  stopifnot("tbl_df" %in% class(m))
  stopifnot(nrow(m) == ncol(m) - 2)
  stopifnot(m[,2] == colnames(m)[-c(1,2)])

  structure(m, class = c("frameMatrix", "tbl_df", "tbl", "data.frame"))
}

frameMatrix = function(rezrObj){
  attr(rezrObj$trackChainDF, "frameMatrix")
}

`frameMatrix<-` = function(rezrObj, value){
  attr(rezrObj$trackChainDF, "frameMatrix") = value
  rezrObj
}


reducedFrameMatrix = function(rezrObj, cond){
  frameMatrix = frameMatrix(rezrObj)
  cond = enexpr(cond)
  trackChainDF = suppressWarnings(combineLayers(rezrObj, "trackChain", "union"))
  wantIDs = trackChainDF %>% filter(!!cond) %>% arrange(.data$chainSeq) %>% pull(.data$id)
  keepIDs = which(frameMatrix$id %in% wantIDs)
  frameMatrix[keepIDs, c(1, 2, keepIDs + 2)]
}

obscureUpper = function(frameMatrix){
  for(i in 3:ncol(frameMatrix)){
    for(j in 1:nrow(frameMatrix)){
      if(j <= i - 2) frameMatrix[j, i] = "/"
    }
  }
  frameMatrix
}

updateFrameMatrixFromDF = function(frameMatrix, changeDF){
  changeDF = changeDF %>% mutate(
    across(all_of(colnames(changeDF) %>% setdiff(c("id", "name"))),
           function(x) case_when(x == "/" | is.na(x) ~ "", T ~ x)
          )
    )
  fmCorr = sapply(changeDF$id, function(x) which(frameMatrix$id == x)[1])

  for(i in 1:nrow(changeDF)){
    for(j in 1:nrow(changeDF)){
        frameMatrix[fmCorr[i], fmCorr[j]+ 2] = changeDF[i , j + 2]
        if(changeDF[i , j + 2] != ""){
          frameMatrix[fmCorr[j], fmCorr[i]+ 2] = exchangeRelation(changeDF[i , j + 2] %>% unlist())
        }
    }
  }

  frameMatrix
}

exchangeRelation = function(string){
  split = strsplit(string, "-")[[1]]
  split[2] %+% "-" %+% split[1]
}




#TODO
reload.frameMatrix = function(frameMatrix){
  frameMatrix
}

getRelatedEntities = function(currID, frameMatrix, inclRelations = NULL){
  if(is.null(inclRelations)){
    entityPos = which(frameMatrix %>% filter(id == currID) %>% select(-id, -name) %>% unlist != "")
  } else {
    entityPos = which(frameMatrix %>% filter(id == currID) %>% select(-id, -name) %>% unlist %in% inclRelations)
  }
  frameMatrix$id[entityPos]
}

isOverlap = function(currBegin, currEnd, begin, end){
  begin >= currBegin & end <= currEnd
}

findPrev = function(curr, x){
  result = suppressWarnings(max(x[x < curr], na.rm = T))
  if(result == -Inf) result = NA
  result
}

#' Functions related to bridging and frames.
#'
#' See also [rezonateR::countCompetitors], [rezonateR::trackPrevContext].
#'
#' @rdname bridging
#' @param unitSeq The vector of units where the mentions appeared.
#' @param chain The chain that each mention belongs to.
#' @export
lastBridgeUnit = function(frameMatrix, unitSeq = NULL, chain = NULL, tokenSeqFirst = NULL, tokenSeqLast = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain", tokenSeqFirst = "discourseTokenSeqFirst", tokenSeqLast = "discourseTokenSeqLast")

  result = numeric(length(unitSeq))

  #In units before
  for(currChain in unique(chain)){
    currUnits = unitSeq[chain == currChain]
    relatedEntities = getRelatedEntities(currChain, frameMatrix)
    bridgeUnits = unitSeq[chain %in% relatedEntities] #Grab all the PRE's units
    result[chain == currChain] = sapply(currUnits, function(x) findPrev(x, bridgeUnits))

    bridgeTokenLast = tokenSeqLast[chain %in% relatedEntities]
    currTokenFirst = tokenSeqFirst[chain == currChain]
    hasSameIUBridge = sapply(1:length(currUnits), function(x){
      any((currTokenFirst[x] > bridgeTokenLast) & (currUnits[x] == bridgeUnits))
    })

    result[chain == currChain] = case_when(hasSameIUBridge ~ currUnits, T ~ result[chain == currChain])
  }

  result
}

#' @rdname bridging
#' @param unitSeq The vector of units where the mentions appeared.
#' @export
unitsToLastBridge = function(frameMatrix, unitSeq = NULL, chain = NULL, tokenSeqLast = NULL, tokenSeqFirst = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain", tokenSeqFirst = "discourseTokenSeqFirst", tokenSeqLast = "discourseTokenSeqLast")

  unitSeq - lastBridgeUnit(frameMatrix, unitSeq, chain, tokenSeqFirst, tokenSeqLast)
}

#' @rdname bridging
#' @param tokenSeq The vector of sequence values values where the mentions appeared. Common choices are discourseTokenSeqFirst, discourseTokenSeqLast, wordTokenSeqFirst and wordTokenseqLast (the last two are available after running [rezonateR::addIsWordField] on a rezrObj. By default it's discourseTokenSeqLast.
#' @export
lastBridgeToken = function(frameMatrix, firstOrLast = "last", tokenSeqFirst = NULL, tokenSeqLast = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(tokenSeqFirst = "discourseTokenSeqFirst", tokenSeqLast = "discourseTokenSeqLast", chain = "chain")
  if(firstOrLast == "last") tokenSeq = tokenSeqLast else tokenSeq = tokenSeqFirst

  result = numeric(length(tokenSeqFirst))
  for(currChain in unique(chain)){
    currTokens = tokenSeq[chain == currChain]
    bridgeBool = chain %in% getRelatedEntities(currChain, frameMatrix)
    currBegins = tokenSeqFirst[chain == currChain]
    currEnds = tokenSeqLast[chain == currChain]
    result[chain == currChain] = sapply(1:length(currTokens), function(x){
      overlapBool = isOverlap(currBegins[x], currEnds[x], tokenSeqFirst, tokenSeqLast)
      candEntities = tokenSeq[bridgeBool & !overlapBool]
      findPrev(currTokens[x], candEntities)
    })
  }
  result
}

#' @rdname bridging
#' @param zeroProtocol If 'literal', I will take the seq values of the zeroes at face value. (If you set zeros as non-words and use discourseWordSeqFirst or discourseWordSeLast as your tokenSeq, this will lead to meaningless values.) If 'unitFinal', I will treat zeroes as if they were the final word of the unit. If 'unitFirst', I will treat zeroes as if they were the first word of the unit.
#' @param zeroCond A condition for determining whether a token is zero. For most people, this should be (word column) == "<0>".
#' @export
tokensToLastBridge = function(frameMatrix, firstOrLast = "last", tokenSeqFirst = NULL, tokenSeqLast = NULL, chain = NULL, zeroProtocol = "literal", zeroCond = NULL, unitSeq = NULL, unitDF = NULL){
  #Get the default column names from the rezrDF environment if unspecified
  grabFromDF(tokenSeqFirst = "discourseTokenSeqFirst", tokenSeqLast = "discourseTokenSeqLast", chain = "chain")
  if(firstOrLast == "last") tokenSeq = tokenSeqLast else tokenSeq = tokenSeqFirst

  if(zeroProtocol == "literal"){
    result = tokenSeq - lastBridgeToken(frameMatrix, firstOrLast, tokenSeqFirst, tokenSeqLast, chain)
  } else if(zeroProtocol == "unitFinal"){
    #Validation
    stopifnot(!is.null(zeroCond))
    grabFromDF(unitSeq = "unitSeqLast")
    stopifnot(!is.null(unitSeq))
    stopifnot(!is.null(unitDF))

    prevUnit = lastBridgeUnit(frameMatrix, unitSeq, chain) #Prev units
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

    prevUnit = lastBridgeUnit(frameMatrix, unitSeq, chain) #Prev units
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
countPrevBridges = function(windowSize, frameMatrix, unitSeq = NULL, chain = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain")
  sapply(1:length(unitSeq), function(i){
    relatedEntities = getRelatedEntities(chain[i], frameMatrix)
    window = unitSeq[sapply(chain, function(x) x %in% relatedEntities)
                     & unitSeq < unitSeq[i]
                     & unitSeq >= unitSeq[i] - windowSize]
    length(window)
  })
}

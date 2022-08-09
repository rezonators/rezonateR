#A group of functions for dealing with bridging and frame relations.
#1) Important, purely internal function: If a certain argument isn't specified, grab it from the DF with the default column name
#2) Functions related to previous context:

#' Add a frame matrix to `trailDF`.
#'
#' @param rezrObj The desired `rezrObj`
#'
#' @return A `rezrObj` object, but its `trailDF` now has an attribute which is a `frameMatrix` object.
#' @export
#'
#' @examples
addFrameMatrix = function(rezrObj){
  attr(rezrObj$trailDF, "frameMatrix") = createFrameMatrix(rezrObj)
  rezrObj
}

createFrameMatrix = function(rezrObj){
  trailDF = combineLayers(rezrObj, "trail")
  ids = trailDF %>% pull(id)
  names = trailDF %>% pull(name)
  result = as_tibble(matrix("", nrow = nrow(trailDF), ncol = nrow(trailDF) + 2))
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

#' Extract/set a `frameMatrix`.
#'
#' @rdname getSetFrame
#' @param rezrObj
#'
#' @return The `frameMatrix` of the `rezrObj`.
#' @export
#'
#' @examples
frameMatrix = function(rezrObj){
  attr(rezrObj$trailDF, "frameMatrix")
}

#' @rdname getSetFrame
#' @param value The new `frameMatrix`.
#' @export
`frameMatrix<-` = function(rezrObj, value){
  attr(rezrObj$trailDF, "frameMatrix") = value
  rezrObj
}


#' Extract a frameMatrix, removing rows and columns that do not participate in frame semantics.
#'
#' @param rezrObj The `rezrObj` from which the `frameMatrix` is extracted.
#' @param cond Conditions for reduction.
#'
#' @return
#' @export
#'
#' @examples
reducedFrameMatrix = function(rezrObj, cond){
  frameMatrix = frameMatrix(rezrObj)
  cond = enexpr(cond)
  trailDF = suppressWarnings(combineLayers(rezrObj, "trail", "union"))
  wantIDs = trailDF %>% filter(!!cond) %>% arrange(.data$chainCreateSeq) %>% pull(.data$id)
  keepIDs = which(frameMatrix$id %in% wantIDs)
  frameMatrix[keepIDs, c(1, 2, keepIDs + 2)]
}

#' Obscure the upper triangular portion of a frameMatrix.
#'
#' @param frameMatrix
#'
#' @return A frameMatrix with the diagonal and all entries above it obscured.
#' @export
#'
#' @examples
obscureUpper = function(frameMatrix){
  for(i in 3:ncol(frameMatrix)){
    for(j in 1:nrow(frameMatrix)){
      if(j <= i - 2) frameMatrix[j, i] = "/"
    }
  }
  frameMatrix
}

#' Update `frameMatrix` from an external source.
#'
#' @param frameMatrix The `frameMatrix` object to be changed.
#' @param changeDF The data frame containing new values.
#'
#' @return An updated frameMatrix. The lower triangular matrix of `frameMatrix` will be read,
#' and the values will be flipped (e.g. 'group-inidividual' becomes 'individual-group')
#' and subsequently added to the upper triangular matrix.
#' @export
#'
#' @examples
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
lastBridgeUnit = function(frameMatrix, unitSeq = NULL, chain = NULL, tokenOrderFirst = NULL, tokenOrderLast = NULL, inclRelations = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain", tokenOrderFirst = "docTokenSeqFirst", tokenOrderLast = "docTokenSeqLast")

  result = numeric(length(unitSeq))

  #In units before
  for(currChain in unique(chain)){
    currUnits = unitSeq[chain == currChain]
    relatedEntities = getRelatedEntities(currChain, frameMatrix, inclRelations)
    bridgeUnits = unitSeq[chain %in% relatedEntities] #Grab all the PRE's units
    result[chain == currChain] = sapply(currUnits, function(x) findPrev(x, bridgeUnits))

    bridgeTokenLast = tokenOrderLast[chain %in% relatedEntities]
    currTokenFirst = tokenOrderFirst[chain == currChain]
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
unitsToLastBridge = function(frameMatrix, unitSeq = NULL, chain = NULL, tokenOrderLast = NULL, tokenOrderFirst = NULL, inclRelations = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain", tokenOrderFirst = "docTokenSeqFirst", tokenOrderLast = "docTokenSeqLast")

  unitSeq - lastBridgeUnit(frameMatrix, unitSeq, chain, tokenOrderFirst, tokenOrderLast, inclRelations)
}

#' @rdname bridging
#' @param tokenOrder The vector of sequence values values where the mentions appeared. Common choices are docTokenSeqFirst, docTokenSeqLast, wordTokenSeqFirst and wordTokenseqLast (the last two are available after running [rezonateR::addIsWordField] on a rezrObj. By default it's docTokenSeqLast.
#' @export
lastBridgeToken = function(frameMatrix, firstOrLast = "last", tokenOrderFirst = NULL, tokenOrderLast = NULL, chain = NULL, inclRelations = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(tokenOrderFirst = "docTokenSeqFirst", tokenOrderLast = "docTokenSeqLast", chain = "chain")
  if(firstOrLast == "last") tokenOrder = tokenOrderLast else tokenOrder = tokenOrderFirst

  result = numeric(length(tokenOrderFirst))
  for(currChain in unique(chain)){
    currTokens = tokenOrder[chain == currChain]
    bridgeBool = chain %in% getRelatedEntities(currChain, frameMatrix, inclRelations)
    currBegins = tokenOrderFirst[chain == currChain]
    currEnds = tokenOrderLast[chain == currChain]
    result[chain == currChain] = sapply(1:length(currTokens), function(x){
      overlapBool = isOverlap(currBegins[x], currEnds[x], tokenOrderFirst, tokenOrderLast)
      candEntities = tokenOrder[bridgeBool & !overlapBool]
      findPrev(currTokens[x], candEntities)
    })
  }
  result
}

#' @rdname bridging
#' @param zeroProtocol If 'literal', I will take the seq values of the zeroes at face value. (If you set zeros as non-words and use docWordSeqFirst or discourseWordSeLast as your tokenOrder, this will lead to meaningless values.) If 'unitFinal', I will treat zeroes as if they were the final word of the unit. If 'unitFirst', I will treat zeroes as if they were the first word of the unit.
#' @param zeroCond A condition for determining whether a token is zero. For most people, this should be (word column) == "<0>".
#' @export
tokensToLastBridge = function(frameMatrix, firstOrLast = "last", tokenOrderFirst = NULL, tokenOrderLast = NULL, chain = NULL, zeroProtocol = "literal", zeroCond = NULL, unitSeq = NULL, unitDF = NULL, inclRelations = NULL){
  #Get the default column names from the rezrDF environment if unspecified
  grabFromDF(tokenOrderFirst = "docTokenSeqFirst", tokenOrderLast = "docTokenSeqLast", chain = "chain")
  if(firstOrLast == "last") tokenOrder = tokenOrderLast else tokenOrder = tokenOrderFirst

  if(zeroProtocol == "literal"){
    result = tokenOrder - lastBridgeToken(frameMatrix, firstOrLast, tokenOrderFirst, tokenOrderLast, chain, inclRelations)
  } else if(zeroProtocol == "unitFinal"){
    #Validation
    stopifnot(!is.null(zeroCond))
    grabFromDF(unitSeq = "unitSeqLast")
    stopifnot(!is.null(unitSeq))
    stopifnot(!is.null(unitDF))

    prevUnit = lastBridgeUnit(frameMatrix, unitSeq, chain, inclRelations) #Prev units
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

    prevUnit = lastBridgeUnit(frameMatrix, unitSeq, chain, inclRelations) #Prev units
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
countPrevBridges = function(windowSize, frameMatrix, unitSeq = NULL, chain = NULL, inclRelations = NULL){
  #Get the default column names from the rezrDF environment
  grabFromDF(unitSeq = "unitSeqLast", chain = "chain")
  sapply(1:length(unitSeq), function(i){
    relatedEntities = getRelatedEntities(chain[i], frameMatrix, inclRelations)
    window = unitSeq[sapply(chain, function(x) x %in% relatedEntities)
                     & unitSeq < unitSeq[i]
                     & unitSeq >= unitSeq[i] - windowSize]
    length(window)
  })
}

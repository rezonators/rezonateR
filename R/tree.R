
getTreeOfEntry = function(treeEntryDF, treeLinkDF, treeNodeMap){
  parents = character(nrow(treeEntryDF))
  trees = sapply(treeEntryDF$id, function(x){
    tree = NA
    for(treeName in names(treeNodeMap)){
      if(x %in% treeNodeMap[[treeName]]$setIDList){
        tree = treeName
        break
      }
    }
    if(is.na(tree)) warning("One of the tree entries, " %+% x %+% " , has no tree. This may be a Rezonator bug.")
    tree
  })
  treeEntryDF = treeEntryDF %>% rez_mutate(tree = trees, fieldaccess = "core") %>% relocate(tree, .after = doc)
  treeEntryDF
}

getTreeEntryForChunk = function(chunkDF, chunkNodeMap, treeEntryDF, treeEntryNodeMap){
  exacts = character(nrow(chunkDF))
  for(x in 1:nrow(chunkDF)){
    currDTSF = chunkDF[x,] %>% pull(discourseTokenSeqFirst)
    currDTSL = chunkDF[x,] %>% pull(discourseTokenSeqLast)
    currDoc = chunkDF[x,] %>% pull(doc)
    currID = chunkDF[x,] %>% pull(id)
    candRows = treeEntryDF %>% filter(doc == currDoc, discourseTokenSeqFirst == currDTSF, discourseTokenSeqLast >= currDTSL, level != -1)
    candIDs = candRows$id

    candsExact = character(0)
    for(cand in candIDs){
      if(setequal(chunkNodeMap[[currID]]$tokenList, treeEntryNodeMap[[cand]]$tokenList)){
        candsExact = c(candsExact, cand)
      }
    }

    if(length(candsExact) > 0){
      exacts[x] = candsExact[1]
      if(length(candsExact) > 1){
        print("There is more than one tree entry match for the chunk " %+% (chunkDF[x, ] %>% pull(id)) %+% ". I will take the first match.")
      }
    }

    #TODO: Non-exacts
  }

  chunkDF = chunkDF %>% rez_mutate(treeEntry = exacts)
  chunkDF

}

getTreeEntryForToken = function(tokenDF, tokenNodeMap, treeEntryDF, treeEntryNodeMap){
  exacts = character(nrow(tokenDF))
  for(x in 1:nrow(tokenDF)){
    currDTS = tokenDF[x,] %>% pull(discourseTokenSeq)
    currDoc = tokenDF[x,] %>% pull(doc)
    currID = tokenDF[x,] %>% pull(id)
    candIDs = treeEntryDF %>% filter(doc == currDoc, discourseTokenSeqFirst == currDTS, discourseTokenSeqLast >= currDTS, level != -1) %>% pull(id)

    if(length(candIDs) > 0){
        isExact = sapply(candIDs, function(cand) all(treeEntryNodeMap[[cand]]$tokenList == currID))
        candsExact = candIDs[isExact]

        if(length(candsExact) > 0){
          exacts[x] = candsExact[1]
          if(length(candsExact) > 1){
            print("There is more than one tree entry match for the token " %+% (tokenDF[x, ] %>% pull(id)) %+%". I will take the first match.")
          }
        }
    }
  }

  tokenDF = tokenDF %>% rez_mutate(treeEntry = exacts)
  tokenDF

}

#' Relate table rows to tree entries covering the same tokens.
#'
#' @inheritParams addUnitSeq
#'
#' @return The rezrObj with table rows added to tree entries.
#' @export
getAllTreeCorrespondences = function(rezrObj, entity = "chunk"){
  #TODO: Tree into layers
  if(entity == "chunk"){
    for(chunkLayer in names(rezrObj$chunkDF)){
      rezrObj$chunkDF[[chunkLayer]] = rezrObj$chunkDF[[chunkLayer]] %>% getTreeEntryForChunk(rezrObj$nodeMap$chunk, rezrObj$treeEntryDF, rezrObj$nodeMap$treeEntry)
    }
  } else if(entity == "token"){
    rezrObj$tokenDF = rezrObj$tokenDF %>% getTreeEntryForToken(rezrObj$nodeMap$token, rezrObj$treeEntryDF, rezrObj$nodeMap$treeEntry)
  } else if(entity == "track"){
    if(!("treeEntry" %in% rezrObj$tokenDF)) rezrObj = getAllTreeCorrespondences(rezrObj, entity = "token")
    if(!any(sapply(rezrObj$chunkEntry, function(x) "treeEntry" %in% x))) rezrObj = getAllTreeCorrespondences(rezrObj, entity = "chunk")

    joinDF = combineTokenChunk(rezrObj) %>% select(id, treeEntry)
    df2Address = c("chunk/" %+% names(rezrObj$chunkDF), "token")
    for(trackLayer in names(rezrObj$trackDF)){
      rezrObj$trackDF[[trackLayer]] = rezrObj$trackDF[[trackLayer]] %>% rez_left_join(joinDF, df2Address = df2Address, fkey = "token", by = c(token = "id"))
    }
  }
  rezrObj
}
#' Relate table rows to tree entries covering the same tokens.
#'
#' @inheritParams addUnitSeq
#' @param treeEntryDF A treeEntry data.frame, possibly filtered.
#' @param addToTrack Do you want to add the chunks to the trackDF as well?
#' @param selectCond The condition for selecting which chunk provides the field values.
#'
#' @return The rezrObj with additional rows for merged chunks. Original chunks stay behind.
#' @export
mergeChunksWithTree = function(rezrObj, treeEntryDF = NULL, addToTrack = F, selectCond = NULL){
  if(is.null(treeEntryDF)){
    #treeEntryDF = combineLayers(rezrObj, "treeEntry")
    treeEntryDF = rezrObj$treeEntryDF
  }
  tcDF = combineTokenChunk(rezrObj)
  chunkDF = combineChunks(rezrObj)
  allGoodEntries = treeEntryDF %>% filter(level != -1) %>% pull(id)
  missingEntries = setdiff(allGoodEntries, tcDF$treeEntry)

  #What number shall we start at?
  currNewChunkNames = chunkDF$name[str_detect(chunkDF$name, "New Chunk ")]
  currNewChunkMax = suppressWarnings(max(max(as.numeric(chompPrefix(currNewChunkNames, "New Chunk ")), na.rm = T), 0))
  i = currNewChunkMax + 1

  #Adding a combinedDF column to the chunkDFs
  for(chunkLayer in names(rezrObj$chunkDF)){
    if(!("combinedChunk" %in% rezrObj$chunkDF[[chunkLayer]])){
      rezrObj$chunkDF[[chunkLayer]] = rezrObj$chunkDF[[chunkLayer]] %>% rez_mutate(combinedChunk = "")
    }
  }

  for(entry in missingEntries){
    currRow = treeEntryDF %>% filter(id == entry)
    currDTSF = currRow %>% pull(discourseTokenSeqFirst)
    currDTSL = currRow %>% pull(discourseTokenSeqLast)
    currDoc = currRow %>% pull(doc)

    done = F
    chunksCombined = getNextChunks(chunkDF, currDoc, currDTSF, currDTSL)
    if(all(!is.na(chunksCombined))){
      #Which chunk's annotations represents the whole thing?
      infoSource = NULL
      selectCond = enexpr(selectCond)
      if(!is.null(selectCond)){
        infoSourceCands = chunkDF %>% filter(id %in% chunksCombined & selectCond)
        if(nrow(infoSourceCands) > 1) infoSource = infoSourceCands %>% slice(1)
      }
      if(is.null(infoSource)){
        infoSource = chunkDF %>% filter(id %in% chunksCombined) %>% arrange(tokenSeqFirst) %>% slice(1)
        newRow = infoSource
        newRow[fieldaccess(chunkDF) %in% c("foreign", "auto")] = NA
        newRow$name = "New Chunk " %+% i
        i = i + 1
        newRow$id = createRezId(1, getIDs(rezrObj))
        newRow = as.list(newRow)
        newRow$combinedChunk = "combined"
        args = newRow
        args[["entity"]] = "chunk"
        args[["layer"]] = newRow$layer
        args[["rezrObj"]] = rezrObj
        args = args[c(length(args), 1:(length(args) - 1))]
        args[["nodeMapArgs"]] = list()
        args$nodeMapArgs[["type"]] = list("chunk")
        args$nodeMapArgs[["tokenList"]] = list(lapply(rezrObj$nodeMap$chunk[chunksCombined], function(x) x$tokenList) %>% unlist)
        #args[["x"]] = rezrObj
        rezrObj = exec("addRow", !!!args)
        rezrObj$chunkDF[[newRow$layer]] = rezrObj$chunkDF[[newRow$layer]] %>% mutate(combinedChunk = case_when(id %in% chunksCombined ~ combinedChunk %+% "|member-" %+% entry, T ~ combinedChunk))
      }
    }
  }
  rezrObj
}

getNextChunks = function(chunkDF, currDoc, p, currDTSL, currVec = character(0)){
  candChunks = chunkDF %>% filter(doc == currDoc, discourseTokenSeqFirst == p, discourseTokenSeqLast <= currDTSL) %>% pull(id)
  if(length(candChunks) > 0){
    for(cand in candChunks){
      p = chunkDF %>% filter(id == cand) %>% pull(discourseTokenSeqLast) + 1
      if(p - 1 == currDTSL){
        result = c(currVec, cand)
        #print("Victory")
        #print(result)
      } else if(p - 1 < currDTSL){
        result = getNextChunks(chunkDF, currDoc, p, currDTSL, c(currVec, cand))
        if(all(!is.na(result))) break
        #print(c(currVec, cand))
      } else {
        result = NA
      }
    }
  } else {
    result = NA
  }
  result
}

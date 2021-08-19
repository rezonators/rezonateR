
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

#Some chunk-related functions

#' Find chunks containing other chunks
#'
#' @rdname containChunk
#' @param containedDF The rezrDF with contained chunks (may be a track/rez rezrDF)
#' @param containerDF The rezrDF with container chunks (may be a track/rez rezrDF)
#' @param proper Do you count only proper containment?
#'
#' @return A vector of container chunks, each of which corresponds to one row in the containedDF (the first one is chosen in the case of multiple parents). Right now, we do not support criteria for choosing container chunk.
#' @export
#'
#' @examples sbc007$chunkDF$verb = sbc007$chunkDF$verb %>% rez_mutate(containingRefexpr = findContainingChunk(sbc007$chunkDF$verb, sbc007$chunkDF$refexpr, proper = T))
findContainingChunk = function(containedDF, containerDF, proper = F){
  #Will have to be modified when I have multiple docs
  sapply(1:nrow(containedDF), function(x){
    result = containerDF %>%
      filter(containerDF$docTokenSeqFirst <= containedDF$docTokenSeqFirst[x], containerDF$docTokenSeqLast >= containedDF$docTokenSeqLast[x],
      !proper | (containerDF$docTokenSeqFirst != containedDF$docTokenSeqFirst[x] | containerDF$docTokenSeqLast != containedDF$docTokenSeqLast[x])) %>%
      arrange(docTokenSeqLast - docTokenSeqFirst, desc(docTokenSeqFirst), docTokenSeqLast) %>%
      slice(1) %>% pull(id)
    if(length(result) == 0) result = NA
    result
  })
}

#' @rdname containChunk
#' @param rezrObj The rezrObj for which you would like to add containing chunks.
#' @export
addContainingChunk = function(containedDF, rezrObj, containerDFAdd){
  #TODO: Multiple containerDFAdds

  containerDFAdd = eval(containerDFAdd)
  containerDF = listAt(rezrObj, containerDFAdd)
  containedDF = suppressWarnings(containedDF %>% rez_mutate(container = findContainingChunk(containedDF, containerDF), fieldaccess = "foreign"))
  updateFunct(containedDF, "container") = new_updateFunction(function(df, rezrObj) updateContainingChunk(df, rezrObj, containerDFAdd), containerDFAdd)
  containedDF
}

updateContainingChunk = function(containedDF, rezrObj, containerDFAdd){
  #TODO: Multiple containerDFAdds

  containerDFAdd = eval(containerDFAdd)
  containerDF = listAt(rezrObj, containerDFAdd)
  containedDF = suppressWarnings(containedDF %>% rez_mutate(container = findContainingChunk(containedDF, containerDF), fieldaccess = "foreign"))
  containedDF
}


#' Relate table rows to tree entries covering the same tokens.
#'
#' @inheritParams addUnitSeq
#' @param treeEntryDF A treeEntry data.frame, possibly filtered.
#' @param addToTrack Do you want to add the chunks to the trackDF as well?
#' @param selectCond The condition for selecting which chunk provides the field values.
#'
#' @return The rezrObj with additional rows for merged chunks. Original chunks stay behind. There will be a new column called combinedChunk. Combined chunks will get the value 'combined'. Members of those chunks will get the value '|infomember=COMBINEDCHUNKID' (if they are providing the data for the combined chunk) or '|member=COMBINEDCHUNKID' (if they are not the data-providing chunk). treeEntry (through getAllTreeCorrespondences()) is required to be present in the chunkDF.
#' @examples sbc007 = mergeChunksWithIDs(sbc007, "largerChunk", selectCond = NULL)
#' @export
mergeChunksWithIDs = function(rezrObj, idField, addToTrack = F, selectCond = NULL){
  tcDF = combineTokenChunk(rezrObj)
  chunkDF = combineChunks(rezrObj)
  newChunks = unique(chunkDF[[idField]][!is.na(chunkDF[[idField]])])

  #What number shall we start at? (If no new chunks have been added before, then we'll just start from 1)
  currNewChunkNames = chunkDF$name[str_detect(chunkDF$name, "New Chunk ")]
  currNewChunkMax = suppressWarnings(max(max(as.numeric(chompPrefix(currNewChunkNames, "New Chunk ")), na.rm = T), 0))
  i = currNewChunkMax + 1

  #Adding a combinedDF column to the chunkDFs
  for(chunkLayer in names(rezrObj$chunkDF)){
    if(!("combinedChunk" %in% names(rezrObj$chunkDF[[chunkLayer]]))){
      rezrObj$chunkDF[[chunkLayer]] = rezrObj$chunkDF[[chunkLayer]] %>% rez_mutate(combinedChunk = "")
    }
  }

  for(newChunk in newChunks){
    chunksCombined = chunkDF %>% filter(!!parse_expr(idField) == newChunk) %>% pull(id)
    rezrObj = mergeGivenChunks(rezrObj, chunkDF, chunksCombined, selectCond, i)
    i = i + 1
  }
  if(addToTrack){
    #todo: addToTrack
  }
  rezrObj
}

mergeGivenChunks = function(rezrObj, chunkDF, chunksCombined, selectCond = NULL, i){
  if(all(!is.na(chunksCombined))){
    #Which chunk's annotations represents the whole thing?
    infoSource = NULL

    if(!is.null(selectCond)){
      selectCond = enexpr(selectCond)
      infoSourceCands = chunkDF %>% filter(id %in% chunksCombined & !!selectCond)
      if(nrow(infoSourceCands) == 1){
        infoSource = infoSourceCands
      } else if(nrow(infoSourceCands) > 1){
        infoSource = infoSourceCands %>% slice(1)
      }
    }
    if(is.null(infoSource)){
      infoSource = chunkDF %>% filter(id %in% chunksCombined) %>% arrange(docTokenSeqFirst) %>% slice(1)
    }
    newRow = infoSource
    newRow[fieldaccess(chunkDF) %in% c("foreign", "auto")] = NA
    newRow$name = "New Chunk " %+% i
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
    rezrObj = suppressWarnings(exec("addRow", !!!args))
    rezrObj$chunkDF[[newRow$layer]] = rezrObj$chunkDF[[newRow$layer]] %>%
      mutate(combinedChunk = case_when(id == infoSource$id ~ combinedChunk %+% "|infomember-" %+% newRow$id,
                                       id %in% chunksCombined ~ combinedChunk %+% "|member-" %+% newRow$id,
                                       T ~ combinedChunk))
  }
  rezrObj
}

#Some chunk-related functions

#' Find chunks containing other chunks
#'
#' @param containedDF The rezrDF with contained chunks (may be a track/rez rezrDF)
#' @param containerDF The rezrDF with container chunks (may be a track/rez rezrDF)
#' @param proper Do you count only proper containment?
#'
#' @return A vector of container chunks, each of which corresponds to one row in the containedDF (the first one is chosen in the case of multiple parents). Right now, we do not support criteria for choosing container chunk.
#' @export
#'
#' @examples
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

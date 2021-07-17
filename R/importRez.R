#' Import a Rez file
#'
#' Import a Rez file. This returns an object containing, among other things, a nodeMap object containing raw information, and data frames for tokens, units, chunks, track chain entries, track chains, containing only key information.
#'
#' @param path A character vector of paths to the files to be imported. For Windows users, please use / instead of \.
#' @param docnames A character vector of the document names. If left blank, a docname will be generated according to the filenames of files you import. For example, the document foo/bar.rez will be named 'bar'.
#'
#' @return rezRObject
#' @import stringr
#' @export
importRez = function(paths, docnames = ""){
  if(length(paths) != length(docnames) & docnames != ""){
    docnames = ""
    warning("Number of input paths does not match the number of document names. I will name your documents automatically, according to your filenames.")
  }
  if(docnames == ""){
    #Detecting document names
    lastSlashLocs = str_locate_all(path, "/")
    lastRezLocs = str_locate_all(path, "\\.rez")
    docnames_start = sapply(1:length(paths), function(x) lastSlashLocs[[x]][nrow(lastSlashLocs[[x]]),1] + 1)
    docnames_end = sapply(1:length(paths), function(x) lastRezLocs[[x]][nrow(lastRezLocs[[x]]),1] - 1)
    docnames = sapply(1:length(paths), function(x) substr(paths[x], docnames_start, docnames_end))
  }

  for(x in 1:length(paths)){
    path = paths[x]

    #Importing .rez file
    rezJSON = rjson::fromJSON(file = path)
    nodeMap = nodeMap(rezJSON[["ROOT"]][[1]][["nodeMap"]])
  }

  returnObj = list(nodeMap = nodeMap)
  return(returnObj)
}

nodeMap = function(importNodeMap, docname){
  #As of Rezonator 16.0.0.0
  nameLists = list()
  nameListNames = names(importNodeMap)[str_detect(names(importNodeMap), "List")]
  for(nameListName in nameListNames){
    if(nameListName %in% names(importNodeMap)){
      nameLists[[nameListName]] = importNodeMap[[nameListName]]
      importNodeMap[[nameListName]] = NULL
    }
  }

  #As of Rezonator 16.0.0.0
  maps = list()
  mapNames = names(importNodeMap)[str_detect(names(importNodeMap), "Map")]
  for(mapName in mapNames){
    if(mapName %in% names(importNodeMap)){
      maps[[mapName]] = importNodeMap[[mapName]]
      importNodeMap[[mapName]] = NULL
    }
  }

  rawNodeMap = list()
  for(nodeName in names(importNodeMap)){
    node = importNodeMap[[nodeName]]
    if(is.null(node$type)){
      warning("Node skipped with no type specification.")
      print(node)
      next
    }
    if(length(node) > 1){
      if(length(rawNodeMap) == 0){
        rawNodeMap[[node$type]] = list()
      } else if (!(node$type %in% names(rawNodeMap))){
        rawNodeMap[[node$type]] = list()
      }
      rawNodeMap[[node$type]][[nodeName]] = node
    }
  }

  return(new_nodeMap(rawNodeMap, docname, nameLists, maps))
}

new_nodeMap = function(organisedNodeMap, docname, idLists, smallMaps){
  stopifnot(is.list(organisedNodeMap))
  stopifnot(is.character(docname))
  stopifnot(is.list(idLists))
  stopifnot(is.list(smallMaps))

  structure(organisedNodeMap, class = "nodeMap", docname = docname, idLists = idLists, smallMaps = smallMaps)
}

getChainInfo= function(chain){
  chainInfo = list(chainID = chain$chain)
  chainInfo = c(reInfo, chain$tagMap)
}

getREInfo= function(re){
  reInfo = list(chainID = re$chain)
  reInfo = c(reInfo, re$tagMap)
}

#An skeleton for when it's needed
getLinkInfo= function(re){
}


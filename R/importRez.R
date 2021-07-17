#' Import a Rez file
#'
#' Import a Rez file. This returns an object containing, among other things, a nodeMap object containing raw information, and data frames for tokens, units, chunks, track chain entries, track chains, containing only key information likely to be useful for the user.
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

  nodeMapSep = list()
  for(x in 1:length(paths)){
    path = paths[x]
    rezJSON = rjson::fromJSON(file = path) #Importing the file
    nodeMapSep[[x]] = nodeMap(rezJSON[["ROOT"]][[1]][["nodeMap"]], docnames[x]) #Getting an individual node map
  }

  if(length(paths) > 1){
    fullNodeMap = Reduce(mergeNodeMaps, nodeMapSep[2:length(nodeMapSep)], mergeNodeMaps[[1]])
  } else {
    fullNodeMap = nodeMapSep[[1]]
  }

  entryDF = nodeToDF(fullNodeMap[["entry"]], entryDFFields)
  unitDF = nodeToDF(fullNodeMap[["unit"]], unitDFFields)
  tokenDF = nodeToDF(fullNodeMap[["token"]], tokenDFFields)
  chunkDF = nodeToDF(fullNodeMap[["chunk"]], chunkDFFields)
  trackDF = nodeToDF(fullNodeMap[["track"]], trackDFFields)
  trackChainDF = nodeToDF(fullNodeMap[["trackChain"]], trackChainDFFields)
  linkDF = nodeToDF(fullNodeMap[["link"]], linkDFFields)
  docDF = nodeToDF(fullNodeMap[["doc"]], docDFFields)

  returnObj = list(nodeMap = fullNodeMap, entryDF = entryDF, unitDF = unitDF, tokenDF = tokenDF, chunkDF = chunkDF, trackDF = trackDF, trackChainDF = trackChainDF, linkDF = linkDF, docDF = docDF)
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
      rawNodeMap[[node$type]][[nodeName]][["doc"]] = docname
    }
  }

  return(new_nodeMap(rawNodeMap, nameLists, maps))
}

#Constructor function for nodeMap.
new_nodeMap = function(organisedNodeMap, idLists, smallMaps){
  stopifnot(is.list(organisedNodeMap))
  stopifnot(is.list(idLists))
  stopifnot(is.list(smallMaps))

  structure(organisedNodeMap, class = "nodeMap", idLists = idLists, smallMaps = smallMaps)
}

#Merge two node maps. Does not currently check for non-unique IDs.
#Not to be called directly. Merge two rezCorpus objects instead.
mergeNodeMaps = function(nm1, nm2){
  nm1_only = setminus(names(nm1), names(nm2))
  nm2_only = setminus(names(nm2), names(nm1))
  if(length(nm1_only) != 0 | length(nm2_only) != 0){
    warning(paste0("The following node types are not found in all documents: ", paste(c(nm1_only, nm2_only, ", ")), "."))
  }

  nodeTypes = union(names(nm1), names(nm2))
  for(nodeType in nodeTypes){
    if(!(nodeType %in% union(nm1_only, nm2_only))){
      nm_new = mergeNodeLists(nm1[[nodeType]], nm2[[nodeType]])
    }
  }
}

#Merge two node lists. Only to be called by mergeNodeMaps.
mergeNodeLists = function(nl1, nl2){
  nl1_only = setminus(nl1[[1]]$tagMap)
  nl2_only = setminus(nl2[[1]]$tagMap)
  if(length(nl1_only) != 0 | length(nl2_only) != 0){
    warning("The two documents do not have compatible field names. New tags will be created.")
  }
  for(field in nl1_only){
    nl2 = lapply(nl2, function(x) c(x, field = ""))
  }
  for(field in nl2_only){
    nl1 = lapply(nl1, function(x) c(x, field = ""))
  }
  return(c(nl1, nl2))
}


#Extract tags from a node list.
extractTags = function(nodeList){
  tags = list()
  fields = names(nodeList[[1]]$tagMap)
  for(field in fields){
    tags[[field]] = sapply(nodeList, function(x) x$tagMap[[field]])
  }
  return(tags)
}

entryDFFields = c("doc", "unit", "token")
unitDFFields = c("doc", "unitStart", "unitEnd", "unitSeq", "pID")
tokenDFFields = c("doc", "unit", "discourseTokenSeq", "tokenSeq")
chunkDFFields = c("doc", "name")
trackDFFields = c("doc", "chain", "sourceLink", "token")
trackChainDFFields = c("doc", "chainSeq", "name")
linkDFFields = c("doc", "source", "goal", "type")
corpusDFFields = c("doc")
docDFFields = c("doc")

#Extract information from a node and turn into a data.frame.
nodeToDF = function(nodeList, fields){
  propList = list(id = names(nodeList))

  for(field in fields){
    propList[[field]] = sapply(nodeList, function(x) x[[field]])
  }
  if("tagMap" %in% names(nodeList[[1]])){
    propList = c(propList, extractTags(nodeList))
  }
  if(any(sapply(propList, is.list))){
    missing = names(propList)[sapply(propList, is.list)]
    warning("One or more of the fields specified is not present in some of the nodes. These will be removed: ", paste(missing, sep = ", "))
    for(prop in missing) propList[[prop]] = NULL
  }
  df = data.frame(propList)
  return(df)
}

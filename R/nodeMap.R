#This is a series of functions for nodeMap objects!
#Most of it has no reason to be exported at the moment.
#Terminology: Node list = list of nodes of the same type
#Table of contents:
#1) The constructor function: new_nodeMap
#2) Helper function that takes an imported node map and turns it into a nodeMap: nodeMap
#3) Merge two node maps/lists: mergeNodeMaps, mergeNodeLists
#4) Extracting tags from a node list: extractTags


#Constructor function for nodeMap.
#organisedNodeMap: an organised node map, i.e. sorted into node types
#ID lists: ID lists in the node map
#smallMaps:
new_nodeMap = function(organisedNodeMap, idLists, smallMaps){
  stopifnot(is.list(organisedNodeMap))
  stopifnot(is.list(idLists))
  stopifnot(is.list(smallMaps))

  structure(organisedNodeMap, class = "nodeMap", idLists = idLists, smallMaps = smallMaps)
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
  fields = unique(unlist(lapply(nodeList, function(x) names(x[["tagMap"]]))))
  for(field in fields){
    tags[[field]] = sapply(nodeList, function(x) x$tagMap[[field]])
  }
  return(tags)
}
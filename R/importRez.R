#This is a series of functions turning .rez files into rezonateR objects.

#' Import a Rez file
#'
#' Import a Rez file. This returns an object containing, among other things, a nodeMap object containing raw information, and data frames for tokens, units, chunks, track chain entries, track chains, containing only key information likely to be useful for the user.
#'
#' @param path A character vector of paths to the files to be imported. For Windows users, please use / instead of \.
#' @param docnames A character vector of the document names. If left blank, a docname will be generated according to the filenames of files you import. For example, the document foo/bar.rez will be named 'bar'.
#' @param concatFields A string of names of token-level fields, for example word or transcription, that should be concatenated to form chunk- or entry-level fields. For example, if your word field is called 'word' and you have an IPA transcription field called 'ipa', then concatFields should be c("word", "ipa").
#' @param layerRegex A list, each of which is a component (just track or chunk for now; stack and rez to be added later). In each list entry, there are three components: 'field' is the field on which the splitting is based; 'regex' is a vector of regular expressions; 'names' is a vector of layer names. 'regex' should have one fewer entry than 'names', as the last of the 'names' should be the default case.
#'
#' @return rezRObject
#' @import stringr
#' @import rlang
#' @export
importRez = function(paths, docnames = "", concatFields = c("word", "wordWylie", "lit"), layerRegex){
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

    #Merging node maps together when there are multiple docs
    if(length(paths) > 1){
      fullNodeMap = Reduce(mergeNodeMaps, nodeMapSep[2:length(nodeMapSep)], mergeNodeMaps[[1]])
    } else {
      fullNodeMap = nodeMapSep[[1]]
    }

    #DF representation
    #TODO: Conditional on these things actually existing
    unitDF = nodeToDF(fullNodeMap[["unit"]], unitDFFields)
    tokenDF = nodeToDF(fullNodeMap[["token"]], tokenDFFields)
    entryDF = nodeToDF(fullNodeMap[["entry"]], entryDFFields)
    chunkDF = nodeToDF(fullNodeMap[["chunk"]], chunkDFFields)
    trackDF = nodeToDF(fullNodeMap[["track"]], trackDFFields)
    trackChainDF = nodeToDF(fullNodeMap[["trackChain"]], trackChainDFFields)
    linkDF = nodeToDF(fullNodeMap[["link"]], linkDFFields)
    docDF = nodeToDF(fullNodeMap[["doc"]], docDFFields)


    #Adding fields to higher-level DFs that depend on lower-level DFs.
    entryDF = entryDF %>% rez_left_join(tokenDF, by = c(token = "id", doc = "doc", unit = "unit"))
    unitDF = concatStringFields(entryDF, unitDF, fullNodeMap[["unit"]], concatFields, tokenListName = "entryList")
    unitDF = getSeqBounds(entryDF, unitDF, fullNodeMap[["unit"]], "discourseTokenSeq", tokenListName = "entryList")

    chunkDF = getSeqBounds(tokenDF, chunkDF, fullNodeMap[["chunk"]], c("tokenSeq", "discourseTokenSeq"))
    chunkDF = concatStringFields(tokenDF, chunkDF, fullNodeMap[["chunk"]], concatFields)

    trackDF = trackDF %>% rez_left_join(mergeTokenChunk(tokenDF, chunkDF), by = c(token = "id", doc = "doc"))

    #Adding fields to lower-level DFs that depend on higher-level DFs.
    trackDF = trackDF %>% rez_left_join(trackChainDF, by = c(chain = "id", doc = "doc"))

    #TODO: Rez, Stack, Clique

    #Split DFs by layer
    #layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr")))
    layeredTypes = c("track", "chunk")
    for(type in layeredTypes){
      if(type %in% names(layerRegex)){
        info = layerRegex[[type]]
        #Validate input
        if(length(info[["regex"]]) > length(info[["names"]])){
          stop("You have more regexes than name for track layers.")
        } else if(length(info[["names"]]) > length(info[["regex"]]) + 1){
          stop("In the track layers, the number of names should be equal to or one more than the number of regexes.")
        } else if(length(info[["names"]]) == length(info[["regex"]])){
          info[["regex"]] = info[["regex"]][-length(info[["regex"]])]
          warning("In the track layers, you have as many regexes as you have names. I will ignore the last regex; it will be the default case.")
        }

        #Split DFs into layers
        conds = c(paste0("str_detect(", info[["field"]], ", \'", c(info[["regex"]]), "\')"), "T")
        cwText = paste0(conds, " ~ '", info[["names"]], "'")
        splitLayers = function(x){
          result = mutate(x, layer = case_when(!!!parse_exprs(cwText))) %>% rez_group_split(layer)
          names(result) = sort(info[["names"]])
          result
        }

        if(type == "track"){
          trackDF = trackDF %>% splitLayers
          trackChainDF = trackChainDF %>% splitLayers
        } else if(type == "chunk"){
          chunkDF = chunkDF %>% splitLayers
        }
      } else {
        if(type == "track"){
          trackDF = list("default" = trackDF)
          trackChainDF = list("default" = trackChainDF)
        } else if(type == "chunk"){
          chunkDF = list("default" = chunkDF)
        }
      }
    }

    returnObj = new_rezrObj(list(nodeMap = fullNodeMap, entryDF = entryDF, unitDF = unitDF, tokenDF = tokenDF, chunkDF = chunkDF, trackDF = trackDF, trackChainDF = trackChainDF, linkDF = linkDF, docDF = docDF))
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
  fields = unique(unlist(lapply(nodeList, function(x) names(x[["tagMap"]]))))
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
#Field types:
#key - Primary key of the table, can't be changed.
#core - Integral to the object. If changed, no guarantee that we can still get the Rez file.
#flex - Can be flexibly changed with minimal damage.
#auto - Automatically generated from other fields. Any change will be overridden when updating the table.
#fkey - foreign key to another table.
#foreign - Value is automatically determined from another table. Any change will be overridden when updating the table.
nodeToDF = function(nodeList, fields){
  #For each property extract the vector of values of that property
  #Node map organisation is node -> property, we want property -> node for DF
  propList = list(id = names(nodeList))
  for(field in fields){
    propList[[field]] = sapply(nodeList, function(x) x[[field]])
  }

  #Add tagMap tags
  if("tagMap" %in% names(nodeList[[1]])){
    propList = c(propList, extractTags(nodeList))
  }
  if(any(sapply(propList, is.list))){
    missing = names(propList)[sapply(propList, is.list)]
    warning("One or more of the fields specified is not present in some of the nodes: ", paste(missing, sep = ", "))
    for(prop in missing){
      propList[[prop]] = sapply(propList[[prop]], function(x){
        if(is.null(x)) NA else x
      })
    }
  }
  df = data.frame(propList, stringsAsFactors = F)

  #Attributes of the rezrDFs
  #The first column is always the primary key, then the fields that are direct properties of the node, then members of the tagMap.
  #updateFunct is just an empty list since there are no update functions yet (they are only relevant to auto fields).
  fieldaccess = c("key", rep("core", length(fields)), rep("flex", length(propList) - length(fields) - 1))
  fieldaccess[which(fields == "name")] = "flex"
  names(fieldaccess) = names(propList)

  #attr(df, "fieldaccess") = fieldaccess

  inNodeMap = c("key", rep("primary", length(fields)), rep("tagmap", length(propList) - length(fields) - 1))
  names(inNodeMap) = names(propList)

  return(new_rezrDF(as_tibble(df), fieldaccess, list(), inNodeMap))
}

#' Constructor function for rezrDF
#'
#' @param df The data.frame
#' @param fieldaccess A set of field access values
#' @param updateFunct A set of update functions
#' @param inNodeMap Whether the fields of the data.frame are in the node map
#'
#' @return A rezrDF object
#' @export
new_rezrDF = function(df, fieldaccess, updateFunct, inNodeMap){
  #Validate all components of the DF
  stopifnot(is_tibble(df))
  stopifnot(is.vector(fieldaccess))
  stopifnot(!is.null(names(fieldaccess)))
  stopifnot(is.list(updateFunct))
  if(length(updateFunct) > 1){
    stopifnot(all(sapply(updateFunct, is.function)))
  }
  stopifnot(is.vector(inNodeMap))
  stopifnot(!is.null(names(inNodeMap)))

  structure(df, class = c("rezrDF", "tbl_df", "tbl", "data.frame"), fieldaccess = fieldaccess, updateFunct = updateFunct, inNodeMap = inNodeMap)
}

lowerToHigher = function(simpleDF = NULL, complexDF, complexNodeMap = NULL, fieldnames, higherFieldnames = "", action, seqName = "discourseTokenSeq", tokenListName = "tokenList", simpleDFAddress = "", complexNodeMapAddress = "", rezrObj = NULL, fieldaccess = "foreign"){
  if(is.null(complexNodeMap)){
    if(!all(complexNodeMapAddress == "") & !is.null(rezrObj)){
      complexNodeMap = listAt(rezrObj, "nodeMap/" %+% complexNodeMapAddress)
    } else {
      stop("No complex node map and/or rezrObj specified.")
    }
  }
  if(is.null(simpleDF)){
    if(!all(simpleDFAddress == "") & !is.null(rezrObj)){
      simpleDF = listAt(rezrObj, simpleDFAddress)
    } else {
      stop("No complex DF and/or rezrObj specified.")
    }
  }

  if(!all(higherFieldnames == "") & length(fieldnames) != length(higherFieldnames)){
    stop("Field name vectors do not match in length.")
  }

  if(!all(complexDF$id %in% names(complexNodeMap))){
    stop("Some of the IDs in the complex DF are not found in the complex node map. Action terminated. Check that your complex DF and complex node map match.")
  } else {
    #Reorder the complex node map to match the id column of the complex DF and discard unneeded entries.
    complexNodeMap = complexNodeMap[complexDF$id]
  }

  #If there are no higher fieldnames passed, then the higher DF has the same fieldname as the lower DF.
  if(all(higherFieldnames == "")){
    higherFieldnames = fieldnames
  }

  #Get the values of the field for the higher DF, and store in the colsToAdd list.
  colsToAdd = list()
  for(i in 1:length(fieldnames)){
    fieldname = fieldnames[i]
    higherFieldname = higherFieldnames[i]
    colsToAdd[[higherFieldname]] = sapply(complexNodeMap, function(entry){
      simpleDF %>% filter(id %in% entry[[tokenListName]]) %>% arrange(!!as.symbol(seqName)) %>% select(!!fieldname) %>% unlist %>% action
    })
  }

  #Integrate the data in the colsToAdd list to the complexDF
  oldFieldnames = intersect(names(complexDF), higherFieldnames)
  newFieldnames = setdiff(higherFieldnames, names(complexDF))
  complexDF = complexDF %>% mutate(across(all_of(oldFieldnames), function(x) colsToAdd[[cur_column()]]))
  for(name in newFieldnames){
    values = colsToAdd[[name]]
    complexDF = complexDF %>% rez_mutate(!!name := values, fieldaccess = fieldaccess)
  }

  if(length(fieldaccess) > 1){
    stop("Multiple field access values not yet implemented for lowerToHigher functions. Please perform action separately.")
  }

  #Attribute stuff
  for(i in 1:length(higherFieldnames)){
    field = fieldnames[i]
    higherField = higherFieldnames[i]
    if(!(higherField %in% names(updateFunct(complexDF)))){
        if(simpleDFAddress != "" & complexNodeMapAddress != ""){
          updateFunct(complexDF, higherField) = createLowerToHigherUpdate(simpleDFAddress %+% "/" %+% field, complexNodeMapAddress %+% "/" %+% tokenListName, action, higherField, fkeyInDF = FALSE, seqName)
        } else {
          message("You didn't specify simple and/or complex DF addresses, so I cannot create an update function for you.")
        }
    }
  }

  complexDF
}

#' Concatenate string values in a lower-level data frame
#'
#' Not currently available to users. I will later write a wrapper so that the user doesn't have to directly pass the DFs and node map, but can simply pass the full rez object and the names of the two DFs, and the function will also return the full rez object.
#'
#' @param simpleDF The lower-level dataframe, for example the token dataframe for chunks and units, or the unit dataframe for stacks.
#' @param complexDF The dataframe that you're trying to add the concatenated fields to.
#' @param complexNodeMap The node map corresponding to the simpleDF.
#' @param fieldnames The fields to be concatenated.
#'
#' @return complexDF
concatStringFields = function(simpleDF, complexDF, complexNodeMap, fieldnames, tokenListName = "tokenList"){
  lowerToHigher(simpleDF, complexDF, complexNodeMap, fieldnames, "", function(x) paste(x, collapse = ""), tokenListName = tokenListName)
}

#Get the max and min of a certain value (typically sequence or time) from a lower object table to a higher object table.
getSeqBounds = function(simpleDF, complexDF, complexNodeMap, fieldnames, simpleIsAtom = T, seqName = "", tokenListName = "tokenList"){
  if(seqName == ""){
    if(simpleIsAtom){
      seqName = "discourseTokenSeq"
    } else {
      seqName = "discourseTokenSeqFirst"
    }
  }

  if(simpleIsAtom){
    complexDF = lowerToHigher(simpleDF, complexDF, complexNodeMap, fieldnames, paste0(fieldnames, "First"), min, seqName = seqName, tokenListName = tokenListName)
    complexDF = lowerToHigher(simpleDF, complexDF, complexNodeMap, fieldnames, paste0(fieldnames, "Last"), max, seqName = seqName, tokenListName = tokenListName)
  } else {
    complexDF = lowerToHigher(simpleDF, complexDF, complexNodeMap, paste0(fieldnames, "First"), paste0(fieldnames, "First"), min, seqName = seqName, tokenListName = tokenListName)
    complexDF = lowerToHigher(simpleDF, complexDF, complexNodeMap, paste0(fieldnames, "Last"), paste0(fieldnames, "Last"), max, seqName = seqName, tokenListName = tokenListName)
  }
  complexDF
}


#Merge token and chunk DFs.
#This is mostly for handling chains, which may refer to a mix of tokens and chunks in Rezonator. (Single-token chain entries are not automatically stored as chunks in Rezonator.)
mergeTokenChunk = function(tokenDF, chunkDF){
  #This is because chunk have begins/end; tokens do not.
  tokenDF = tokenDF %>% mutate(tokenSeqFirst = tokenSeq, tokenSeqLast = tokenSeq, discourseTokenSeqFirst = discourseTokenSeq, discousreTokenSeqLast = discourseTokenSeq)

  commonFields = intersect(colnames(tokenDF), colnames(chunkDF))
  (tokenDF %>% select(all_of(commonFields))) %>% rbind(chunkDF %>% select(commonFields))
}

#' Constructor function for rezrObj.
#'
#' @param list
#'
#' @return The rezObj.
#' @export
new_rezrObj = function(list){
  stopifnot("nodeMap" %in% names(list))
  stopifnot("tokenDF" %in% names(list))
  stopifnot("unitDF" %in% names(list))

  structure(list, class = "rezrObj")
}

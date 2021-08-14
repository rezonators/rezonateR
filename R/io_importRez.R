#This is a series of functions turning .rez files into rezonateR objects!
#This includes the main function, importRez, plus internal functions supporting it.
#Table of contents:
#1) The main import function: importRez
#2) dfFields: The non-tag fields to be extracted from the node maps to the rezrDFs.
#3) Change a list of nodes into a data frame: nodeToDF
#4) Merge the token and chunk tables: mergeTokenChunk

#' Import a Rez file
#'
#' Import a Rez file. This returns an object containing, among other things, a nodeMap object containing raw information, and data frames for tokens, units, chunks, track chain entries, track chains, containing only key information likely to be useful for the user.
#'
#' @param paths A character vector of paths to the files to be imported. For Windows users, please use / instead of \.
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
      lastSlashLocs = str_locate_all(paths, "/")
      lastRezLocs = str_locate_all(paths, "\\.rez")
      docnames_start = sapply(1:length(paths), function(x) lastSlashLocs[[x]][nrow(lastSlashLocs[[x]]),1] + 1)
      docnames_end = sapply(1:length(paths), function(x) lastRezLocs[[x]][nrow(lastRezLocs[[x]]),1] - 1)
      docnames = sapply(1:length(paths), function(x) substr(paths[x], docnames_start, docnames_end))
    }

    nodeMapSep = list()
    for(x in 1:length(paths)){
      path = paths[x]
      rezJSON = rjson::fromJSON(file = paths) #Importing the file
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
    entryDF = entryDF %>% rez_left_join(tokenDF, by = c(token = "id", doc = "doc", unit = "unit"), df2Address = "tokenDF", fkey = "token")

    unitDF = concatStringFields(entryDF, unitDF, fullNodeMap[["unit"]], concatFields, tokenListName = "entryList", simpleDFAddress = "entryDF", complexNodeMapAddress = "unit")
    unitDF = getSeqBounds(entryDF, unitDF, fullNodeMap[["unit"]], "discourseTokenSeq", tokenListName = "entryList", simpleDFAddress = "entryDF", complexNodeMapAddress = "unit")

    chunkDF = getSeqBounds(tokenDF, chunkDF, fullNodeMap[["chunk"]], c("tokenSeq", "discourseTokenSeq"), simpleDFAddress = "tokenDF", complexNodeMapAddress = "chunk")
    chunkDF = concatStringFields(tokenDF, chunkDF, fullNodeMap[["chunk"]], concatFields, simpleDFAddress = "tokenDF", complexNodeMapAddress = "chunk")
    fieldaccess(chunkDF, concatFields) = "foreign"

    trackDF = trackDF %>% rez_left_join(mergeTokenChunk(tokenDF, chunkDF), by = c(token = "id", doc = "doc"), df2Address = c("tokenDF", "chunkDF"), fkey = "token")

    #Adding fields to lower-level DFs that depend on higher-level DFs.
    trackDF = trackDF %>% rez_left_join(trackChainDF, by = c(chain = "id", doc = "doc"), df2Address = "trackChainDF", fkey = "token")

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
          result = suppressMessages(rez_mutate(x, layer = case_when(!!!parse_exprs(cwText))) %>% rez_group_split(layer))
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

    #Track DFs needs dependencies fixed for two reasons:
    #1) First, Last mistakenly present in dependencies for tokenDF (not the most elegant solution to solve here, should think about this later)
    #2) After chunks and trackChainDF got split into layers, need to update addresses
    for(trackLayer in names(trackDF)){
      for(i in 1:length(updateFunct(trackDF[[trackLayer]]))){
        uf = updateFunct(trackDF[[trackLayer]])[[i]]
        field = names(updateFunct(trackDF[[trackLayer]]))[i]
        if(any(str_detect(deps(uf), "chunkDF"))){
        #Grab original dependency data
          sourceFieldToken = (deps(updateFunct(trackDF[[trackLayer]], field))[1] %>% strsplit("/"))[[1]] %>% last %>% chompSuffix("First|Last")
          sourceFieldChunk = (deps(updateFunct(trackDF[[trackLayer]], field))[2] %>% strsplit("/"))[[1]] %>% last
          sourceField = c(sourceFieldToken, rep(sourceFieldChunk, length(chunkDF)))
          updateFunct(trackDF[[trackLayer]], field) = createLeftJoinUpdate(address = c("tokenDF", paste0("chunkDF/", names(chunkDF))) %+% "/" %+% sourceField, fkey = "token", field =field)
        } else if(any(str_detect(deps(uf), "trackChainDF"))){
          sourceField = (deps(updateFunct(trackDF[[trackLayer]], field))[1] %>% strsplit("/"))[[1]] %>% last
          updateFunct(trackDF[[trackLayer]], field) = createLeftJoinUpdate(address = "trackChainDF/" %+% trackLayer %+% "/" %+% sourceField, fkey = "chain", field = field)
        }
        #print(updateFunct(trackDF[[trackLayer]], field))
      }
    }

    returnObj = new_rezrObj(list(nodeMap = fullNodeMap, entryDF = entryDF, unitDF = unitDF, tokenDF = tokenDF, chunkDF = chunkDF, trackDF = trackDF, trackChainDF = trackChainDF, linkDF = linkDF, docDF = docDF))
    return(returnObj)
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

  #Rezonator sometimes has an issue where a tag is present only in a subset of nodes of a certain type
  #These will be treated as NA
  if(any(sapply(propList, is.list))){
    missing = names(propList)[sapply(propList, is.list)]
    message("One or more of the fields specified is present in only some of the nodes in one of the node lists: ", paste(missing, sep = ", "))
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

#Merge token and chunk DFs.
#This is mostly for handling chains, which may refer to a mix of tokens and chunks in Rezonator. (Single-token chain entries are not automatically stored as chunks in Rezonator.)
mergeTokenChunk = function(tokenDF, chunkDF){
  #This is because chunk have begins/end; tokens do not.
  tokenDF = tokenDF %>% mutate(tokenSeqFirst = tokenSeq, tokenSeqLast = tokenSeq, discourseTokenSeqFirst = discourseTokenSeq, discourseTokenSeqLast = discourseTokenSeq)

  commonFields = intersect(colnames(tokenDF), colnames(chunkDF))
  (tokenDF %>% select(all_of(commonFields))) %>% rbind(chunkDF %>% select(commonFields))
}

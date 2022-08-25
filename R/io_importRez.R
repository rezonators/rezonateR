#This is a series of functions turning .rez files into rezonateR objects!
#This includes the main function, importRez, plus internal functions supporting it.
#Table of contents:
#1) The main import function: importRez
#2) dfFields: The non-tag fields to be extracted from the node maps to the rezrDFs.
#3) Change a list of nodes into a data frame: nodeToDF
#4) Merge the token and chunk tables: mergeTokenChunk

#' Import a Rez file
#'
#' Import a Rez file. This returns an object containing, among other things, a `nodeMap` object containing raw information, and data frames for tokens, units, chunks, track chain entries, track chains, containing only key information likely to be useful for the user.
#'
#' @param paths A character vector of paths to the files to be imported. For Windows users, please use / instead of \.
#' @param docnames A character vector of the document names. If left blank, a `docname` will be generated according to the filenames of files you import. For example, the document foo/bar.rez will be named 'bar'.
#' @param concatFields A string of names of token-level fields, for example word or transcription, that should be concatenated to form chunk- or entry-level fields. For example, if your word field is called 'word' and you have an IPA transcription field called 'ipa', then concatFields should be c("word", "ipa").
#' @param separator The character you wish to use to separate words in concatenated columns, generally the empty string in languages like Tibetan and Chinese, and a single space in languages like Spanish and English.
#' @param layerRegex A list, each of which is a component (just tree, track, rez, or chunk for now; stack to be added later). In each list entry, there are three components: `field` is the field on which the splitting is based; `regex` is a vector of regular expressions; `names` is a vector of layer names. `regex` should have one fewer entry than `names`, as the last of the '`names`' should be the default case.
#'
#' @return A rezrObj object. See [rezonateR::new_rezrObj] for details.
#' @note After import, you may consider calling such functions as [rezonateR::addUnitSeq], [rezonateR::addIsWordField] or [rezonateR::getAllTreeCorrespondences], which are excluded from the import because of performance issues.
#' @import stringr
#' @import rlang
#' @examples
#' path = system.file("extdata", "sbc007.rez", package = "rezonateR", mustWork = T)
#' layerRegex = list(chunk = list(field = "chunkType", regex = c("verb"), names = c("verb", "refexpr")))
#' concatFields = c("text", "transcript")
#' rez007 = importRez(path, layerRegex = layerRegex, concatFields = concatFields)
#' @export
importRez = function(paths, docnames = "", concatFields, layerRegex = list(), separator = " "){
    if(length(paths) != length(docnames) & docnames != ""){
      docnames = ""
      warning("Number of input paths does not match the number of document names. I will name your documents automatically, according to your filenames.")
    }

    message("Import starting - please be patient ...")
    if(docnames == ""){
      #Detecting document names
      lastSlashLocs = str_locate_all(paths, "/")
      lastRezLocs = str_locate_all(paths, "\\.rez")
      docnames_start = sapply(1:length(paths), function(x) lastSlashLocs[[x]][nrow(lastSlashLocs[[x]]),1] + 1)
      docnames_end = sapply(1:length(paths), function(x) lastRezLocs[[x]][nrow(lastRezLocs[[x]]),1] - 1)
      docnames = sapply(1:length(paths), function(x) substr(paths[x], docnames_start, docnames_end))
    }


    message("Creating node maps ...")

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

    message("Creating rezrDFs ...")


    #DF representation
    unitDF = nodeToDF(fullNodeMap[["unit"]], unitDFFields)
    tokenDF = nodeToDF(fullNodeMap[["token"]], tokenDFFields)
    entryDF = nodeToDF(fullNodeMap[["entry"]], entryDFFields)

    if("chunk" %in% names(fullNodeMap)){
      chunkDF = nodeToDF(fullNodeMap[["chunk"]], chunkDFFields)
    }
    if("track" %in% names(fullNodeMap)){
      trackDF = nodeToDF(fullNodeMap[["track"]], trackDFFields)
      trailDF = nodeToDF(fullNodeMap[["trail"]], trailDFFields)
    }
    if("rez" %in% names(fullNodeMap)){
      rezDF = nodeToDF(fullNodeMap[["rez"]], rezDFFields)
      resonanceDF = nodeToDF(fullNodeMap[["resonance"]], resonanceDFFields)
    }
    if("link" %in% names(fullNodeMap)){
      linkDF = nodeToDF(fullNodeMap[["link"]], linkDFFields)
    }
    if("tree" %in% names(fullNodeMap)){
      treeDF = nodeToDF(fullNodeMap[["tree"]], treeDFFields)
      treeEntryDF = nodeToDF(fullNodeMap[["treeEntry"]], treeEntryDFFields)
      treeLinkDF = nodeToDF(fullNodeMap[["treeLink"]], linkDFFields)
    }
    if("stack" %in% names(fullNodeMap)){
      stackDF = nodeToDF(fullNodeMap[["stack"]], stackDFFields)
      cardDF = nodeToDF(fullNodeMap[["card"]], cardDFFields)
    }

    docDF = nodeToDF(fullNodeMap[["doc"]], docDFFields)

    message("Adding foreign fields to rezrDFs and sorting (this is the slowest step) ...")
    tokenDF = tokenDF %>% arrange(docTokenSeq)

    message(">Adding to unit entry DF ...")
    #Adding fields to higher-level DFs that depend on lower-level DFs.
    entryDF = entryDF %>% rez_left_join(tokenDF, by = c(token = "id", doc = "doc", unit = "unit"), df2Address = "tokenDF", fkey = "token")
    entryDF = entryDF %>% arrange(docTokenSeq)

    message(">Adding to unit DF ...")
    unitDF = concatStringFields(entryDF, unitDF, fullNodeMap[["unit"]], concatFields, tokenListName = "entryList", simpleDFAddress = "entryDF", complexNodeMapAddress = "unit", separator = separator)
    unitDF = getSeqBounds(entryDF, unitDF, fullNodeMap[["unit"]], "docTokenSeq", tokenListName = "entryList", simpleDFAddress = "entryDF", complexNodeMapAddress = "unit")
    unitDF = unitDF %>% arrange(docTokenSeqFirst, docTokenSeqLast)

    if("chunk" %in% names(fullNodeMap)){
      message(">Adding to chunk DF ...")
      chunkDF = getSeqBounds(tokenDF, chunkDF, fullNodeMap[["chunk"]], c("tokenOrder", "docTokenSeq"), simpleDFAddress = "tokenDF", complexNodeMapAddress = "chunk")
      chunkDF = concatStringFields(tokenDF, chunkDF, fullNodeMap[["chunk"]], concatFields, simpleDFAddress = "tokenDF", complexNodeMapAddress = "chunk", separator = separator)
      fieldaccess(chunkDF, concatFields) = "foreign"
      chunkDF = chunkDF %>% arrange(docTokenSeqFirst, docTokenSeqLast)
      mergedDF = mergeTokenChunk(tokenDF, chunkDF)
    }


    if("rez" %in% names(fullNodeMap)){
      message(">Adding to track DFs ...")
      #mergedDF from the previous condition
      rezDF = rezDF %>% rez_left_join(mergedDF, by = c(token = "id", doc = "doc"), df2Address = c("tokenDF", "chunkDF"), fkey = "token")
      #Adding fields to lower-level DFs that depend on higher-level DFs.
      rezDF = rezDF %>% rez_left_join(resonanceDF, by = c(chain = "id", doc = "doc"), df2Address = "resonanceDF", fkey = "token")
      rezDF = rezDF %>% arrange(docTokenSeqFirst, docTokenSeqLast)
    }


    if("track" %in% names(fullNodeMap)){
      message(">Adding to track DFs ...")
      #mergedDF from the previous condition
      trackDF = trackDF %>% rez_left_join(mergedDF, by = c(token = "id", doc = "doc"), df2Address = c("tokenDF", "chunkDF"), fkey = "token")
      #Adding fields to lower-level DFs that depend on higher-level DFs.
      trackDF = trackDF %>% rez_left_join(trailDF, by = c(chain = "id", doc = "doc"), df2Address = "trailDF", fkey = "token")
      trackDF = trackDF %>% arrange(docTokenSeqFirst, docTokenSeqLast)
    }

    if("tree" %in% names(fullNodeMap)){
      message(">Adding to tree DFs ...")
      treeEntryDF = getSeqBounds(tokenDF, treeEntryDF, fullNodeMap[["treeEntry"]], c("tokenOrder", "docTokenSeq"), simpleDFAddress = "tokenDF", complexNodeMapAddress = "treeEntry")
      treeEntryDF = concatStringFields(tokenDF, treeEntryDF, fullNodeMap[["treeEntry"]], concatFields, simpleDFAddress = "tokenDF", complexNodeMapAddress = "treeEntry", separator = separator)
      fieldaccess(treeEntryDF, concatFields) = "foreign"
      treeEntryDF = treeEntryDF %>% arrange(docTokenSeqFirst, docTokenSeqLast)
      treeEntryDF = treeEntryDF %>% getTreeOfEntry(treeLinkDF, fullNodeMap$tree)
      treeEntryDF = suppressMessages(treeEntryDF %>% rez_left_join(treeLinkDF %>% select(-goal, -doc, -type), df2Address = "treeLink", fkey = "sourceLink", by = c("sourceLink" = "id")) %>% rez_rename(parent = source))

      treeDF = getSeqBounds(tokenDF, treeDF, fullNodeMap[["tree"]], c("tokenOrder", "docTokenSeq"), simpleDFAddress = "tokenDF", complexNodeMapAddress = "tree")
      treeDF = concatStringFields(tokenDF, treeDF, fullNodeMap[["tree"]], concatFields, simpleDFAddress = "tokenDF", complexNodeMapAddress = "tree", separator = separator)
      fieldaccess(treeDF, concatFields) = "foreign"
      treeDF = treeDF %>% arrange(docTokenSeqFirst, docTokenSeqLast)
      }

    #TODO: Rez, Stack, Clique

    message("Splitting rezrDFs into layers ...")

    #Split DFs by layer
    #layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr")))
    layeredTypes = c("track", "chunk", "tree", "rez") %>% intersect(names(fullNodeMap))
    for(type in layeredTypes){
      if(type %in% names(layerRegex)){
        info = layerRegex[[type]]
        #Validate input
        if(length(info[["regex"]]) > length(info[["names"]])){
          stop("You have more regexes than name for layers.")
        } else if(length(info[["names"]]) > length(info[["regex"]]) + 1){
          stop("In the layers object, the number of names should be equal to or one more than the number of regexes.")
        } else if(length(info[["names"]]) == length(info[["regex"]])){
          info[["regex"]] = info[["regex"]][-length(info[["regex"]])]
          warning("In the layers object, you have as many regexes as you have names. I will ignore the last regex; it will be the default case.")
        }

        #Split DFs into layers
        conds = c(paste0("str_detect(", info[["field"]], ", \'", c(info[["regex"]]), "\')"), "T")
        cwText = paste0(conds, " ~ '", info[["names"]], "'")
        splitLayers = function(x){
          result = suppressMessages(rez_mutate(x, layer = case_when(!!!parse_exprs(cwText))) %>% rez_group_split(layer))
          names(result) = sapply(result, function(x) x$layer[1])
          result
        }

        if(type == "track"){
          trackDF = trackDF %>% splitLayers
          trailDF = trailDF %>% splitLayers
        } else if(type == "chunk"){
          chunkDF = chunkDF %>% splitLayers
        } else if(type == "tree"){
          treeDF = treeDF %>% splitLayers
          treeEntryDF = treeEntryDF %>% splitLayers
        } else if(type == "rez"){
          rezDF = trackDF %>% splitLayers
          resonanceDF = trailDF %>% splitLayers
        }
      } else {
        if(type == "track"){
          trackDF = list("default" = trackDF)
          trailDF = list("default" = trailDF)

          trackDF$default = trackDF$default %>% rez_mutate(layer = "default")
          trailDF$default = trailDF$default %>% rez_mutate(layer = "default")
        } else if(type == "chunk"){
          chunkDF = list("default" = chunkDF)

          chunkDF$default = chunkDF$default %>% rez_mutate(layer = "default")
        } else if(type == "tree"){
          treeDF = list("default" = treeDF)
          treeEntryDF = list("default" = treeEntryDF)

          treeDF$default = treeDF$default %>% rez_mutate(layer = "default")
          treeEntryDF$default = treeEntryDF$default %>% rez_mutate(layer = "default")
        } else if(type == "rez"){
          rezDF = list("default" = rezDF)
          resonanceDF = list("default" = resonanceDF)

          rezDF$default = rezDF$default %>% rez_mutate(layer = "default")
          resonanceDF$default = resonanceDF$default %>% rez_mutate(layer = "default")
        }
      }
    }

    message("A few finishing touches ...")
    #Track DFs needs dependencies fixed for two reasons:
    #1) First, First/Last mistakenly present in dependencies for tokenDF (not the most elegant solution to solve here, should think about this later)
    #2) After chunks and trailDF got split into layers, need to update addresses
    if("track" %in% names(fullNodeMap)){
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
          } else if(any(str_detect(deps(uf), "trailDF"))){
            sourceField = (deps(updateFunct(trackDF[[trackLayer]], field))[1] %>% strsplit("/"))[[1]] %>% last
            updateFunct(trackDF[[trackLayer]], field) = createLeftJoinUpdate(address = "trailDF/" %+% trackLayer %+% "/" %+% sourceField, fkey = "chain", field = field)
          }
          #print(updateFunct(trackDF[[trackLayer]], field))
        }
      }
    }

    objList = list(nodeMap = fullNodeMap)
    dfNames = ls(environment())[str_ends(ls(environment()), "DF")]
    for(name in dfNames) objList[[name]] = environment()[[name]]

    returnObj = new_rezrObj(objList)
    message("Done!")
    return(returnObj)
}

entryDFFields = c("doc", "unit", "token")
unitDFFields = c("doc", "unitStart", "unitEnd", "unitSeq", "pID")
tokenDFFields = c("doc", "unit", "docTokenSeq", "tokenOrder")
chunkDFFields = c("doc", "name", "nest")
trackDFFields = c("doc", "chain", "sourceLink", "token")
trailDFFields = c("doc", "chainCreateSeq", "name")
cardDFFields = c("doc", "chain", "unit")
stackDFFields = c("doc", "chainCreateSeq", "name")
rezDFFields = c("doc", "chain", "sourceLink", "token")
resonanceDFFields = c("doc", "chainCreateSeq", "name")
linkDFFields = c("doc", "source", "goal", "type", "subtype")
treeEntryDFFields = c("doc", "order", "sourceLink", "level")
treeDFFields = c("doc", "name", "maxLevel")
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
    #message("One or more of the fields specified is present in only some of the nodes in one of the node lists: ", paste(missing, sep = ", "))
    #useful for debugging but the regular user doesn't need to see this
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
  tokenDF = tokenDF %>% mutate(tokenOrderFirst = tokenOrder, tokenOrderLast = tokenOrder, docTokenSeqFirst = docTokenSeq, docTokenSeqLast = docTokenSeq)

  commonFields = intersect(colnames(tokenDF), colnames(chunkDF))
  (tokenDF %>% select(all_of(commonFields))) %>% rbind(chunkDF %>% select(commonFields))
}

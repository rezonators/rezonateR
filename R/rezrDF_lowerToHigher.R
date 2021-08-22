#This file is for rezrDF-related functions about lowerToHigher operations, i.e. the creation of fields that are based on combining information from another table, several rows at a time.
#Table of contents:
#1) The lowerToHigher function itself: lowerToHigher
#2) Two concrete cases of lowerToHigher:
#  a) Concatenate string fields: concatStringFields
#  b) Getting the max and min of a (generally temporal) numeric field: getSegBounds
#3) Updating a foreign field with lowerToHigher: updateLowerToHigher
#4) Creating an update function with lowerToHigher: createLowerToHigherUpdate

#' Combine information from multiple entries in a lower-level source table to a higher-level target table.
#'
#' @param simpleDF The rezrDF containing simple information (source rezrDF).
#' @param complexDF The rezrDF to contain complex information from the simple table (target rezrDF).
#' @param complexNodeMap The node map corresponding to the complex (target) rezrDF
#' @param fieldnames The field names in the simple rezrDF whose information is to be extracted.
#' @param higherFieldnames The field names in the complex rezrDF whose information is to be extracted.
#' @param action The action to be performed on the data from the source rezrDF
#' @param seqName The name of the sequence field in the source (simple) rezrDF.
#' @param tokenListName The name of the list of foreign keys in the target table.
#' @param simpleDFAddress An address to the simple (source) rezrDF, e.g. 'unitDF' or 'chunkDF/refexpr'. Only needed if you want an automatic update function, i.e. for 'foreign' fields.
#' @param complexNodeMapAddress An address to the complex (target) rezrDF. You don't need the 'nodeMap/' oart or the layer; just type the name of the entity (e.g. track, chunk). Only needed if you want an automatic update function, i.e. for 'foreign' fields.
#' @param rezrObj A rezrObj object.
#' @param fieldaccess The field access value of the field. If not set to foreign, no update function will be automatically added.
#'
#' @return The modified target table (complexDF). Most use cases should be handled by other functions like [rezonateR::addUnitSeq] and [rezonateR::addIsWordField]. If you do call this function, do note that the rezrDF you are changing is the second parameter, not the first. As such, piping should be done like this: someDF %>% getSeqBounds(simpleDF, ., complexNodeMap, ...)
#' @export
lowerToHigher = function(simpleDF = NULL, complexDF, complexNodeMap = NULL, fieldnames, higherFieldnames = "", action, seqName = "discourseTokenSeq", tokenListName = "tokenList", simpleDFAddress = "", complexNodeMapAddress = "", rezrObj = NULL, fieldaccess = "foreign"){
  if(is.null(complexNodeMap)){
    if(!all(complexNodeMapAddress == "") & !is.null(rezrObj)){
      complexNodeMap = listAt(rezrObj, "nodeMap/" %+% complexNodeMapAddress)
    } else {
      stop("No valid complex node map and/or rezrObj specified.")
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

  complexKey = getKey(complexDF)
  if(!all(complexDF[[complexKey]] %in% names(complexNodeMap))){
    stop("Some of the IDs in the complex DF are not found in the complex node map. Action terminated. Check that your complex DF and complex node map match.")
  } else {
    #Reorder the complex node map to match the id column of the complex DF and discard unneeded entries.
    complexNodeMap = complexNodeMap[complexDF[[complexKey]]]
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
#' Not recommended to be called by most users; [rezonateR::addFieldForeign] with [rezonateR:concatenateAll] suffices for most purposes.
#'
#' @param simpleDF The lower-level dataframe, for example the token dataframe for chunks and units, or the unit dataframe for stacks.
#' @param complexDF The dataframe that you're trying to add the concatenated fields to.
#' @param complexNodeMap The node map corresponding to the simpleDF.
#' @param fieldnames The fields to be concatenated.
#' @param separator The character by which words will be separated.
#' @param ... Additional fields 'simpleDFAddress', 'complexNodeMapAddress', 'fieldaccess' (foreign by default) from [rezonateR::lowerToHigher()]. Only needed if you want automatically generated update functions.
#'
#' @export
#' @return complexDF
concatStringFields = function(simpleDF, complexDF, complexNodeMap, fieldnames, tokenListName = "tokenList", separator = "", ...){
  lowerToHigher(simpleDF, complexDF, complexNodeMap, fieldnames, "", function(x) paste(x, collapse = separator), tokenListName = tokenListName, ...)
}

#' Get the max and min of a certain value from a lower object table to a higher object table.
#'
#' This is typically used for sequence or time.
#'
#' @param simpleDF The lower-level dataframe, for example the token dataframe for chunks and units, or the unit dataframe for stacks. There should either be an integer field that contains the sequence in question, or two integer fields containing the first and last (see simpleIsAtom).
#' @param complexDF The dataframe that you're trying to add the concatenated fields to. We will create two integer fields, one for the first and one for the last integer.
#' @param complexNodeMap The node map corresponding to the simpleDF.
#' @param simpleIsAtom If set to T, that means the simpleDF only contains single values, not a range. If set to F, that means the simpleDF contains a range, i.e. somethingFirst and somethingLast.
#' @param fieldnames The fields to be concatenated.
#' @param ... Additional fields 'simpleDFAddress', 'complexNodeMapAddress', 'fieldaccess' (foreign by default) from [rezonateR::lowerToHigher()]. Needed if you want automatically generated update functions.
#'
#' @return A complex DF.
#' @note Most use cases should be handled by other functions like [rezonateR::addUnitSeq] and [rezonateR::addIsWordField]. If you do call this function, do note that the rezrDF you are changing is the second parameter, not the first. As such, piping should be done like this: someDF %>% getSeqBounds(simpleDF, ., complexNodeMap, ...)
#' @export
getSeqBounds = function(simpleDF, complexDF, complexNodeMap, fieldnames, simpleIsAtom = T, seqName = "", tokenListName = "tokenList", exclude0 =  T, ...){
  if(seqName == ""){
    if(simpleIsAtom){
      seqName = "discourseTokenSeq"
    } else {
      seqName = "discourseTokenSeqFirst"
    }
  }

  if(exclude0) fmin = min_no0 else fmin = min
  if(exclude0) fmax = max_no0 else fmax = max

  if(simpleIsAtom){
    complexDF = lowerToHigher(simpleDF, complexDF, complexNodeMap, fieldnames, paste0(fieldnames, "First"), fmin, seqName = seqName, tokenListName = tokenListName, ...)
    complexDF = lowerToHigher(simpleDF, complexDF, complexNodeMap, fieldnames, paste0(fieldnames, "Last"), fmax, seqName = seqName, tokenListName = tokenListName, ...)
  } else {
    #Untested
    complexDF = lowerToHigher(simpleDF, complexDF, complexNodeMap, paste0(fieldnames, "First"), paste0(fieldnames, "First"), fmin, seqName = seqName, tokenListName = tokenListName, ...)
    complexDF = lowerToHigher(simpleDF, complexDF, complexNodeMap, paste0(fieldnames, "Last"), paste0(fieldnames, "Last"), fmax, seqName = seqName, tokenListName = tokenListName, ...)
  }
  complexDF
}

#' Update a field using a lowerToHigher operation.
#'
#' Not normally called by users, but acts as an updateFunct to be called be [rezonateR::reload].

#'
#' @param df The target rezrDF to be updated.
#' @param rezrObj The full rezrObj.
#' @param address An address to the field from the original df, from the rezrObj root. For example, the 'word' field of tokenDF has the address 'tokenDF/word', and the 'word' field of the 'verb' layer of chunkDF has the address 'chunkDF/verb/word'.
#' @param fkeyAddress If fkeyInDF = FALSE, an address to the list of foreign keys inside the nodeMap (from the root rezrObj). If fkeyInDF = TRUE, a field in the target rezrDF containing a vector of foreign keys (not currently supported and will result in an error).
#' @param action The function to be performed on the values in the source rezrDF to combine them into a single value.
#' @param field The name of the field in the target rezrDF to be updated. If the field names in the source DFs are all the same and also the same as the name in the target DF, you may leave this unspecified.
#' @param fkeyInDF See fkey description.
#'
#' @return The updated data frame.
#' @export
updateLowerToHigher = function(df, rezrObj, address, fkeyAddress, action, field = "", fkeyInDF = FALSE, seqName = "discourseTokenSeq"){
  if(length(fkeyAddress) > 1){
    stop("Multiple sources are currently not supported in the updateLowerToHigher function. Please run this function multiple times. Sorry!")
  } else if(fkeyInDF){
    stop("Foreign keys in the current DF in the lowerToHigher function are not currently supported. Please use the correponding nodeMap.")
  }

  #Get the source table, source field, primary key
  sourceTableInfo = getSourceTableInfo(rezrObj, address, "", field)
  unpackList(sourceTableInfo)
  field = sourceTableInfo[["field"]]
  #This operation yields four new variables in the local environment:
  #df2key (source DF key), df2field (source DF info field), df2 (source DF), field (target DF, if not specified at the beginning)

  if(!fkeyInDF){
    #TODO: Multiple sources
    fkeyAddressVec = strsplit(fkeyAddress, "/")[[1]]
    fkeyField = fkeyAddressVec[length(fkeyAddressVec)]
    fkeyPath = fkeyAddressVec[-length(fkeyAddressVec)]
    if(length(fkeyPath) > 1){
      stop("Nested node maps are not currently supported. Please check your foreign key address.")
    }
    complexNodeMap = rezrObj %>% listAt("nodeMap/" %+% fkeyPath)
    fieldnames = field

  } else {
    #TODO: Placeholder for when I support fkeyInDF = T
  }
  result = lowerToHigher(df2, df, complexNodeMap, df2field, field, action, seqName, fkeyField)

  result
}

#' Create an update function based on a lowerToHigher-type action.
#'
#' A function factory that allows the user to create an update function based on a [rezonateR::lowerToUpper] operation. Not to be called by most users; [rezonateR::lowerToHigher] will call this automatically if the necessary data is supplied.
#'
#' @param df The target rezrDF you want to modify.
#' @param rezrObj The rezrObj.
#' @param address The address to the info field that you want from your source rezrDF.
#' @param fkeyAddress The address to the token list in the nodeMap that corresponds to the target rezrDF.
#' @param action The action to be performed to combine the fields.
#' @param field The target field.
#' @param fkeyInDF Whether the foreign key will be in the data frame.
#' @param seqName The name of the sequencing in index in the lower rezrDF.
#'
#' @return An update function for a lowerToHigher-type foreign field.
#' @export
createLowerToHigherUpdate = function(address, fkeyAddress, action, field = "", fkeyInDF = FALSE, seqName = "discourseTokenSeq"){
  if(length(fkeyAddress) > 1){
    stop("Multiple sources are currently not supported in the updateLowerToHigher function. Sorry!")
  } else if(fkeyInDF){
    stop("Foreign keys in the current DF in the lowerToHigher function are not currently supported. Please use the correponding nodeMap.")
  }

  #Create the function itself (easy!)
  field = eval(field) #Force evaluation of these so that they don't depend on parent environment
  address = eval(address)
  fkeyAddress = eval(fkeyAddress)
  action = eval(action)
  fkeyInDF = eval(fkeyInDF)
  seqName = eval(seqName)
  funct = function(df, rezrObj) updateLowerToHigher(df, rezrObj, address, fkeyAddress, action, field, fkeyInDF, seqName)

  #Figure out the deps (actually still pretty simple!)
  deps = address

  new_updateFunction(funct, deps)
}



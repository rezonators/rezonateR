#This file is for ease-of-use editing functions for rezrObj
#They have lots of validation and other ease-of-use features.
#Table of contents:
#1) Add/change a field using local information: addFieldLocal.rezrObj, changeFieldLocal.rezrObj
#  a) Purely internal functions to support the two: getDFFromNames, setDFFromNames, validateACFieldLocalObj
#2) Add/change a field using foreign information: addFieldoreign.rezrObj
#  a) Purely internal functions to support the two: validateSimpleForeignObj
#3) Shortcut functions: addField.rezrObj, changeField.rezrObj

#' Easily add a field to / change a field in a rezrDF from a rezrObj using only information from that rezrDF
#'
#' @rdname acFieldLocal.rezrObj
#' @param rezrObj The rezrObj that you will be changing.
#' @param entity The name of the Rezonator entity that you will be changing - track, rez, chunk, token, unit, stack, etc.
#' @param layer The name of the layer of the Rezonator entity that you wil be changing. Leave blank for entities without layers (i.e. token, entry and unit). If you don't have layers for other entities, type 'default'.
#' @inheritParams addFieldLocal.rezrDF
#'
#' @return A rezrObj with the changed rezrDF.
#' @export
addFieldLocal.rezrObj = function(rezrObj, entity, layer, fieldName, expression, type = "simple", fieldaccess = "flex", groupField = ""){
  entity = chompSuffix(entity, "DF")

  validateACFieldLocalObj(rezrObj, entity, layer, fieldName, expression, type, fieldaccess, groupField)
  oneArgs = c("entity", "layer", "fieldName", "fieldaccess", "type")
  checkIfOne(oneArgs, "You can only add one field at a time with addFieldLocal.")

  df = getDFFromNames(rezrObj, entity, layer)
  if(fieldName %in% names(df)) stop("You cannot add a field with the same name as an existing field.")

  df = addFieldLocal(df, fieldName, !!enexpr(expression), type, fieldaccess, groupField)
  rezrObj = setDFFromNames(rezrObj, entity, layer, df)
  rezrObj
}

#' @rdname acFieldLocal.rezrObj
#' @export
changeFieldLocal.rezrObj = function(rezrObj, entity, layer, fieldName, expression, type = "simple", fieldaccess = "flex", groupField = ""){
  entity = chompSuffix(entity, "DF")

  validateACFieldLocalObj(rezrObj, entity, layer, fieldName, expression, type, fieldaccess, groupField)
  oneArgs = c("entity", "layer", "fieldName", "fieldaccess", "type")
  checkIfOne(oneArgs, "You can only change one field at a time with changeFieldLocal.")

  df = getDFFromNames(rezrObj, entity, layer)
  if(!(fieldName %in% names(df))) stop("Field does not exist.")

  df = changeFieldLocal(df, fieldName, !!enexpr(expression), type, fieldaccess, groupField)
  rezrObj = setDFFromNames(rezrObj, entity, layer, df)
  rezrObj
}

getDFFromNames = function(rezrObj, entity, layer){
  if(layer == ""){
    df = rezrObj[[entity %+% "DF"]]
  } else {
    df = rezrObj[[entity %+% "DF"]][[layer]]
  }
  df
}

setDFFromNames = function(rezrObj, entity, layer, df){
  if(layer == ""){
    rezrObj[[entity %+% "DF"]] = df
  } else {
    rezrObj[[entity %+% "DF"]][[layer]] = df
  }
  rezrObj
}

validateACFieldLocalObj = function(rezrObj, entity, layer, fieldName, expression, type, fieldaccess, groupField){
  stopifnot("rezrObj" %in% class(rezrObj))
  stopifnot(is.character(entity))
  stopifnot(is.character(layer))
  stopifnot(is.character(fieldName))
  stopifnot(is.character(type))
  stopifnot(is.character(fieldaccess))
  stopifnot(is.character(groupField))
}

#' Easily add a field to / change a field in a rezrDF using information from another rezrDF
#'
#' @rdname acFieldForeign.rezrObj
#' @param rezrObj The Rezonator object that you will be changing.
#' @param targetEntity The Rezontor entity that you will be changing (e.g. token, unit, entry ...)
#' @param targetLayer The layer of the Rezonator entity that you will be changing from. Leave blank for entities without layers (i.e. token, entry and unit). If you don't have layers for other entities, type 'default'.
#' @param sourceEntity The Rezonator entity that you will be getting your information from.
#' @param sourceLayer The layer of the Rezonator entity that you will be getting your information from. Leave blank for entities without layers (i.e. token, entry and unit). If you don't have layers for other entities, type 'default'.
#' @inheritParams addFieldForeign.rezrDF
#'
#' @return
#' @export
addFieldForeign.rezrObj = function(rezrObj, targetEntity, targetLayer = "", sourceEntity, sourceLayer = "", targetForeignKeyName, targetFieldName = "", sourceFieldName = "", type = "simple", fieldaccess = "flex", complexAction = NULL){

  #A bunch of validation
  validateSimpleForeignObj(targetEntity, targetLayer, sourceEntity, sourceLayer, targetForeignKeyName, targetFieldName, sourceFieldName, type, fieldaccess, complexAction)
  if(sourceFieldName == "" & targetFieldName == ""){
    stop("You must provide either source or target field name.")
  } else if(sourceFieldName == ""){
    sourceFieldName = targetFieldName
  } else if(targetFieldName == ""){
    targetFieldName = sourceFieldName
  }
  oneArgs = c("sourceEntity", "sourceLayer", "targetEntity", "targetLayer", "targetFieldName", "sourceFieldName", "type", "fieldaccess", "complexAction")
  checkIfOne(oneArgs, "You can only add one field at a time with addFieldForeign.")

  #Getting info from the data
  sourceEntity = chompSuffix(sourceEntity, "DF")
  targetEntity = chompSuffix(targetEntity, "DF")
  if(sourceLayer != ""){
    sourceDFAddress = sourceEntity %+% "DF/" %+% sourceLayer
    sourceDF = rezrObj[[sourceEntity %+% "DF"]][[sourceLayer]]
  } else {
    sourceDFAddress = sourceEntity %+% "DF"
    sourceDF = rezrObj[[sourceEntity %+% "DF"]]
  }
  if(targetLayer != ""){
    targetDFAddress = targetEntity %+% "DF/" %+% targetLayer
    targetDF = rezrObj[[targetEntity %+% "DF"]][[targetLayer]]
  } else {
    targetDFAddress = targetEntity %+% "DF"
    targetDF = rezrObj[[targetEntity %+% "DF"]]
  }
  if(targetFieldName %in% names(targetDF)) stop("You cannot add a field with the same name as an existing field.")

  #Performing the actual actions
  if(type == "simple"){
    sourceKey = getKey(sourceDF)
    sourceDF = sourceDF %>% select(c(all_of(sourceKey), all_of(sourceFieldName)))
    byLine = character()
    byLine[targetForeignKeyName] = sourceKey
    result = suppressMessages(targetDF %>% rez_left_join(sourceDF, fieldName = sourceFieldName, fieldaccess = fieldaccess, by = byLine, df2Address = sourceDFAddress, fkey = targetForeignKeyName))

    #Rename if sourceFieldName != targetFieldName
    if(sourceFieldName != targetFieldName){
      if((sourceFieldName %+% "_lower") %in% names(result)){
        result = suppressMessages(result %>% rez_rename(!!targetFieldName := !!expr(sourceFieldName %+% "_lower")))
      } else if(sourceFieldName %in% names(result)){
        result = suppressMessages(result %>% rez_rename(!!targetFieldName := !!sourceFieldName))
      } else {
        stop("Mysterious error. Please contact package manager with the following information: In addFieldForeign, sourceFieldName and sourceFieldName %+% '_lower' both absent in result table.")
      }
    }

  } else if(type == "complex"){
    targetNodeMap = rezrObj[["nodeMap"]][[targetEntity]]
    targetNodeMapAddress = targetEntity

    result = suppressMessages(lowerToHigher(sourceDF, targetDF, complexNodeMap = targetNodeMap, fieldnames = sourceFieldName, higherFieldnames = targetFieldName, action = complexAction, tokenListName = targetForeignKeyName, fieldaccess = fieldaccess, complexNodeMapAddress = targetEntity, simpleDFAddress = sourceDFAddress))

  } else {
    stop("The only available types are simple and complex.")
  }

  #Putting the new DF in the rezrObj
  if(targetLayer != ""){
    rezrObj[[targetEntity %+% "DF"]][[targetLayer]] = result
  } else {
    rezrObj[[targetEntity %+% "DF"]] = result
  }

  rezrObj
}

#' @rdname acFieldForeign.rezrObj
#' @export
changeFieldForeign.rezrObj = function(rezrObj, targetEntity, targetLayer = "", sourceEntity, sourceLayer = "", targetForeignKeyName, targetFieldName = "", sourceFieldName = "", type = "simple", fieldaccess = "flex", complexAction = NULL){
  #A bunch of input validation
  validateSimpleForeignObj(targetEntity, targetLayer, sourceEntity, sourceLayer, targetForeignKeyName, targetFieldName, sourceFieldName, type, fieldaccess, complexAction)
  if(sourceFieldName == "" & targetFieldName == ""){
    stop("You must provide either source or target field name.")
  } else if(sourceFieldName == ""){
    sourceFieldName = targetFieldName
  } else if(targetFieldName == ""){
    targetFieldName = sourceFieldName
  }
  oneArgs = c("sourceEntity", "sourceLayer", "targetEntity", "targetLayer", "targetFieldName", "sourceFieldName", "type", "fieldaccess", "complexAction")
  checkIfOne(oneArgs, "You can only add one field at a time with changeFieldForeign.")

  #Getting data from the input
  sourceEntity = chompSuffix(sourceEntity, "DF")
  targetEntity = chompSuffix(targetEntity, "DF")
  if(sourceLayer != ""){
    sourceDFAddress = sourceEntity %+% "DF/" %+% sourceLayer
    sourceDF = rezrObj[[sourceEntity %+% "DF"]][[sourceLayer]]
  } else {
    sourceDFAddress = sourceEntity %+% "DF"
    sourceDF = rezrObj[[sourceEntity %+% "DF"]]
  }
  if(targetLayer != ""){
    targetDFAddress = targetEntity %+% "DF/" %+% targetLayer
    targetDF = rezrObj[[targetEntity %+% "DF"]][[targetLayer]]
    rezrObj[[targetEntity %+% "DF"]][[targetLayer]] = targetDF %>% rez_select(-targetFieldName)
  } else {
    targetDFAddress = targetEntity %+% "DF"
    targetDF = rezrObj[[targetEntity %+% "DF"]]
    rezrObj[[targetEntity %+% "DF"]] = targetDF %>% rez_select(-targetFieldName)
  }
  if(!(targetFieldName %in% names(targetDF))) stop("The field does not exist.")

  rezrObj = addFieldForeign(rezrObj, targetEntity, targetLayer, sourceEntity, sourceLayer, targetForeignKeyName, targetFieldName, sourceFieldName, type, fieldaccess, complexAction)
  rezrObj
}

validateSimpleForeignObj = function(targetEntity, targetLayer, sourceEntity, sourceLayer, targetForeignKeyName, targetFieldName, sourceFieldName, type, fieldaccess, complexAction){
  stopifnot(is.character(targetEntity))
  stopifnot(is.character(targetLayer))
  stopifnot(is.character(sourceEntity))
  stopifnot(is.character(sourceLayer))
  stopifnot(is.character(targetForeignKeyName))
  stopifnot(is.character(targetFieldName))
  stopifnot(is.character(sourceFieldName))
  stopifnot(is.character(type))
  stopifnot(is.character(fieldaccess))

  stopifnot(is.function(complexAction) | is.null(complexAction))

  if(type == "complex"){
    if(is.null(complexAction)){
      stop("Please specify an action for aggregating the source values if you choose type 'complex'.")
    }
  }
}


#' @rdname acField
#' @export
addField.rezrObj = function(rezrObj, ..., foreign = F){
  if(!foreign){
    addFieldLocal(rezrObj, ...)
  } else {
    addFieldForeign(rezrObj, ...)
  }
}

#' @rdname acField
#' @export
changeField.rezrObj = function(rezrObj, ..., foreign = F){
  if(!foreign){
    changeFieldLocal(rezrObj, ...)
  } else {
    changeFieldForeign(rezrObj, ...)
  }
}

addRow.rezrDF = function(rezrDF, ...){
  rez_add_row(rezrDF, ...)
}

#' Add rows to a certain data.frame
#'
#' @rdname addRow
#' @inheritParams addUnitSeq
#' @param layer The layer you would like to edit.
#' @param nodeMapArgs A list of fields to be added to the nodeMap only, not the DF.
#' @param ... Arguments to be passed on to [rezonateR::rez_add_row]
#'
#' @return A rezrObj with both the rezrDF and the associated nodeMap updated.
#' @export
addRow.rezrObj = function(rezrObj, entity, layer, nodeMapArgs = list(), ...){
  args = list(...)
  if(any(names(args) %in% c("df", ".data", ".before", ".after"))){
    args = args[-which(names(args) %in% c("df", ".data", ".before", ".after"))]
  }
  noNewRows = length(args[[1]])
  if(layer == ""){
    df = rezrObj[[entity %+% "DF"]]
    oldNRow = nrow(df)
    rezrObj[[entity %+% "DF"]] = rez_add_row(df, ..., layer = layer)
    rezrObj$nodeMap[[entity]] = rezrObj$nodeMap[[entity]] %>% c(assembleNodeFromDF(rezrObj[[entity %+% "DF"]], (oldNRow + 1):(oldNRow + noNewRows), addInfo = nodeMapArgs))
    rezrObj[[entity %+% "DF"]] = reload(rezrObj[[entity %+% "DF"]], rezrObj)
  } else {
    df = rezrObj[[entity %+% "DF"]][[layer]]
    oldNRow = nrow(df)
    rezrObj[[entity %+% "DF"]][[layer]] = rez_add_row(df, ..., layer = layer)
    rezrObj$nodeMap[[entity]] = rezrObj$nodeMap[[entity]] %>% c(assembleNodeFromDF(rezrObj[[entity %+% "DF"]][[layer]], (oldNRow + 1):(oldNRow + noNewRows), addInfo = nodeMapArgs))
    rezrObj[[entity %+% "DF"]][[layer]] = reload(rezrObj[[entity %+% "DF"]][[layer]], rezrObj)
  }

  rezrObj
}

#This file is for ease-of-use editing functions for rezrDF
#They have lots of validation and other ease-of-use features.
#Table of contents:
#1) Add a field using local information: addFieldLocal.rezrDF
#2) Add a field using foreign information: addFieldForeign.rezrDF
#3) A group of functions that may be used in addFieldForeign: concatenateAll, longest, longestLength, shortest, shortestLength
#5) Change a field using local information: changeFieldLocal.rezrDF
#6) Change a field using foreign information: changeFieldForeign.rezrDF

addFieldLocal.rezrObj = function(rezrObj, entity, layer, fieldName, expression, fieldaccess = "flex"){
  entity = chompSuffix(entity, "DF")

  validateACFieldLocalObj(rezrObj, entity, layer, fieldName, expression, fieldaccess)
  oneArgs = c("entity", "layer", "fieldName", "fieldaccess")
  checkIfOne(oneArgs, "You can only add one field at a time with addFieldLocal.")

  df = getDFFromNames(rezrObj, entity, layer)
  df = addFieldLocal(df, fieldName, !!enexpr(expression), fieldaccess)
  rezrObj = setDFFromNames(rezrObj, entity, layer, df)
  rezrObj
}

changeFieldLocal.rezrObj = function(rezrObj, entity, layer, fieldName, expression, fieldaccess = "flex"){
  entity = chompSuffix(entity, "DF")

  validateACFieldLocalObj(rezrObj, entity, layer, fieldName, expression, fieldaccess)
  oneArgs = c("entity", "layer", "fieldName", "fieldaccess")
  checkIfOne(oneArgs, "You can only change one field at a time with addFieldLocal.")

  df = getDFFromNames(rezrObj, entity, layer)
  df = changeFieldLocal(df, fieldName, !!enexpr(expression), fieldaccess)
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

validateACFieldLocalObj = function(rezrObj, entity, layer, fieldName, expression, fieldaccess){
  stopifnot("rezrObj" %in% class(rezrObj))
  stopifnot(is.character(entity))
  stopifnot(is.character(layer))
  stopifnot(is.character(fieldName))
  stopifnot(is.character(fieldaccess))
}

addFieldForeign.rezrObj = function(rezrObj, targetEntity, targetLayer = "", sourceEntity, sourceLayer = "", targetForeignKeyName, targetFieldName = "", sourceFieldName = "", type = "simple", fieldaccess = "flex", complexAction = NULL){

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

  oneArgs = c("sourceEntity", "sourceLayer", "targetEntity", "targetLayer", "targetFieldName", "sourceFieldName", "type", "fieldaccess", "complexAction")
  checkIfOne(oneArgs, "You can only add one field at a time with addFieldLocal.")

  #... blahblahblah
  if(type == "simple"){
    sourceKey = getKey(sourceDF)
    sourceDF = sourceDF %>% select(c(all_of(sourceKey), all_of(sourceFieldName)))
    byLine = character()
    byLine[targetForeignKeyName] = sourceKey
    result = suppressMessages(targetDF %>% rez_left_join(sourceDF, fieldName = sourceFieldName, fieldaccess = fieldaccess, by = byLine, df2Address = sourceDFAddress, fkey = targetForeignKeyName))

    #Rename if sourceFieldName != targetFieldName
    if(sourceFieldName != targetFieldName){
      if((sourceFieldName %+% "_lower") %in% names(result)){
        result = result %>% rez_rename(!!targetFieldName := !!expr(sourceFieldName %+% "_lower"))
      } else if(sourceFieldName %in% names(result)){
        result = result %>% rez_rename(!!targetFieldName := !!sourceFieldName)
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

  if(targetLayer != ""){
    rezrObj[[targetEntity %+% "DF"]][[targetLayer]] = result
  } else {
    rezrObj[[targetEntity %+% "DF"]] = result
  }

  print(updateFunct(rezrObj[[targetEntity %+% "DF"]], targetFieldName))


  rezrObj
}

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

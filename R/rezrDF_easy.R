#This file is for ease-of-use editing functions for rezrDF
#They have lots of validation and other ease-of-use features.
#Table of contents:
#1) Add a field using local information: addFieldLocal.rezrDF
#2) Add a field using foreign information: addFieldForeign.rezrDF
#3) A group of functions that may be used in addFieldForeign: concatenateAll, longest, longestLength, shortest, shortestLength
#5) Change a field using local information: changeFieldLocal.rezrDF
#6) Change a field using foreign information: changeFieldForeign.rezrDF

addFieldLocal.rezrDF = function(rezrDF, fieldName, expression, fieldaccess = "flex"){
  #Validation with differences with changeFieldLocal
  #(Validation in common with changeFieldLocal is checked in simpleMutate)
  currArgs = c("fieldName", "fieldaccess")
  checkIfOne(currArgs, "You can only add one field at a time.")

  if(fieldName %in% names(rezrDF)) stop("You cannot add a field with the same name as an existing field.")

  if(fieldaccess == "key"){
    warning("Are you sure you want to add a primary key field to the table? Compound key fields are currently not well supported.")
  } else if(fieldaccess == "foreign"){
    stop("addFieldLocal cannot add foreign fields. Please use addFieldForeign instead.")
  }
  expression = enexpr(expression)
  simpleMutate(rezrDF, fieldName, expression, fieldaccess)
}

#Internal function, for addFieldForeign and changeFieldForeign ONLY
validateSimpleForeign = function(targetDF, sourceDF, targetForeignKeyName, targetFieldName, sourceFieldName, type, fieldaccess, complexAction, targetNodeMap){
  stopifnot("rezrDF" %in% class(targetDF))
  stopifnot("rezrDF" %in% class(sourceDF))
  stopifnot(is.character(targetForeignKeyName))
  stopifnot(is.character(targetFieldName))
  stopifnot(is.character(sourceFieldName))
  stopifnot(is.character(type))
  stopifnot(is.character(fieldaccess))
  stopifnot(is.function(complexAction) | is.null(complexAction))
  stopifnot(is.list(targetNodeMap) | is.null(targetNodeMap))

  if(sourceFieldName == "" & targetFieldName == ""){
    stop("You must provide either source or target field name.")
  } else if(sourceFieldName == ""){
    sourceFieldName = targetFieldName
  } else if(targetFieldName == ""){
    targetFieldName = sourceFieldName
  }

}


addFieldForeign.rezrDF = function(targetDF, sourceDF, targetForeignKeyName, targetFieldName = "", sourceFieldName = "", type = "simple", fieldaccess = "flex", complexAction = NULL, targetNodeMap = NULL){
  #Validate input
  if(targetFieldName %in% names(targetDF)) stop("You cannot add a field with the same name as an existing field.")

  validateSimpleForeign(targetDF, sourceDF, targetForeignKeyName, targetFieldName, sourceFieldName, type, fieldaccess, complexAction, targetNodeMap)

  if(fieldaccess == "foreign"){
    warning("If you use addFieldForeign on a rezrDF, I cannot add an update function for you. Consider using addFieldForeign on a rezrObj instead.")
  } else if(fieldaccess == "auto"){
    stop("You shouldn't be creating an auto field with foreign data. Do you mean fieldaccess = foreign?")
  }

  if(type == "complex"){
    if(is.null(complexAction)){
      stop("Please specify an action for aggregating the source values if you choose type 'complex'.")
    }
    if(is.null(targetNodeMap)){
     stop("Please give me the nodeMap corresponding to the target in the targetNodeMap if you want to add a complex foreign field.")
    }
  }

  if(sourceFieldName == "" & targetFieldName == ""){
    stop("You must provide either source or target field name.")
  } else if(sourceFieldName == ""){
    sourceFieldName = targetFieldName
  } else if(targetFieldName == ""){
    targetFieldName = sourceFieldName
  }

  currArgs = c("targetForeignKeyName", "targetFieldName", "sourceFieldName", "type", "fieldaccess", "complexAction")
  checkIfOne(currArgs, "You can only add one field at a time.")

  #Perform actions
  if(type == "simple"){
    sourceKey = getKey(sourceDF)
    sourceDF = sourceDF %>% select(c(sourceKey, sourceFieldName))
    byLine = character()
    byLine[targetForeignKeyName] = sourceKey
    result = suppressMessages(targetDF %>% rez_left_join(sourceDF, fieldName = sourceFieldName, fieldaccess = fieldaccess, by = byLine))

    #Rename if sourceFieldName != targetFieldName
    if(sourceFieldName != targetFieldName){
      if((sourceFieldName %+% "_lower") %in% names(result)){
        result = result %>% rename(!!targetFieldName := !!expr(sourceFieldName %+% "_lower"))
      } else if(sourceFieldName %in% names(result)){
        result = result %>% rename(!!targetFieldName := !!sourceFieldName)
      } else {
        stop("Mysterious error. Please contact package manager with the following information: In addFieldForeign, sourceFieldName and sourceFieldName %+% '_lower' both absent in result table.")
      }
    }
  } else if(type == "complex"){
    result = suppressMessages(lowerToHigher(sourceDF, targetDF, complexNodeMap = targetNodeMap, fieldnames = sourceFieldName, higherFieldnames = targetFieldName, action = complexAction, tokenListName = targetForeignKeyName, fieldaccess = fieldaccess))
  } else {
    stop("The only available types are simple and complex.")
  }

  result
}

concatenateAll = function(x){
  x = sapply(x, function(y) if(is.na(y)) "" else y)
  paste(x, collapse = "")
}

longestLength = function(x){
  max(nchar(x), na.rm = T)
}

longest = function(x){
  x[nchar(x) == max(nchar(x), na.rm = T)]
}

shortestLength = function(x){
  min(nchar(x), na.rm = T)
}

shortest = function(x){
  x[nchar(x) == max(nchar(x), na.rm = T)]
}

addField.rezrDF = function(rezrDF, ..., foreign = F){
  if(!foreign){
    addFieldLocal(rezrDF, ...)
  } else {
    addFieldForeign(rezrDF, ...)
  }
}

changeFieldLocal.rezrDF = function(rezrDF, fieldName, expression, fieldaccess = ""){
  currArgs = c("fieldName", "fieldaccess")
  checkIfOne(currArgs, "You can only change one field at a time.")

  if(fieldaccess == "key"){
    warning("Are you sure you want to change a field to a primary key? Compound key fields are currently not well supported.")
  } else if(fieldaccess == "foreign"){
    stop("changeFieldLocal cannot turn a field foreign or modify fields using foreign information. Please use changeFieldForeign instead.")
  }

  #Change field access first
  fieldaccess(rezrDF, fieldName) = fieldaccess
  rez_validate_fieldchange(rezrDF, fieldName, changingStatus = T)

  #Then change the actual field
  simpleMutate(rezrDF, fieldName, enexpr(expression), fieldaccess)
}

#Purely internal function for commnalities between changeFieldLocal and addFieldLocal
simpleMutate = function(rezrDF, fieldName, enexpression, fieldaccess){
  stopifnot("rezrDF" %in% class(rezrDF))
  stopifnot(is.character(fieldName))
  stopifnot(is.character(fieldaccess))

  result = suppressWarnings(rez_mutate(rezrDF, !!fieldName := !!enexpression, fieldaccess = fieldaccess))

  if(fieldaccess == "auto"){
    updateFunct(result, fieldName) = createUpdateFunction(!!fieldName, !!enexpression, df)
  }

  result
}

changeFieldForeign.rezrDF = function(targetDF, sourceDF, targetForeignKeyName, targetFieldName = "", sourceFieldName = "", type = "simple", fieldaccess = "flex", complexAction = NULL, targetNodeMap = NULL){
  #Validate input
  if(!(targetFieldName %in% names(targetDF))) stop("Target field not found.")

  validateSimpleForeign(targetDF, sourceDF, targetForeignKeyName, targetFieldName, sourceFieldName, type, fieldaccess, complexAction, targetNodeMap)

  if(fieldaccess == "foreign"){
    warning("If you use changeFieldForeign on a rezrDF, I cannot add an update function for you. Consider using changeFieldForeign on a rezrObj instead.")
  } else if(fieldaccess == "auto"){
    stop("You shouldn't be changing an auto field with foreign data. Do you mean fieldaccess = foreign?")
  }

  if(type == "complex"){
    if(is.null(complexAction)){
      stop("Please specify an action for aggregating the source values if you choose type 'complex'.")
    }
    if(is.null(targetNodeMap)){
      stop("Please give me the nodeMap corresponding to the target in the targetNodeMap if you want to add a complex foreign field.")
    }
  }

  currArgs = c("targetForeignKeyName", "targetFieldName", "sourceFieldName", "type", "fieldaccess", "complexAction")
  checkIfOne(currArgs, "You can only change one field at a time.")

  oldNames = names(targetDF)
  suppressWarnings(targetDF %>% select(-!!targetFieldName) %>% addFieldForeign(sourceDF, targetForeignKeyName, targetFieldName, sourceFieldName, type, fieldaccess, complexAction, targetNodeMap) %>% select(all_of(oldNames)))

}


a = function(field, e){
  e = enexpr(e)
  df %>% mutate(!!field := !!e)
}


changeField.rezrDF = function(rezrDF, ..., foreign = F){
  if(!foreign){
    changeFieldLocal(rezrDF, ...)
  } else {
    changeFieldForeign(rezrDF, ...)
  }
}

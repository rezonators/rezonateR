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

  print(rezrDF)
  print(fieldName)
  print(fieldaccess)
  print(enexpr(expression))
  simpleMutate(rezrDF, fieldName, enexpr(expression), fieldaccess)
}


addFieldForeign.rezrDF = function(targetDF, sourceDF, targetForeignKeyName, targetFieldName = "", sourceFieldName = "", type = "simple", fieldaccess = "flex", complexAction = NULL, targetNodeMap = NULL){
  if(targetFieldName %in% names(targetDF)) stop("You cannot add a field with the same name as an existing field.")

  stopifnot("rezrDF" %in% class(targetDF))
  stopifnot("rezrDF" %in% class(sourceDF))
  stopifnot(is.character(targetForeignKeyName))
  stopifnot(is.character(targetFieldName))
  stopifnot(is.character(sourceFieldName))
  stopifnot(is.character(type))
  stopifnot(is.character(fieldaccess))
  stopifnot(is.function(complexAction))
  stopifnot(is.list(targetNodeMap))

  #Validate input
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
    stop("You must provide either source or target field name.")
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
    result = lowerToHigher(sourceDF, targetDF, complexNodeMap = targetNodeMap, fieldnames = sourceFieldName, higherFieldnames = targetFieldName, action = complexAction, tokenListName = targetForeignKeyName, fieldaccess = fieldaccess)
  } else {
    stop("The only availble types are simple and complex.")
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


a = function(field, e){
  e = enexpr(e)
  df %>% mutate(!!field := !!e)
}

checkIfOne = function(args, message){
  for (arg in args){
    e = env_parent(environment())
    if(length(e[[arg]]) > 1){
      warning(message %+% "I am taking the first of the following field: " %+% arg)
      e[[arg]] = e[[arg]][1]
    }
  }
}


changeField.rezrDF = function(rezrDF, ..., foreign = F){
  if(!foreign){
    changeFieldLocal(rezrDF, ...)
  } else {
    changeFieldForeign(rezrDF, ...)
  }
}

#This file is for ease-of-use editing functions for rezrDF
#They have lots of validation and other ease-of-use features.
#Table of contents:
#1) Add a field using local information: addFieldLocal.rezrDF
#2) Add a field using foreign information: addFieldForeign.rezrDF
#3) Change a field using local information: changeFieldLocal.rezrDF
#4) Change a field using foreign information: changeFieldForeign.rezrDF

addLocalField.rezrDF = function(rezrDF, fieldName, expression, fieldaccess = "flex"){
  stopifnot("rezrDF" %in% class(rezrDF))
  stopifnot(is.character(fieldName))
  stopifnot(is.character(fieldaccess))

  currArgs = c("rezrDF", "fieldName", "fieldaccess")
  for (arg in currArgs){
    e = environment()
    if(length(e[[arg]]) > 1){
      warning("Only one complex action is allowed. I am taking the first of the following field: " %+% arg)
      e[[arg]] = e[[arg]][1]
    }
  }

  if(fieldName %in% names(rezrDF)) stop("You cannot add a field with the same name as an existing field.")

  if(fieldaccess == "key"){
    warning("Are you sure you want to add a primary key field to the table? Compound key fields are currently not well supported.")
  } else if(fieldaccess == "foreign"){
    stop("addLocalField cannot add foreign fields. Please use addForeignField instead.")
  }

  rez_validate_fieldchange(fieldName)

  enexpression = enexpr(expression)

  result = suppresswarnings(rez_mutate(rezrDF, !!fieldName := !!enexpression, fieldaccess = fieldaccess))
  if(fieldaccess == "auto"){
    updateFunct(result, fieldName) = createUpdateFunction(!!fieldName, !!enexpression, df)
  }

  result
}


addForeignField.rezrDF = function(targetDF, sourceDF, targetForeignKeyName, targetFieldName = "", sourceFieldName = "", type = "simple", fieldaccess = "flex", complexAction = NULL, targetNodeMap = NULL){
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
    warning("If you use addForeignField on a rezrDF, I cannot add an update function for you. Consider using addForeignField on a rezrObj instead.")
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
  for (arg in currArgs){
    e = environment()
    if(length(e[[arg]]) > 1){
      warning("Only one of each field is allowed. I am taking the first of the following field: " %+% arg)
      e[[arg]] = e[[arg]][1]
    }
  }

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
        stop("Mysterious error. Please contact package manager with the following information: In addForeignField, sourceFieldName and sourceFieldName %+% '_lower' both absent in result table.")
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
    addLocalField(rezrDF, ...)
  } else {
    addForeignField(rezrDF, ...)
  }
}

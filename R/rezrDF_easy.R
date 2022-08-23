#This file is for ease-of-use editing functions for rezrDF
#They have lots of validation and other ease-of-use features.
#Table of contents:
#1) Add/change a field using local information: addFieldLocal.rezrDF, changeFieldLocal.rezrDF
#  a) Purely internal functions to support the two: simpleMutate
#2) Add/change a field using foreign information: addFieldForeign.rezrDF
#  b) Purely internal functions to support the two: validateSimpleForeign
#3) A group of functions that may be used in addFieldForeign: concatenateAll, longest, longestLength, shortest, shortestLength
#4) Shortcut functions: addField.rezrDF, changeField.rezrDF

#' Easily add a field to / change a field in a rezrDF using only information from that rezrDF
#'
#' @rdname acFieldLocal
#' @param rezrDF The rezrDF object you would like to change.
#'
#' @return When used on a rezrDF, the rezrDF with the new or changed field.
#' @note changeFieldLocal and changeFieldForeign will set the field access status of the changed fields to 'flex' by default, even if they are originally auto or foreign. Please specify fieldaccess = 'auto' or 'foreign' if you want the changed fields to change to or maintain these statuses. (This is to ensure you are aware of the fact that you are changing the update function in these cases.) changeField does not support changing an auto or foreign field's value without changing its field access status or update function, as this is generally a mistake; if you must do it, use rez_mutate.')
#' @export
addFieldLocal.rezrDF = function(rezrDF, fieldName, expression, type = "simple", fieldaccess = "flex", groupField = ""){
  #Validation with differences with changeFieldLocal
  #(Validation in common with changeFieldLocal is checked in simpleMutate / complexMutate)
  currArgs = c("fieldName", "fieldaccess")
  checkIfOne(currArgs, "You can only add one field at a time.")

  if(fieldName %in% names(rezrDF)) stop("You cannot add a field with the same name as an existing field.")

  if(fieldaccess == "key"){
    warning("Are you sure you want to add a primary key field to the table? Compound key fields are currently not well supported.")
  } else if(fieldaccess == "foreign"){
    stop("addFieldLocal cannot add foreign fields. Please use addFieldForeign instead.")
  }
  expression = enexpr(expression)
  localMutate(rezrDF, fieldName, expression, type, fieldaccess, groupField)
}

#' @rdname acFieldLocal
#' @export
changeFieldLocal.rezrDF = function(rezrDF, fieldName, expression, type = "simple", fieldaccess = "", groupField =""){
  currArgs = c("fieldName", "fieldaccess")
  checkIfOne(currArgs, "You can only change one field at a time.")

  if(fieldaccess == "key"){
    stop("You cannot change a field to a primary key.")
  } else if(fieldaccess == "foreign"){
    stop("changeFieldLocal cannot turn a field foreign or modify fields using foreign information. Please use changeFieldForeign instead.")
  }

  #Change field access first
  rez_validate_fieldchange(rezrDF, fieldName, changingStatus = T, fieldaccess = fieldaccess)
  fieldaccess(rezrDF, fieldName) = fieldaccess

  #Then change the actual field
  localMutate(rezrDF, fieldName, enexpr(expression), type, fieldaccess, groupField)
}

#Purely internal function for commonalities between changeFieldLocal and addFieldLocal
localMutate = function(rezrDF, fieldName, enexpression, type, fieldaccess, groupField){
  stopifnot("rezrDF" %in% class(rezrDF))
  stopifnot(is.character(fieldName))
  stopifnot(is.character(fieldaccess))
  stopifnot(is.character(groupField))
  stopifnot(is.character(type))
  if(type == "complex" & groupField == ""){
    stop("Please specify a groupField if you chose type = 'complex'.")
  }


  if(type == "simple"){
    result = rez_mutate(rezrDF, !!fieldName := !!enexpression, fieldaccess = fieldaccess)
  } else if(type == "complex"){
    #result = (rezrDF %>% rez_group_by(!!groupField) %>% rez_mutate(!!fieldName := !!enexpression, fieldaccess = fieldaccess) %>% rez_ungroup)
    result = rezrDF %>% rez_group_by(!!parse_expr(groupField))
    result = result %>% rez_mutate(!!fieldName := !!enexpression, fieldaccess = fieldaccess)
    result = result %>% rez_ungroup
  } else {
    stop("Only type options are 'simple' and 'complex'.")
  }

  if(fieldaccess == "auto"){
    if(type == "simple"){
      updateFunct(result, fieldName) = createUpdateFunction(!!fieldName, !!enexpression, df)
    } else {
      updateFunct(result, fieldName) = createUpdateFunction(!!fieldName, !!enexpression, df, groupField)
    }
  }

  result
}


#' @rdname acFieldForeign
#' @param targetDF The rezrDF you will be changing.
#' @param sourceDF The rezrDF you will be getting information from.
#'
#' @return When applied on a rezrDF, a rezrDF object with the added / changed field.
#' @export
addFieldForeign.rezrDF = function(targetDF, sourceDF, targetForeignKeyName, targetFieldName = "", sourceFieldName = "", type = "simple", fieldaccess = "flex", complexAction = NULL, targetNodeMap = NULL){
  #Validate input
  if(targetFieldName %in% names(targetDF)) stop("You cannot add a field with the same name as an existing field.")

  if(sourceFieldName == "" & targetFieldName == ""){
    stop("You must provide either source or target field name.")
  } else if(sourceFieldName == ""){
    sourceFieldName = targetFieldName
  } else if(targetFieldName == ""){
    targetFieldName = sourceFieldName
  }

  validateSimpleForeign(targetDF, sourceDF, targetForeignKeyName, targetFieldName, sourceFieldName, type, fieldaccess, complexAction, targetNodeMap)

  if(fieldaccess == "foreign"){
    warning("If you use addFieldForeign on a rezrDF, I cannot add an update function for you. Consider using addFieldForeign on a rezrObj instead.")
  } else if(fieldaccess == "auto"){
    stop("You shouldn't be creating an auto field with foreign data. Do you mean fieldaccess = foreign?")
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
        result = result %>% rez_rename(!!targetFieldName := !!expr(sourceFieldName %+% "_lower"))
      } else if(sourceFieldName %in% names(result)){
        result = result %>% rez_rename(!!targetFieldName := !!sourceFieldName)
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


#' @rdname acFieldForeign
#' @export
changeFieldForeign.rezrDF = function(targetDF, sourceDF, targetForeignKeyName, targetFieldName = "", sourceFieldName = "", type = "simple", fieldaccess = "flex", complexAction = NULL, targetNodeMap = NULL){
  #Validate input
  if(!(targetFieldName %in% names(targetDF))) stop("Target field not found.")

  if(sourceFieldName == "" & targetFieldName == ""){
    stop("You must provide either source or target field name.")
  } else if(sourceFieldName == ""){
    sourceFieldName = targetFieldName
  } else if(targetFieldName == ""){
    targetFieldName = sourceFieldName
  }

  validateSimpleForeign(targetDF, sourceDF, targetForeignKeyName, targetFieldName, sourceFieldName, type, fieldaccess, complexAction, targetNodeMap)

  if(fieldaccess == "foreign"){
    warning("If you use changeFieldForeign on a rezrDF, I cannot add an update function for you. Consider using changeFieldForeign on a rezrObj instead.")
  } else if(fieldaccess == "auto"){
    stop("You shouldn't be changing an auto field with foreign data. Do you mean fieldaccess = foreign?")
  }

  currArgs = c("targetForeignKeyName", "targetFieldName", "sourceFieldName", "type", "fieldaccess", "complexAction")
  checkIfOne(currArgs, "You can only change one field at a time.")

  oldNames = names(targetDF)
  suppressWarnings(targetDF %>% rez_select(-!!targetFieldName) %>% addFieldForeign(sourceDF, targetForeignKeyName, targetFieldName, sourceFieldName, type, fieldaccess, complexAction, targetNodeMap) %>% select(all_of(oldNames)))
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

  if(type == "complex"){
    if(is.null(complexAction)){
      stop("Please specify an action for aggregating the source values if you choose type 'complex'.")
    }
    if(is.null(targetNodeMap)){
      stop("Please give me the nodeMap corresponding to the target in the targetNodeMap if you want to add a complex foreign field.")
    }
  }

}


#' Functions for getting summary information about a vector of strings.
#'
#' These functions may be used for the complexAction field in [rezonateR::addFieldForeign] / [rezonateR::changeFieldForeign]], or within expressions in functions like [rezonateR::addField].
#'
#' @rdname complexActions
#' @param x The information from the source rezrDF.
#' @param isWord Name of the column that determines whether a token is a word or not.
#' @note concatenateAll concatenates everything together. It is not to be confused with [rezonateR::concatStringFields], which is applied on dataFrames. longest and shortest give the longest and shortest strings, and may have multiple entries if there are ties. longestLength and shortestLength give the lengths of the longest and shortest strings in x. Some base R functions that may be used include max, min, mean, range, etc.
#'
#' @note Remember to include only the function name in complexAction fields, and include the 'x' (normally the name of a column inside your rezrDF) in expression fields.
#'
#' @export
concatenateAll = function(x){
  x = sapply(x, function(y) if(is.na(y)) "" else y)
  paste(x, collapse = "")
}

#' @rdname complexActions
#' @export
longestLength = function(x, isWord = T){
  max(nchar(x[isWord]), na.rm = T)
}

#' @rdname complexActions
#' @export
longest = function(x, isWord = T){
  xWord = x[isWord]
  xWord[nchar(xWord) == max(nchar(xWord), na.rm = T)][1]
}

#' @rdname complexActions
#' @export
shortestLength = function(x, isWord = T){
  min(nchar(x[isWord]), na.rm = T)
}

#' @rdname complexActions
#' @export
shortest = function(x, isWord = T){
  xWord = x[isWord]
  xWord[nchar(xWord) == max(nchar(xWord), na.rm = T)][1]
}

#' @rdname complexActions
#' @export
inLength = function(x, isWord = T){
  length(x[isWord])
}


#' Functions for getting information on whether something is located at the beginning or end of a larger structure.
#'
#' @rdname initfin
#' @inheritparams complexActions
#' @param seq Name of the column containing the sequence value to be taken into account.
#'
#'
#' @export
isInitial = function(seq){
  as.integer(seq) == 1
}

#' @rdname initfin
#' @param length Name of the column containing the maximum sequence value.
#' @export
isFinal = function(seq, length){
  as.integer(seq) == length
}


#' Shortcut functions for functions beginning with addField and changeField
#'
#' Mostly a shortcut for [rezonateR::addFieldLocal] and [rezonateR::changeFieldLocal].
#'
#' @rdname acField
#' @param rezrDF The rezrDF to be modified.
#' @param ... Arguments to be passed to functions such as those in see [rezonateR::addFieldLocal.rezrDF], [rezonateR::addFieldForeign.rezrDF], [rezonateR::addFieldLocal.rezrObj] and [rezonateR::addFieldForeign.rezrObj].
#' @param foreign If TRUE, you are adding a foreign field. Normally set to false; you can just call [rezonateR::addFieldForeign.rezrDF] / [rezonateR::addFieldForeign.rezrObj] for foreign fields.
#'
#' @return The rezrDF or rezrObj being modified.
#' @export
addField.rezrDF = function(rezrDF, ..., foreign = F){
  if(!foreign){
    addFieldLocal(rezrDF, ...)
  } else {
    addFieldForeign(rezrDF, ...)
  }
}

#' @rdname acField
#' @export
changeField.rezrDF = function(rezrDF, ..., foreign = F){
  if(!foreign){
    changeFieldLocal(rezrDF, ...)
  } else {
    changeFieldForeign(rezrDF, ...)
  }
}


#' Remove a field from a rezrDF.
#'
#' @rdname rmField
#' @inheritparams acField
#' @param fields Vector of column names to be removed.
#'
#' @return A rezrDF with the specified fields removed.
#' @export
#'
#' @examples
removeField.rezrDF = function(rezrDF, fields){
  rezrDF %>% rez_select(!fields)
}

#' Merging and renaming categories
#'
#' Functions for merging and renaming categories in character or factor columns.
#'
#' @rdname mergecat
#' @param x A column or vector whose categories are to be merged or renamed.
#' @param ... The name of each argument is a new category, and the value of each argument is a vector of names of old categories (as character values, even if the original column/vector contains factors).
#' @param asFactor Do you want the result to be a factor?
#' @param levels If asFactor = T, you an use this to set the levels of the factor.
#'
#' @return A column or vector with the desired changes.
#' @export
mergeCats = function(x, ..., asFactor = F, levels = NULL){
  x = as.character(x)
  cats = unique(x)
  mappings = list(...)
  oldCats = unlist(mappings)
  if(length(oldCats) > length(unique(oldCats))){
    stop("Each old category can only correspond to one new category.")
  }
  newCats = rep(names(mappings), sapply(mappings, length))
  result = "case_when(" %+% paste0("x == '", oldCats, "' ~ '", newCats, collapse = "', ") %+% "', T ~ x)" %>% parse_expr() %>% eval_tidy()
  if(is.null(levels)) levels = unique(result)
  if(asFactor) result = factor(result, levels = levels)
  result
}

#' @rdname mergecat
#' @export
renameCats = function(x, ..., asFactor = F, levels = NULL){
  mergeCats(x, ..., asFactor, levels)
}

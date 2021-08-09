#A group of functions for handling the modification of Rez data structures.
#At the moment, it handles the data frames only. At one point, the node maps will also be handled through this interface.

#' Extract/set an attribute of a rezrDF.
#'
#' Extract an attribute of a data.frame (inNodeMap, fieldaccess, updateFunct), or set it.
#'
#' @rdname getRezrDFAttr
#' @param df The data.frame whose field access attributes you want to see.
#' @param attr The attribute that you want to extract.
#' @param fields The field whose access attribute you want to see. If left blank, a vector containing all the attributes is output.
#' @param value The value you want to set it to.
#'
#' @return The value of the attribute you want to extract (for extract functions), and the DF with the value set (for set functions).
#' @export
getRezrDFAttr = function(df, attr, fields = ""){
  if(all(fields == "")){
    attr(df, attr, fields)
  } else {
    if(length(fields) == 1){
      attr(df, attr, fields)[[fields]]
    } else {
      attr(df, attr, fields)[fields]
    }
  }
}

#' @rdname getRezrDFAttr
#' @export
fieldaccess = function(df, fields = "") getRezrDFAttr(df, "fieldaccess", fields)

#' @rdname getRezrDFAttr
#' @export
updateFunct = function(df, fields = "") getRezrDFAttr(df, "updateFunct", fields)

#' @rdname getRezrDFAttr
#' @export
inNodeMap = function(df, fields = "") getRezrDFAttr(df, "inNodeMap", fields)

#' @rdname getRezrDFAttr
#' @export
setRezrDFAttr = function(df, attr, fields = "", value){
  if(all(fields == "")){
    if(!setequal(colnames(df), names(value))){
      if(attr != "updateFunct" | length(setdiff(names(value), colnames(df))) != 0)
      warning("The data.frame does not have the same field as the value vector that you are giving me. Please check that the value vector is correct, or specifiy the fields you want to set. " %+% "Data frame column names: " %+% paste(colnames(df), collapse = ", ") %+% "; value vector names: " %+% paste(colnames(df), collapse = ", ") %+% ".")
    }
    attr(df, attr) = value
  } else {
    if(length(fields) == 1){
      attr(df, attr)[[fields]] = value
    } else {
      attr(df, attr)[fields] = value
    }
  }
  df
}

#' @rdname getRezrDFAttr
#' @export
`updateFunct<-` = function(df, fields = "", value) setRezrDFAttr(df, "updateFunct", fields, value)

#' @rdname getRezrDFAttr
#' @export
`fieldaccess<-` = function(df, fields = "", value) setRezrDFAttr(df, "fieldaccess", fields, value)

#' @rdname getRezrDFAttr
#' @export
`inNodeMap<-` = function(df, fields = "", value) setRezrDFAttr(df, "inNodeMap", fields, value)


#The rez_ data.frame functions
#' Perform an operation on a data.frame while updating field access status.
#'
#' This is a wrapper for performing data frame manipulation functions (such as dplyr functions) while also updating field access status. This function only needs to be directly called by advanced users; beginning users may stick to function such as rez_mutate and rez_left_join, which are much more simple and intuitive to use.
#'
#' @param df The data frame to be modified.
#' @param fieldaccess The field access status of the field you're addding, either a single character (to apply to all of the new fields) or a vector of characters for each new field. Note that if you are both modifying and adding fields, only the added fields will have access values changed. So if you're specifying an entire vector of field access values, the best practice in using this function is to separate new-field and added-field mutates, otherwise the code will be difficult to read.
#' @param .f The function to be performed.
#' @param ... Argument to be passed to .f.
#'
#' @return resultDF
#' @export
rez_dfop = function(df, .f, ..., fieldaccess = "flex", updateFunct = NA, oldNames = ""){
  if(all(oldNames == "")){
    oldNames = colnames(df)
  }
  resultDF = .f(df, ...)
  newNames = colnames(resultDF)
  #Find which columns (if any) are new
  addedNames = setdiff(newNames, oldNames)
  #Set the column to the given fieldaccess value
  fieldaccess(resultDF, addedNames) = fieldaccess
  inNodeMap(resultDF, addedNames) = "no"
  if(!is.na(updateFunct)){
    updateFunct(resultDF) = c(updateFunct(resultDF), updateFunct)
  }
  resultDF
}


#' Validate a field change.
#'
#' This function ensures that the fields in a DF you wish to change are actually a good idea to change. It produces an error if a primary key is among the fields you wish to change, and warnings otherwise. This is automatically called if you use a rezonateR-internal function such as rez_mutate.
#'
#' @param df The rezrDF you are planning to change.
#' @param changedFields The fields you want to change.
#'
#' @export
rez_validate_fieldchange = function(df, changedFields){
  for(entry in changedFields){
    if(entry %in% names(fieldaccess(df))){
      if(fieldaccess(df)[entry] == "key"){
        stop("You cannot change a primary key: " %+% entry)
      } else if(fieldaccess(df)[entry] == "fkey"){
        warning("Note that you are changing a foreign key " %+% entry %+% ". This should only be used for changing association links between tables, and may break thing down the road.")
      } else if(fieldaccess(df)[entry] == "auto"){
        warning("Note that you are changing a field " %+% entry %+% "that is automatically updated. Your change is likely to be overridden by a future update.")
      } else if(fieldaccess(df)[entry] == "foreign"){
        warning("Note that you are changing a field " %+% entry %+% "that depends on another data.frame. Your change is likely to be overridden by a future update on the data.frame that this data.frame depends on.")
      }
    }
  }

}

#' Mutate a Rez data.frame and change field access status.
#'
#' This is a wrapper for performing mutations on Rez data.frames, including validation to ensure you do not change anything you shouldn't change. It *only* changes the data frame, such as by changing field access data, at the moment. If your desired fieldaccess value is flex, this may serve as a drop-in replacement for mutate. Note that apart from the data.frame to be modified, all arguments of mutate must be named.
#' Currently does not work with across, so you will have to stick to mutate for that. You may validate the fields beforehand using rez_validate_fieldchange.
#'
#' @param df The data frame to be modified.
#' @param fieldaccess The field access status of the field you're addding, either a single character (to apply to all of the new fields) or a vector of characters for each new field. Note that if you are both modifying and adding fields, only the added fields will have access values changed. So if you're specifying an entire vector of field access values, the best practice in using this function is to separate new-field and added-field mutates, otherwise the code will be difficult to read.
#' @param ... Other functions passed onto mutate, i.e. the columns you will be changing or adding.
#'
#' @return resultDF
rez_mutate = function(df, ..., fieldaccess = "flex"){

  #Validation
  #If it's dynamic, list should throw an error
  if("try-error" %in% class(try(list(...), silent=TRUE))){
    if("try-error" %in% class(try(expr(...), silent=TRUE))){
      #TODO: Validate these cases
      changedFields = character()
      message("Could not figure out a way to validate the current mutate. Please ensure you're not touching any fields you shouldn't touch ...")
    } else if(any(str_detect(as.character(expr(...)), ":="))){
      #For dynamically specified field
      quoted_field = str_extract(as.character(expr(...)), "\\\".+\\\"")
      changedFields = substr(quoted_field, 2, nchar(quoted_field) - 1)
    }
  } else {
    #For statically specified fields
    changedFields = names(list(...))
  }
  rez_validate_fieldchange(df, changedFields)

  #TODO: Automatically create update function for the user if 'auto' is specified.

  rez_dfop(df, mutate, fieldaccess = fieldaccess, ...)
}

#' Perform a left join on two rez data.frames and change field access status.
#'
#' This is a wrapper for performing left joins on Rez data.frames. It *only* changes the data frame, such as by changing field access data, at the moment. If your desired fieldaccess value is flex, this may serve as a drop-in replacement for mutate. Note that apart from the data.frame to be modified and the data.frame you are joining from, all arguments of left_join must be named.
#' By default, if no suffix is specified, the suffixes are c("", "_lower"). That is, if you are joining two data.frames, both with a column called 'name', then the left data.frame's column will still be called 'name' in the new data.frame but the right data.frame's column will get called 'name_lower'.
#'
#' @param df1 The left data.frame.
#' @param df2 The right data.frame.
#' @param fieldaccess The field access status of the field you're addding, either a single character (to apply to all of the new fields) or a vector of characters for each new field. Note that if you are both modifying and adding fields, only the added fields will have access values changed. So if you're specifying an entire vector of field access values, the best practice in using this function is to separate new-field and added-field mutates, otherwise the code will be difficult to read.
#' @param ... Other functions passed onto left_join, i.e. the columns you will be changing or adding.
#'
#' @return resultDF
rez_left_join = function(df1, df2 = NULL, ..., fieldaccess = "foreign", address = "", fkey = "", rezrObj = NULL){
  #TODO: No need to specify df2 if address is provided

  oldNames = colnames(df1)

  updateFunction = NA

  if(fieldaccess == "foreign"){
    updateFunction = NA
  } else if(fieldaccess == "auto"){
    warning("You are creating new fields using a left_join, but specified field access as 'auto'. I cannot automatically attach an update function for you; please set update function manually using set_updateFunct. Alternatively, set fieldaccess to foreign.")
  }

  suffixIncl = F
  if(!("try-error" %in% class(try(list(...), silent=TRUE)))){
    if("suffix" %in% names(list(...))){
      suffixIncl = T
    }
  }

  #Figuring out missing info from other info:
  #a) Figuring out the DF from the address
  df2Add = strsplit(address, "/")[[1]]
  if(is.null(df2)){
    if(!is.null(rezrObj) & !(all(address == ""))){
      df2 = listAt(rezrObj, df2Add)
    } else{
      stop("You need to specify either a right data.frame, or an address with a rezrObj.")
    }
  }

  #b) Figuring out foreign key from the by-line
  autoBy = character(0)
  if(is.null(list(...)[["by"]])){
    if(!all(fkey == "")){
      i = 1
      for(key in fkey){
        autoBy[key] = names(fieldaccess(df2)[fieldaccess(df2) == "key"])[i]
        i = i + 1
        message("You didn't give me a by-line for the left join. So I figured it out for you from your key information.")
      }
    }
  }

  #c) Figuring out by-line from the foreign key
  if(all(fkey == "")){
    if(!is.null(list(...)[["by"]])){
      fkey = names(list(...)[["by"]])[1]
      message("You didn't give me a foreign key for future updates, so I'm assuming it's the first of your by-fields.")
    }
  }

  if(!suffixIncl){
    result = rez_dfop(df1, left_join, fieldaccess = fieldaccess, updateFunct = updateFunction, df2, suffix = c("", "_lower"), ...)

    #For the update function
    newNames = setdiff(names(result), oldNames)
    rightTblNames = chompSuffix(newNames, "_lower")
  } else {
    #We need to specify oldNames to rez_dfop because the names of fields in df1 may be changed.
    leftSuffix = list(...)[["suffix"]][1]
    oldNames = unique(c(oldNames, paste0(oldNames, leftSuffix)))
    result = rez_dfop(df1, left_join, fieldaccess = fieldaccess, updateFunct = updateFunction, oldNames = oldNames, df2, ...)

    #For the update function
    newNames = setdiff(names(result), oldNames)
    rightTblNames = chompSuffix(newNames, list(...)[["suffix"]][2])
  }

  #Update function stuff
  if(all(address == "") | all(fkey == "")){
    message("You didn't provide an address and/or foreign key, so I won't be adding an update function automatically. Nuh-uh!")
  } else {
    for(i in 1:length(newNames)){
        newName = newNames[i]
        rightTblName = rightTblNames[i]
        updateFunct(result, newName) = createLeftJoinUpdate(address %+% "/" %+% rightTblName, fkey, newName)
    }
  }

  result
}

rez_group_split = function(df, ...){
  split = df %>% group_split(...)
  result = list()
  for(i in 1:length(split)){
    result[[i]] = new_rezrDF(split[[i]], fieldaccess(df), updateFunct(df), inNodeMap(df))
  }
  result
}

#' Functions for reloading auto and foreign fields.
#'
#' @rdname reload
#' @param df The rezrDF you want to modify.
#' @param rezrObj The entire rezrObj.
#' @param fields The fields of the rezrDF you want to modify.
#'
#' @return The modified rezrDF. Foreign fields are always updated before auto fields. If the fields parameter is not available, I will find the best update order; otherwise, I will use the order you provide except for the rule where foreign fields are updated first.
#' @export
reload = function(x, ...){
  UseMethod("reload")
}

#' @rdname reload
#' @export
reload.rezrDF = function(df, rezrObj, fields = ""){
  if(length(updateFunct(df)) > 1){
    if(all(fields == "")){
      df = reloadForeign(df, rezrObj, fields = "")
      df = reloadLocal(df, fields = "")
    } else {
      faList = fieldaccess(df)
      autoFields = intersect(faList[faList == "auto"], fields)
      foreignFields = intersect(faList[faList == "foreign"], fields)
      df = reloadForeign(df, fields = foreignFields)
      df = reloadLocal(df, fields = autoFields)
    }
  } else {
    warning("Reloading rezrDF with no update functions. The rezrDF was unchanged.")
  }
  df
}

#' @rdname reload
#' @export
reloadLocal = function(df, fields = ""){
  if(all(fields == "")){
    autoFields = names(fieldaccess(df)[fieldaccess(df) == "auto"])
    fields = intersect(names(updateFunct(df)), autoFields)
    if(length(fields) > 0){
      if(length(setdiff(autoFields, fields)) > 1){
        warning("The following auto fields have no updateFunction and cannot be updated:" %+%  paste(length(setdiff(autoFields, fields)), collapse = ", ") %+% ".")
      }
      updateFunctList = updateFunct(df)[fields]

      depsList = lapply(updateFunctList, function(x) deps(x))
      order = getUpdateOrder(depsList)
      df = reloadLocal(df, order)
    }
  } else {
    for(field in fields){
      if(!(field %in% names(updateFunct(df)))){
        stop("The field " %+% field %+% " does not have an update function defined.")
      }
      df = updateFunct(df)[[field]](df)
    }
  }
  df
}

#' @rdname reload
#' @export
reloadForeign = function(df, rezrObj, fields = ""){
  if(all(fields == '')){
    #Only select fields that are foreign AND have an update function
    foreignFields = names(fieldaccess(df)[fieldaccess(df) == "foreign"])
    updateableFields = names(updateFunct(df))
    fields = intersect(foreignFields, updateableFields)
    if(length(setdiff(foreignFields, fields)) > 1){
      warning("The following foreign fields have no updateFunction and cannot be updated:" %+%  paste(setdiff(foreignFields, fields), collapse = ", ") %+% ".")
    }
  }
  for(field in fields){
    if(!(field %in% names(updateFunct(df)))){
      stop("The field " %+% field %+% " does not have an update function defined.")
    }
    df = df %>% updateFunct(df)[[field]](rezrObj)
  }
  df
}


#' A constructor function for updateFunction objects
#'
#' @rdname updateFunction
#' @param f The function itself.
#' @param deps The dependencies of the function. Either a vector of single field names, or a single foreign field specified using an address, e.g. 'chunkDF/refexpr/word' refers to the word field of the refexpr layer of chunkDF.
#'
#' @return An updateFunction object
#' @export
new_updateFunction = function(f, deps){
  stopifnot(is.vector(deps))
  stopifnot(is.function(f))

  structure(f, class = c("updateFunction", "function"), deps = deps)
}


#' Create an update function.
#'
#' This is for 'auto' fields only; 'foreign' field take createUpdateFunction.
#'
#' @param field The field for which you want to create an update function.
#' @param x An R expression. For example, if you want to column2 to be updated to always be three times column3, then x should be column3 * 3.
#' @param df The rezrDF for which you want to create an update function.
#'
#' @return An update function with automatically generated dependency information. I will figure out the dependency information for you, so you don't have to define it yourself.
#' @export
createUpdateFunction = function(field, x, df){
  #Create the function itself
  field = enexpr(field)
  x = enexpr(x)
  funct = eval(expr(function(df) mutate(df, !!field := !!x)))

  #Figure out dependencies
  deps = character(0)
  x_flat = flatten_expr(x, includeFunct = F)
  for(item in x_flat){
    if(item %in% colnames(df)){
      deps = c(deps, item)
    }
  }

  new_updateFunction(funct, deps)
}

deps = function(updateFunct){
  attr(updateFunct, "deps")
}

`deps<-` = function(updateFunct, value){
  attr(updateFunct, "deps") = x
  updateFunct
}

getUpdateOrder = function(depsList){
  updateOrder = character(0)
  done = F
  #depsList = depsListOld
  while(!done){
    for(field in names(depsList)){
      currDeps = depsList[[field]]
      updateable = T
      for(dep in currDeps){
        if(dep %in% names(depsList)){
          #If field depends on something that has an entry in the depsList, then it's not safe to update!
          updateable = F
        }
      }
      if(updateable){
        #i.e. This field doesn't depend on anything inside the depsList, yay!
        updateOrder = c(updateOrder, field)
        #We can remove this guy from the devList since, after updating this this guy, we can safely update stuff that depends on it and other updated entries alone.
        depsList[[field]] = NULL
      }
    }
    if(length(names(depsList)) == 0) done = T
  }

  updateOrder
}

#' Create a left join update function.
#'
#' A function factory that allows the user to create an update function based on a left join.
#'
#' @param df The rezrDF to be updated.
#' @param rezrObj The full rezrObj.
#' @param address The address of the field you want to get data from in the *source* rezrDF. May be a vector if you have more than one source rezrDF. For example, the 'word' field of tokenDF has the address 'tokenDF/word', and the 'word' field of the 'verb' layer of chunkDF has the address 'chunkDF/verb/word'.
#' @param fkey The name of the foreign key in the target rezrDF.
#' @param field The name of the field in the target rezrDF to be updated. If the field names in the source DFs are all the same and also the same as the name in the target DF, you may leave this unspecified.
#'
#' @return An update function for the left join defined.
#' @export
createLeftJoinUpdate = function(address, fkey, field = ""){
  #Create the function itself (easy!)
  funct = function(df, rezrObj) updateLeftJoin(df, rezrObj, address, fkey, field)

  #Figure out the deps (actually still pretty simple!)
  deps = address

  new_updateFunction(funct, deps)
}


#' Update a field using a left join.
#'
#' @param df1 The rezrDF to be updated.
#' @param rezrObj The full rezrObj.
#' @param address An address to the field from the original df, from the rezrObj root. For example, the 'word' field of tokenDF has the address 'tokenDF/word', and the 'word' field of the 'verb' layer of chunkDF has the address 'chunkDF/verb/word'.
#' @param fkey The foreign key(s). Should match the number of primary keys in the df you're pulling information from (i.e. fieldaccess set as 'key').
#' @param field The name of the field in the target rezrDF to be updated. If the field names in the source DFs are all the same and also the same as the name in the target DF, you may leave this unspecified.
#'
#' @return The updated data frame.
#' @export
updateLeftJoin = function(df1, rezrObj, address, fkey, field = ""){
  #Get the source table, source field, source primary key, target field if unspecified
  targetTableInfo = getTargetTableInfo(rezrObj, address, field)
  unpackList(targetTableInfo)
  field = targetTableInfo[["field"]]

  #Create the by-line
  if(length(fkey) != length(df2key)){
    stop("Number of foreign keys does not match the number of primary keys in df1.")
  }
  by = character()
  for(i in 1:length(fkey)){
    by[[fkey[[i]]]] = df2key[[i]]
  }
  destField = parse_expr(splitAdd[length(splitAdd)])

  #Perform the join
  newVals = left_join(df1 %>% select(!!fkey), df2 %>% select(!!df2key, !!df2field), by = by) %>% pull(!!df2field)
  df1 = df1 %>% mutate(!!field := newVals)
  df1
}

getTargetTableInfo = function(rezrObj, address, field){
  if(length(address) == 1){
    splitAdd = strsplit(address, "/")[[1]]
    df2Add = splitAdd[-length(splitAdd)]
    df2 = listAt(rezrObj, df2Add)
    df2key = names(fieldaccess(df2)[fieldaccess(df2) == "key"])
    df2field = splitAdd[length(splitAdd)]
    if(field == ""){
      field = df2field
    }
  } else {
    #Most common example:
    #address = c("tokenDF/tokenSeq", "chunkDF/refexpr/tokenSeqFirst")

    splitAdds = strsplit(address, "/")
    df2Adds = lapply(splitAdds, function(x) paste0(x[-length(x)], collapse = "/"))
    df2s = lapply(df2Adds, function(x) listAt(rezrObj, x))
    df2keys = sapply(df2s, function(x) names(fieldaccess(x)[fieldaccess(x) == "key"]))
    df2fields = sapply(splitAdds, function(x) x[length(x)])
    if(field == ""){
      if(length(unique(df2fields)) == 1){
        field = df2fields[[1]]
      } else {
        stop("Please specify a field name.")
      }
    }
    df2key = df2keys[[1]]
    df2field = field
    df2s_prejoin = lapply(1:length(df2s), function(x) as.data.frame(df2s[[x]]) %>% select(!!df2key := df2keys[[x]], !!field := df2fields[[x]]))
    df2 = Reduce(rbind, df2s_prejoin[2:length(df2s_prejoin)], df2s_prejoin[[1]])
  }

  return(list(df2key = df2key, df2field = df2field, df2 = df2, field = field, splitAdd = splitAdd))
}

#' Update a field using a lowerToHigher operation.
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
  targetTableInfo = getTargetTableInfo(rezrObj, address, field)
  unpackList(targetTableInfo)
  field = targetTableInfo[["field"]]
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
#' @param df The target rezrDF you want to modify.
#' @param rezrObj The rezrObj.
#' @param address The address to the info field that you want from your source DF.
#' @param fkeyAddress The address to the token list in the nodeMap that corresponds to the target rezrDF.
#' @param action
#' @param field
#' @param fkeyInDF
#' @param seqName
#'
#' @return An update function for a lowerToHigher-type foreign field.
#' @export
#'
#' @examples
createLowerToHigherUpdate = function(address, fkeyAddress, action, field = "", fkeyInDF = FALSE, seqName = "discourseTokenSeq"){
  if(length(fkeyAddress) > 1){
    stop("Multiple sources are currently not supported in the updateLowerToHigher function. Sorry!")
  } else if(fkeyInDF){
    stop("Foreign keys in the current DF in the lowerToHigher function are not currently supported. Please use the correponding nodeMap.")
  }

  #Create the function itself (easy!)
  funct = function(df, rezrObj) updateLowerToHigher(df, rezrObj, address, fkeyAddress, action, field, fkeyInDF, seqName)

  #Figure out the deps (actually still pretty simple!)
  deps = address

  new_updateFunction(funct, deps)
}

#' Select columns in a rezrDF
#'
#' @param df A rezrDF object
#' @param ... Functions to be passed to dplyr select
#'
#' @return The rezrDF object with only the required columns
#' @export
#'
#' @examples
rez_select = function(df, ...){
  result = df %>% select(...)
  fieldaccess(result) = fieldaccess(result)[names(fieldaccess(result)) %in% colnames(result)]
  updateFunct(result) = updateFunct(result)[names(updateFunct(result)) %in% colnames(result)]
  inNodeMap(result) = inNodeMap(result)[names(inNodeMap(result)) %in% colnames(result)]
  result
}

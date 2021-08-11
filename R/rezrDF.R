#This file is for rezrDF-related functions that don't belong to a larger group!
#Table of contents:
#1) Constructor function: new_rezrDF
#2) Extracting attributes: fieldaccess, updateFunct, inNodeMap
#3) Setting attributes: fieldaccess<-, updateFunct-, inNodeMap<-
#4) Matters related to updateFunctions:
#  a) Constructor function: new_updateFunction
#  b) Getting and setting 'deps' attribute: deps, deps<-
#5) Reloading functions: reload.rzrdf, reloadLocal, reloadForeign
#6) General functions for rezrDF operations
#  a) df_op: The main function called by all complex rezrDF operations
#  b) getSourceTableInfo: Internal function used for unpacking information entered into update functions
#  NB: See rezrDF_mutate.R, rezrDF_lowerToUpper.R and rezrDF_left_join.R for more complex rezrDF operations with their own R files, plus rezrDF_easy for ease-of-use versions of these.
#7) Miscellaneous data.frame operations (without their own files):
#  a) rez_group_split
#  b) rez_select
#  TBA: rez_rename, rez_group_by
#8) Utilities:
#  a) Get fields of a certain field access type: getFieldsOfType, getKey

#' Constructor function for rezrDF
#'
#' @param df The data.frame
#' @param fieldaccess A set of field access values
#' @param updateFunct A set of update functions
#' @param inNodeMap Whether the fields of the data.frame are in the node map
#'
#' @return A rezrDF object
#' @export
new_rezrDF = function(df, fieldaccess, updateFunct, inNodeMap){
  #Validate all components of the DF
  stopifnot(is_tibble(df))
  stopifnot(is.vector(fieldaccess))
  stopifnot(!is.null(names(fieldaccess)))
  stopifnot(is.list(updateFunct))
  if(length(updateFunct) > 1){
    stopifnot(all(sapply(updateFunct, is.function)))
  }
  stopifnot(is.vector(inNodeMap))
  stopifnot(!is.null(names(inNodeMap)))

  structure(df, class = c("rezrDF", "tbl_df", "tbl", "data.frame"), fieldaccess = fieldaccess, updateFunct = updateFunct, inNodeMap = inNodeMap)
}


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

#' Extract/set an attribute of a updateFunction.
#'
#' Extract dependency information from an updateFunction, or set it.
#'
#' @rdname deps
#' @param updateFunct An updateFunction object.
#' @param value The value you want to set the function to.
#' @export
deps = function(updateFunct){
  attr(updateFunct, "deps")
}

#' @rdname deps
#' @export
`deps<-` = function(updateFunct, value){
  attr(updateFunct, "deps") = x
  updateFunct
}

#' @rdname reload
#' @export
reload.rezrDF = function(df, rezrObj, fields = ""){
  if(length(updateFunct(df)) > 1){
    if(all(fields == "")){
      #When no field is specified, just reload foreign fields first, then reload local fields
      df = reloadForeign(df, rezrObj, fields = "")
      df = reloadLocal(df, fields = "")
    } else {
      #If some fields are specified, we need to figure out which fields are auto and which are foreign. Then we reload foreign fields, and finally auto fields.
      faList = fieldaccess(df)
      autoFields = intersect(faList[faList == "auto"], fields)
      foreignFields = intersect(faList[faList == "foreign"], fields)
      df = reloadForeign(df, rezrObj, fields = foreignFields)
      df = reloadLocal(df, fields = autoFields)
    }
  } else {
    #If there are no updateFuncts, then we can't reload anything.
    warning("Reloading rezrDF with no update functions. The rezrDF was unchanged.")
  }
  df
}

#' @rdname reload
#' @export
reloadLocal = function(df, fields = ""){
  if(all(fields == "")){
    #If no fields are specified

    #Figure out which functions to update
    autoFields = names(fieldaccess(df)[fieldaccess(df) == "auto"])
    fields = intersect(names(updateFunct(df)), autoFields)

    #If there's something to update ...
    if(length(fields) > 0){
      if(length(setdiff(autoFields, fields)) > 1){
        #Warn the user about auto fields with no updateFunction
        warning("The following auto fields have no updateFunction and cannot be updated:" %+%  paste(length(setdiff(autoFields, fields)), collapse = ", ") %+% ".")
      }

      #Extract the updateFunctions, then get the update order from the dependency data
      updateFunctList = updateFunct(df)[fields]
      depsList = lapply(updateFunctList, function(x) deps(x))
      order = getUpdateOrder(depsList)

      #Re-run this function, but with specified fields
      df = reloadLocal(df, order)
    }
  } else {
    for(field in fields){
      if(!(field %in% names(updateFunct(df)))){
        #Only happens when the user specifies fields
        stop("The field " %+% field %+% " does not have an update function defined.")
      }

      df = updateFunct(df)[[field]](df) #Perform the update function
    }
  }
  df
}

#A purely internal function
#For getting the update order of AUTO fields
getUpdateOrder = function(depsList){
  updateOrder = character(0)
  done = F
  #depsList = depsListOld

  #At each stage, we look at the list of things to update.
  #If something does not depend on fields that are neither non-auto nor in the update queue, then it gets added to the update queue.
  #Repeat until update order is decided.
  while(!done){
    someUpdateable = F
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
        someUpdateable = T
        #i.e. This field doesn't depend on anything inside the depsList, yay!
        updateOrder = c(updateOrder, field)
        #We can remove this guy from the devList since, after updating this this guy, we can safely update stuff that depends on it and other updated entries alone.
        depsList[[field]] = NULL
      }
    }
    if(length(names(depsList)) == 0){
      done = T
      break
    }
    if(!someUpdateable){
      stop("You have circular dependencies in your auto fields (e.g. A depends on B, B depends on C, C depends on A).")
    }
  }

  updateOrder
}


#' @rdname reload
#' @export
reloadForeign = function(df, rezrObj, fields = ""){
  if(all(fields == '')){
    #When no field is specified ...
    #Only select fields that are foreign AND have an update function
    foreignFields = names(fieldaccess(df)[fieldaccess(df) == "foreign"])
    updateableFields = names(updateFunct(df))
    fields = intersect(foreignFields, updateableFields)

    #Warn the user if some foreign fields lack update functions
    if(length(setdiff(foreignFields, fields)) > 1){
      warning("The following foreign fields have no updateFunction and cannot be updated:" %+%  paste(setdiff(foreignFields, fields), collapse = ", ") %+% ".")
    }
  }

  for(field in fields){
    if(!(field %in% names(updateFunct(df)))){
      #This will really only happen when the user specifies fields and they don't have update functions. It's an error instead of a warning since it's clearly not intended behaviour
      stop("The field " %+% field %+% " does not have an update function defined.")
    }
    df = df %>% updateFunct(df)[[field]](rezrObj) #Perform the update function
  }
  df
}



#' Perform an operation on a data.frame while updating field access status.
#'
#' This is a wrapper for performing data frame manipulation functions (such as dplyr functions) while also updating field access status. This function only needs to be directly called by advanced users; beginning users may stick to function such as rez_mutate and rez_left_join, which are much more simple and intuitive to use.
#'
#' @param df The data frame to be modified.
#' @param fieldaccess The field access status of the field you're addding, either a single character (to apply to all of the new fields) or a vector of characters for each new field. Note that if you are both modifying and adding fields, only the added fields will have access values changed. So if you're specifying an entire vector of field access values, the best practice in using this function is to separate new-field and added-field mutates, otherwise the code will be difficult to read.
#' @param .f The function to be performed.
#' @param ... Argument to be passed to .f.
#' @param fieldaccess The field access status of the arguments.
#' @param updateFunct A list of updateFunctions to be added.
#' @param oldNames The names of the fields originally in the rezrDF AFTER the operation. If no fields will be renamed, leave blank.
#'
#' @return The DF after the operation.
#' @export
rez_dfop = function(df, .f, ..., fieldaccess = "flex", updateFunct = NA, oldNames = ""){
  #oldNames = Names of the original columns after the operation. If they won't be changed in the operation, then oldNames can just be left blank since we'll just take the original names.
  if(all(oldNames == "")){
    oldNames = colnames(df)
  }

  resultDF = .f(df, ...)


  newNames = colnames(resultDF)
  addedNames = setdiff(newNames, oldNames) #Find which columns (if any) are new

  #Setting DF properties of new columns
  if(length(addedNames) >= 1){
    fieldaccess(resultDF, addedNames) = fieldaccess
    inNodeMap(resultDF, addedNames) = "no"
    if(!is.na(updateFunct)){
      updateFunct(resultDF) = c(updateFunct(resultDF), updateFunct)
    }
  }

  resultDF
}

#This table extracts source table info from inputs to rezrDF operation functions.
#Despite the name, it also guesses TARGET table field when it's guessable from the source information.
#rezrObj: The rezrObj object where we want to extract target table info
#address: An address to a field name in the source rezrDF
#field: The field name in the TARGET rezrDF in case we need to figure it out
getSourceTableInfo = function(rezrObj, address, field){
  if(length(address) == 1){
    #When there's only one address ...
    splitAdd = strsplit(address, "/")[[1]] #Split the address up
    df2Add = splitAdd[-length(splitAdd)] #df2Add is the address without the field name
    df2 = listAt(rezrObj, paste0(df2Add, collapse = "/")) #Extract the source table
    df2key = names(fieldaccess(df2)[fieldaccess(df2) == "key"]) #Extract the key field of the source table
    df2field = splitAdd[length(splitAdd)] #Extract the source table field
    if(field == ""){ #If target table field isn't specified ...
      field = df2field #Use the SOURCE table field
    }

    return(list(df2key = df2key, df2field = df2field, df2 = df2, field = field, splitAdd = splitAdd))
  } else {
    #When there isn't one single source table but multiple.
    #We'll have to combine the source tables.
    #Most common example:
    #address = c("tokenDF/tokenSeq", "chunkDF/refexpr/tokenSeqFirst")

    #These are mostly as above but apply to a vector of addresses.
    splitAdds = strsplit(address, "/")
    df2Adds = lapply(splitAdds, function(x) paste0(x[-length(x)], collapse = "/"))
    df2s = lapply(df2Adds, function(x) listAt(rezrObj, x))
    df2keys = sapply(df2s, function(x) names(fieldaccess(x)[fieldaccess(x) == "key"]))
    df2fields = sapply(splitAdds, function(x) x[length(x)])

    #If target field isn't specified, we can only guess what it is if there's all the source fields have the same name!
    if(field == ""){
      if(length(unique(df2fields)) == 1){
        field = df2fields[[1]]
      } else {
        stop("Please specify a target field name.")
      }
    }

    #We'll now create a temporary merged table ...

    df2key = df2keys[[1]]#Arbitrarily pick the key field name of the first source table as the name of the key field in the temp table
    df2field = field #Choose the name of the target field as the name of the other field in the temp table
    df2s_prejoin = lapply(1:length(df2s), function(x) as.data.frame(df2s[[x]]) %>% select(!!df2key := df2keys[[x]], !!field := df2fields[[x]])) #Create the bits of the df2...
    df2 = Reduce(rbind, df2s_prejoin[2:length(df2s_prejoin)], df2s_prejoin[[1]]) #And put them together

    return(list(df2key = df2key, df2field = df2field, df2 = df2, field = field))
  }

}


#' Perform a group_split on a rezrDF
#'
#' The main difference with group_split is that attributes are retained. If you use dplyr group_split, attributes will NOT be retained!
#'
#' @param df A rezrDF object.
#' @param ... Usual group_split parameters.
#'
#' @return A list rezrDF objects after the group_split.
#' @export
#'
#' @examples
rez_group_split = function(df, ...){
  split = df %>% group_split(...)
  result = list()
  for(i in 1:length(split)){
    result[[i]] = new_rezrDF(split[[i]], fieldaccess(df), updateFunct(df), inNodeMap(df))
  }
  message("rez_group_split is a potentially destructive action. It is NOT recommended to assign it back to a rezrDF inside a rezrObj. If you must do so, be careful to check all addresses to ensure that they are correct.")
  result
}

#' Select columns in a rezrDF
#'
#' The main difference with dplyr select is that attributes will be updated to remove fields no longer there. There are no other differences at the moment.
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

  #Update attributes
  fieldaccess(result) = fieldaccess(result)[names(fieldaccess(result)) %in% colnames(result)]
  updateFunct(result) = updateFunct(result)[names(updateFunct(result)) %in% colnames(result)]
  inNodeMap(result) = inNodeMap(result)[names(inNodeMap(result)) %in% colnames(result)]
  result
}

rez_rename = function(df, ...){
  oldNames = colnames(df)
  result = rename(df, ...)
  newNames = colnames(result)
  for(i in 1:length(oldNames)){
    if(oldNames[i] != newNames[i]){
      names(attr(result, "updateFunct"))[names(attr(result, "updateFunct")) == oldNames[i]] = newNames[i]
      names(attr(result, "fieldaccess"))[names(attr(result, "fieldaccess")) == oldNames[i]] = newNames[i]
      names(attr(result, "inNodeMap"))[names(attr(result, "inNodeMap")) == oldNames[i]] = newNames[i]
    }
  }
  result
}

getFieldsOfType = function(df, type){
  names(fieldaccess(df))[fieldaccess(df) == type]
}

getKey = function(df){
  getFieldsOfType(df, "key")
}

#A group of functions for handling the modification of Rez data structures.
#At the moment, it handles the data frames only. At one point, the node maps will also be handled through this interface.

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

#Field access types

#' Extract the field access type from a data.frame
#'
#' Extract field access type data from a data.frame.
#'
#' @param df The data.frame whose field access attributes you want to see.
#' @param fields The field whose access attribute you want to see. If left blank, a vector containing all the attributes is output.
#'
#' @return fieldaccess
#' @export
fieldaccess = function(df, fields = "") getRezrDFAttr(df, "fieldaccess", fields)
updateFunct = function(df, fields = "") getRezrDFAttr(df, "updateFunct", fields)
inNodeMap = function(df, fields = "") getRezrDFAttr(df, "inNodeMap", fields)


#setFieldaccess = function(df, fields = "", value) setRezrDFAttr(df, "fieldaccess", fields, value)
#setUpdateFunct = function(df, fields = "", value) setRezrDFAttr(df, "updateFunct", fields, value)
#setInNodeMap = function(df, fields = "", value) setRezrDFAttr(df, "inNodeMap", fields, value)
#`updateFunct<-` = function(df, value) setRezrDFAttr(df, "updateFunct", "", value)

#' Set an attribute in a data.frame
#'
#' Set an attribute in a data.frame.
#'
#' @param df The data.frame whose field access attributes you want to set.
#' @param field The field whose access attribute you want to set. If left blank, a vector containing all the attributes is output.
#' @param value The value you want to set the fields to. It may be one single value for all of the entries, a vector for each entry in fields, or if fields is unspecified, a named vector containing field access status of all fields in the data.frame.
#'
#' @return fieldaccess
setRezrDFAttr = function(df, attr, fields = "", value){
  if(all(fields == "")){
    if(!setequal(colnames(df), names(fields))){
      warning("The data.frame does not have the same field as the value vector that you are giving me. Please check that the value vector is correct, or specifiy the fields you want to set.")
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
`updateFunct<-` = function(df, fields = "", value) setRezrDFAttr(df, "updateFunct", fields, value)
`fieldaccess<-` = function(df, fields = "", value) setRezrDFAttr(df, "fieldaccess", fields, value)
`inNodeMap<-` = function(df, fields = "", value) setRezrDFAttr(df, "inNodeMap", fields, value)


#' Extract the update functions from a rezrDF
#'
#' Extract update functions from a rezrDF
#'
#' @param df The rezrDF whose field access attributes you want to see.
#' @param fields The field whose access attribute you want to see. If left blank, a vector containing all the attributes is output.
#'
#' @return updateFunct
#' @export


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
#' @param df
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
rez_left_join = function(df1, df2, ..., fieldaccess = "foreign"){
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

  if(!suffixIncl){
    result = rez_dfop(df1, left_join, fieldaccess = fieldaccess, updateFunct = updateFunction, df2, suffix = c("", "_lower"), ...)
  } else {
    #We need to specify oldNames to rez_dfop because the names of fields in df1 may be changed.
    leftSuffix = list(...)[["suffix"]][1]
    oldNames = unique(c(oldNames, paste0(oldNames, leftSuffix)))
    result = rez_dfop(df1, left_join, fieldaccess = fieldaccess, updateFunct = updateFunction, oldNames = oldNames, df2, ...)
  }

  result
}

rez_group_split = function(df, ...){
  split = df %>% group_split(...)
  result = list()
  for(i in 1:length(split)){
    print(fieldaccess(df))
    result[[i]] = new_rezrDF(split[[i]], fieldaccess(df), updateFunct(df), inNodeMap(df))
  }
  result
}

reload = function(x, ...){
  if("rezrDF" %in% class(x)){
    reload.rezrDF(x, ...)
  } else if("rezrObj" %in% class(x)){
    reload.rezrObj(x, ...)
  }
}

reload.rezrDF = function(df, fields = ""){
  if(length(updateFunct(df)) > 1){
    if(all(fields == "")){
      #TODO: Reload all auto fields in the DF
      depsList = lapply(updateFunct(df), function(x) deps(x))
      order = getUpdateOrder(depsList)
      df = reload.rezrDF(df, order)
    } else {
      for(field in fields){
        if(!(field %in% names(updateFunct(df)))){
          stop("The field " %+% field %+% " does not have an update function defined.")
        }
        #print(updateFunct(df)[[field]])
        df = updateFunct(df)[[field]](df)
      }
    }
  } else {
    warning("Reloading rezrDF with no update functions. The rezrDF was unchanged.")
  }
  df
}

new_updateFunction = function(f, deps){
  stopifnot(is.list(deps))
  stopifnot(is.function(f))

  structure(f, class = c("updateFunction", "function"), deps = deps)
}


#' Create an update function.
#'
#' This is for 'auto' fields only; 'foreign' field take createUpdateFunction.
#'
#' @param df The rezrDF for which you want to create an update function.
#' @param field The field for which you want to create an update function.
#' @param x An R expression. For example, if you want to column2 to be updated to always be three times column3, then x should be column3 * 3.
#'
#' @return An update function with automatically generated dependency information. I will figure out the dependency information for you, so you don't have to define it yourself.
#' @export
#'
#' @examples
createUpdateFunction = function(df, field, x){
  #Create the function itself
  field = enexpr(field)
  x = enexpr(x)
  funct = eval(expr(function(df) mutate(df, !!field := !!x)))

  #Figure out dependencies
  deps = list()
  x_flat = flatten_expr(x, includeFunct = F)
  for(item in x_flat){
    print(item)
    if(item %in% colnames(df)){
      deps = c(deps, item)
    }
  }

  new_updateFunction(funct, deps)
}

deps = function(updateFunct){
  attr(updateFunct, "deps")
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

createLeftJoinUpdate = function(rezObj, df, field, x){

}

updateLeftJoin = function(rezObj, df, field, x){

}

#A group of functions for handling the modification of Rez data structures.
#At the moment, it handles the data frames only. At one point, the node maps will also be handled through this interface.


getRezrDFAttr = function(df, attr, fields = ""){
  if(all(fields == "")){
    attr(df, attr, fields)
  } else {
    attr(df, attr, fields)[fields]
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

setRezrDFAttr = function(df, attr, fields = "", value){
  if(all(fields == "")){
    if(!setequal(colnames(df), names(fields))){
      warning("The data.frame does not have the same field as the value vector that you are giving me. Please check that the value vector is correct, or specifiy the fields you want to set.")
    }
    attr(df, attr) = value
  } else {
    attr(df, attr)[fields] = value
  }
  df
}

setFieldaccess = function(df, fields = "", value) setRezrDFAttr(df, "fieldaccess", fields, value)
setUpdateFunct = function(df, fields = "", value) setRezrDFAttr(df, "updateFunct", fields, value)
setInNodeMap = function(df, fields = "", value) setRezrDFAttr(df, "inNodeMap", fields, value)


#' Set the field access type from a data.frame
#'
#' Set field access type data from a data.frame.
#'
#' @param df The data.frame whose field access attributes you want to set.
#' @param field The field whose access attribute you want to set. If left blank, a vector containing all the attributes is output.
#' @param value The value you want to set the fields to. It may be one single value for all of the entries, a vector for each entry in fields, or if fields is unspecified, a named vector containing field access status of all fields in the data.frame.
#'
#' @return fieldaccess
#' @export


#' Extract the update functions from a rezrDF
#'
#' Extract update functions from a rezrDF
#'
#' @param df The rezrDF whose field access attributes you want to see.
#' @param fields The field whose access attribute you want to see. If left blank, a vector containing all the attributes is output.
#'
#' @return updateFunct
#' @export
fieldaccess = function(df, fields = ""){
  if(all(fields == "")){
    attr(df, "updateFunct")
  } else {
    attr(df, "updateFunct")[fields]
  }
}


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
rez_dfop = function(df, fieldaccess, updateFunct = NA, oldNames = "", .f, ...){
  if(all(oldNames) == ""){
    oldNames = colnames(df)
  }
  resultDF = .f(df, ...)
  newNames = colnames(resultDF)
  #Find which columns (if any) are new
  addedNames = setdiff(newNames, oldNames)
  #Set the column to the given fieldaccess value
  attr(resultDF, "fieldaccess")[addedNames] = fieldaccess
  attr(resultDF, "inNodeMap")[addedNames] = "no"
  if(!is.na(updateFunct)){
    attr(resultDF, "updateFunct") = c(updateFunct(resultDF), updateFunct)
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
rez_mutate = function(df, fieldaccess = "flex", ...){
  #Validation
  #If it's dynamic, list should throw an error
  if("try-error" %in% class(try(list(...), silent=TRUE))){
    if("try-error" %in% class(try(expr(...), silent=TRUE))){
      changedFields = character()
      warning("Validation failed for the current mutate.")
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
  rez_dfop(df, fieldaccess, NA, mutate, ...)
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
rez_left_join = function(df1, df2, fieldaccess = "foreign", ...){
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
    result = rez_dfop(df1, fieldaccess, updateFunction, .f = left_join, df2, suffix = c("", "_lower"), ...)
  } else {
    #We need to specify oldNames to rez_dfop because the names of fields in df1 may be changed.
    leftSuffix = list(...)[["suffix"]][1]
    oldNames = unique(c(oldNames, paste0(oldNames, leftSuffix)))
    result = rez_dfop(df1, fieldaccess, updateFunction, oldNames = oldNames, .f = left_join, df2, ...)
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

rez_reload(df, field = ""){

}

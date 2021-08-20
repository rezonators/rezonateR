#This file is for rezrDF-related functions that don't belong to a larger group!
#Table of contents:
#1) Constructor function: new_rezrDF
#2) Extracting attributes: fieldaccess, updateFunct, inNodeMap
#3) Setting attributes: fieldaccess<-, updateFunct-, inNodeMap<-
#4) Matters related to updateFunctions:
#  a) Constructor function: new_updateFunction
#  b) Getting and setting 'deps' attribute: deps, deps<-
#5) Reloading functions: reload.rzrdf, reloadLocal, reloadForeign
#7) Utilities:
#  a) Get fields of a certain field access type: getFieldsOfType, getKey
#  b) killIfPresent: Remove a column if it's present in a rezrDF.

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
  stopifnot("tbl_df" %in% class(df))
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
        warning("The data.frame does not have the same field as the value vector that you are giving me. Please check that the value vector is correct, or specifiy the fields you want to set. " %+% "Data frame column only: " %+% paste(setdiff(colnames(df), names(value)), collapse = ", ") %+% "; value vector names only: " %+% paste(setdiff(names(value), colnames(df)), collapse = ", ") %+% ".")
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
  if(length(updateFunct(df)) >= 1){
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

      dfTry = updateFunct(df)[[field]](df) #Perform the update function
      if("try-error" %in% class(df)){
        warning("Error in updating the following field: " %+% field %+% ". Field skipped.")
      } else {
        df = dfTry
      }
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
    dfTry = try(df %>% updateFunct(df)[[field]](rezrObj)) #Perform the update function
    if("try-error" %in% class(dfTry)){
      warning("Error in updating the following field: " %+% field %+% ". Field skipped.")
    } else {
      df = dfTry
    }
  }
  df
}

stringToFactor = function(df, colsToChange = NULL, levels = list()){
  result = df
  if(is.null(colsToChange)) colsToChange = colnames(df)
  for(col in colsToChange){
    if(!is.character(col) | !(col %in% colsToChange)) next
    if(col %in% names(levels)){
      result[[col]] = factor(result[[col]], levels[[col]])
    } else {
      result[[col]] = factor(result[[col]])
    }
    if(col %in% names(updateFunct(df))){
      oldFunct = updateFunct(df, col)
      if(col %in% names(levels)){
        updateFunct(result, col) = function(...) oldFunct(...) %>%
          mutate(!!parse_expr(col) := factor(!!parse_expr(col), levels[[col]]))
      } else {
        updateFunct(result, col) = function(...) oldFunct(...) %>%
          mutate(!!parse_expr(col) := factor(!!parse_expr(col)))
      }
    }
  }
  result
}


getFieldsOfType = function(df, type){
  names(fieldaccess(df))[fieldaccess(df) %in% type]
}

getKey = function(df){
  getFieldsOfType(df, "key")
}

killIfPresent = function(df, colnames){
  for(colname in colnames){
    if(colname %in% names(df)){
      df = df %>% rez_select(-!!parse_expr(colname))
    }
  }
  df
}

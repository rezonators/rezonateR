#1) General functions for rezrDF operations
#  a) df_op: The main function called by all complex rezrDF operations
#  b) getSourceTableInfo: Internal function used for unpacking information entered into update functions
#  NB: See rezrDF_mutate.R, rezrDF_lowerToHigher.R and rezrDF_left_join.R for more complex rezrDF operations with their own R files, plus rezrDF_easy for ease-of-use versions of these.
#2) Miscellaneous data.frame operations (without their own files):
#3) Selecting columns: rez_select
#4) Renaming columns: rez_rename
#5) Grouping functions: rez_group_by, rez_ungroup, rez_group_split
#6) Combining data frames: rez_bind_row

#' Perform an operation on a data.frame while updating field access status.
#'
#' This is a wrapper for performing data frame manipulation functions (such as dplyr functions) while also updating field access status. Not ordinarily called by users; beginning users may stick to functions such as [rezonateR::addFieldLocal], [rezonateR::changeFieldForeign], [rezonateR::rez_mutate], [rezonateR::rez_left_join], which are much more simple and intuitive to use.
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

  oldFieldAccess = fieldaccess(df)
  oldUpdateFunct = updateFunct(df)
  oldInNodeMap = inNodeMap(df)

  #oldNames = Names of the original columns after the operation. If they won't be changed in the operation, then oldNames can just be left blank since we'll just take the original names.
  if(all(oldNames == "")){
    oldNames = colnames(df)
  }

  resultDF = .f(df, ...)

  suppressWarnings(
    if("grouped_df" %in% class(resultDF)){
      fieldaccess(resultDF) = oldFieldAccess
      updateFunct(resultDF) = oldUpdateFunct
      inNodeMap(resultDF) = oldInNodeMap
    }
  )

  newNames = colnames(resultDF)
  addedNames = setdiff(newNames, oldNames) #Find which columns (if any) are new

  #Setting DF properties of new columns ONLY
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
#df2key: Key field in the source table, if empty then we take the primary key
getSourceTableInfo = function(rezrObj, address, df2key, field){

  if(length(address) == 1){
    #When there's only one address ...
    splitAdd = strsplit(address, "/")[[1]] #Split the address up
    df2Add = splitAdd[-length(splitAdd)] #df2Add is the address without the field name
    df2 = listAt(rezrObj, paste0(df2Add, collapse = "/")) #Extract the source table

    if(any(df2key == "")) df2key = names(fieldaccess(df2)[fieldaccess(df2) == "key"]) #Extract the key field of the source table
    df2field = splitAdd[length(splitAdd)] #Extract the source table field
    if(field == ""){ #If target table field isn't specified ...
      field = df2field #Use the SOURCE table field
    }

    return(list(df2key = df2key, df2field = df2field, df2 = df2, field = field, splitAdd = splitAdd))
  } else {
    df2keys = NULL
    #When there isn't one single source table but multiple.
    #We'll have to combine the source tables.
    #Most common example:
    #address = c("tokenDF/tokenOrder", "chunkDF/refexpr/tokenOrderFirst")

    #These are mostly as above but apply to a vector of addresses.
    splitAdds = strsplit(address, "/")
    df2Adds = lapply(splitAdds, function(x) paste0(x[-length(x)], collapse = "/"))
    df2s = lapply(df2Adds, function(x) listAt(rezrObj, x))
    if(all(df2key == "")) df2keys = sapply(df2s, function(x) names(fieldaccess(x)[fieldaccess(x) == "key"]))
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

    if(df2key == "" | is.na(df2key) | is.null(df2key)){
      if(!is.null(df2keys)){
        df2key = df2keys[[1]]#Arbitrarily pick the key field name of the first source table as the name of the key field in the temp table
      } else {
        stop("Missing df2key.")
      }
    }

    if(is.null(df2keys) | length(df2keys) == 0){
      df2keys = rep(df2key, length(address))
    }

    df2field = field #Choose the name of the target field as the name of the other field in the temp table
    df2s_prejoin = lapply(1:length(df2s), function(x) as.data.frame(df2s[[x]]) %>% select(!!parse_expr(df2key) := df2keys[[x]],   !!parse_expr(field) := df2fields[[x]])) #Create the bits of the df2...
    df2 = Reduce(rbind, df2s_prejoin[2:length(df2s_prejoin)], df2s_prejoin[[1]]) #And put them together

    return(list(df2key = df2key, df2field = df2field, df2 = df2, field = field))
  }

}


#' Select columns in a rezrDF
#'
#' The main difference with dplyr select is that attributes will be updated to remove fields no longer there. There are no other differences at the moment.
#'
#' @param df A rezrDF object
#' @param ... Functions to be passed to dplyr select
#'
#' @return The rezrDF object with only the required columns
#' @examples sbc007$trackDF$default %>% rez_select(id, name, text)
#' @export
rez_select = function(df, ...){
  result = df %>% select(...)

  #Update attributes
  fieldaccess(result) = fieldaccess(result)[names(fieldaccess(result)) %in% colnames(result)]
  updateFunct(result) = updateFunct(result)[names(updateFunct(result)) %in% colnames(result)]
  inNodeMap(result) = inNodeMap(result)[names(inNodeMap(result)) %in% colnames(result)]
  result
}

#' Rename rezrDF columns.
#'
#' @inheritParams rez_mutate
#' @param ... Functions to be passed to rename. New column names are argument names; old column names are argument values.
#'
#'
#' @return A rezrDF object.
#' @examples sbc007$treeEntryDF$default %>% rez_rename(gramRel = Relation)
#' @note This function does not update foreign references to the field that you're renaming. So be sure to update the updateFunctions of those fields; otherwise, you will break your rezrObj.
#' @export
rez_rename = function(df, ...){
  message("Tip: When performed on a rezrDF inside a rezrObj, rez_rename is a potentially destructive action. It is NOT recommended to assign it back to a rezrDF inside a rezrObj. If you must do so, be careful to update all addresses from other DFs to this DF.")

  oldNames = colnames(df)
  result = rename(df, ...)
  newNames = colnames(result)
  for(i in 1:length(oldNames)){
    if(oldNames[i] != newNames[i]){
      names(attr(result, "updateFunct"))[names(attr(result, "updateFunct")) == oldNames[i]] = newNames[i]
      names(attr(result, "fieldaccess"))[names(attr(result, "fieldaccess")) == oldNames[i]] = newNames[i]
      names(attr(result, "inNodeMap"))[names(attr(result, "inNodeMap")) == oldNames[i]] = newNames[i]

      #Updating updateFuncts to reflect new names
      for(f in attr(result, "updateFunct")){
        for(var in ls(environment(f))){
          if(var %in% c("address", "fkey", "field", "fkeyAddress", "seqName")){
            if(all(environment(f)[[var]] == oldNames[i])){
              environment(f)[[var]] = newNames[i]
            }
          } else if(var == oldNames[i]){
            environment(f)[[newNames[i]]] = environment(f)[[var]]
            rm(var, envir = environment)
          } else if(is.call(environment(f)[[var]])){
            environment(f)[[var]] = replace_expr_element(environment(f)[[var]], oldNames[i], newNames[i])
          }
        }
      }
    }
  }
  result
}



#' Group / ungroup rezrDFs.
#'
#' A replacement for [dplyr::group_by()] and [dplyr::ungroup()].
#'
#' @rdname gug
#' @inheritParams rez_mutate
#' @param ... Arguments to be passed to group_by / ungroup, including the field(s) on which you're grouping.
#'
#' @return The grouped / ungrouped rezrDF.
#' @examples
#' sbc007$tokenDF = sbc007$tokenDF %>%
#' rez_group_by(unit) %>% rez_mutate(unitLength = inLength(text, isWord = (kind == "Word"))) %>% rez_ungroup
#' @note There is no replacement for [dplyr::summarise()]. Currently there are no plans for a `rezonateR` equivalent of [dplyr::summarise()], but please feel free to describe a use case in a GitHub issue if you would like.
#' @export
rez_group_by = function(df, ...){
  result = group_by(df, ...)
  updateFunct(result) = updateFunct(df)
  fieldaccess(result) = fieldaccess(df)
  inNodeMap(result) = inNodeMap(df)
  class(result) = c("rezrDF", class(result))
  result
}

#' @rdname gug
#' @export
rez_ungroup = function(df, ...){
  result = ungroup(df, ...)
  updateFunct(result) = updateFunct(df)
  fieldaccess(result) = fieldaccess(df)
  inNodeMap(result) = inNodeMap(df)
  class(result) = c("rezrDF", class(result))
  result
}

#' Perform a group_split on a `rezrDF`
#'
#' The main difference with [dplyr::group_split()] is that attributes are retained. If you use [dplyr::group_split()], attributes will NOT be retained.
#'
#' @param df A `rezrDF` object.
#' @param ... Usual `group_split` parameters.
#'
#' @return A list `rezrDF` objects after the `group_split`.
#' @examples sbc007_arguments_byRelation = sbc007$treeEntry$default %>% filter(level = 1) %>% group_split(Relation)
#' @note This is mostly useful for creating smaller emancipated rezrDFs. Do not assign the results of this function back to a rezrObj.
#' @export
#'
rez_group_split = function(df, ...){
  split = df %>% group_split(...)
  result = list()
  for(i in 1:length(split)){
    result[[i]] = new_rezrDF(split[[i]], fieldaccess(df), updateFunct(df), inNodeMap(df))
  }
  message("Tip: rez_group_split is a potentially destructive action. It is NOT recommended to assign it back to a rezrDF inside a rezrObj. If you must do so, be careful to check all addresses to ensure that they are correct.")
  result
}

#' Bind together related `rezrDF` objects.
#'
#' @param ... The `rezrDF` objects to be combined
#' @param type The type of combination. If 'intersect', I will take the intersection of the columns in the `rezrDFs`. If 'union', I will take the union of the columns, populating missing fields with `NA`s.
#'
#' @return The bound `rezrDF`
#' @examples sbc007_allChunks = rez_bind_rows(sbc007$chunkDF$verb, sbc007$chunkDF$refexpr)
#' @export
rez_bind_rows = function(..., type = "intersect"){
  args = list(...)
  if(!is.null(names(args))){
    dfs = args[names(args) != c("deparse.level", "make.row.names", "stringsAsFactors", "factor.exclude")]
  } else {
    dfs = args
  }
  df1 = dfs[[1]]

  if(type == "intersect"){
    newCols = multi_intersect(lapply(dfs, names))
    dfs_new = lapply(dfs, function(x) rez_select(x, all_of(newCols)))
  } else {
    dfs_new = dfs
  }


  result = bind_rows(dfs_new)
  if(type == "union") newCols = names(result)

  updateFunct(result) = updateFunct(df1)[names(updateFunct(df1)) %in% newCols]
  fieldaccess(result) = fieldaccess(df1)[names(fieldaccess(df1)) %in% newCols]
  inNodeMap(result) = inNodeMap(df1)[names(inNodeMap(df1)) %in% newCols]
  class(result) = c("rezrDF", class(result))
  result

  #TODO: Cater for differing field names
}

#' Add new rows to a rezrDF.
#'
#' @param df The rezrDF to be updated.
#' @param ... Argument names are column names, and argument values are vectors of values of the rows you are adding. If a primary key is not supplied, I will generate one for you. Auto fields are automatically updated and do not need to be supplied.
#' @param rezrObj A rezrObj, if you want to ensure that the primary key doesn't overlap with any other node in the nodeMap.
#'
#'
#' @return The rezrDF with the new row(s).
#' @note Does not update foreign fields. If you want to update foreign fields, use [rezonateR::addRow.rezrDF]. If you call this function on a rezrDF with complex foreign fields, use [rezonateR::addRow.rezrDF] instead; otherwise, you cannot update those fields in the future.
#' @examples sbc007 = rez_add_row(sbc007$trailDF$default, doc = "sbc007", chainCreateSeq = max(rez007$trailDF$default$chainCreateSeq) + 1, layer = "default", name = "Danae", chainSize = 1)
#' @export
rez_add_row = function(df, ..., rezrObj = NULL){
  args = list(...)
  args[[".data"]] = df
  newVals = args[-which(names(args) %in% c(".data", ".before", ".after"))]

  idCol = getKey(df)
  if(!is.null(rezrObj)){
    existingIDs = getIDs(rezrObj$nodeMap)
  } else {
    existingIDs = df[[idCol]]
  }
  if(!(idCol %in% names(newVals))){
    numNewRows = length(newVals[[1]])
    args[[idCol]] = createRezId(numNewRows, existingIDs)
  } else {
    overlaps = intersect(newVals[[idCol]], existingIDs)
    if(length(overlaps) > 0){
      stop("The following IDs already exist and cannot be added: " %+% paste0(overlaps, collapse = ", "))
    }
  }

  missingFields = setdiff(getFieldsOfType(df, c("flex", "core")), names(newVals))
  if(length(missingFields) > 0){
    message("The following core and flex fields and not present and will have to be supplied later: " %+% paste0(missingFields, collapse = ", "))
  }

  result = exec("add_row", !!!args)

  result = reloadLocal(result)
}

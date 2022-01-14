#This file is for rezrDF-related functions about left joins.
#Table of contents:
#1) The left_join function itself: rez_left_join
#2) Updating a foreign field with lowerToHigher: updateLeftJoin
#3) Creating an updateFunction with updateLeftJoin: createLowerToHigherUpdate

#' Perform a left join on two rez data.frames and change field access status.
#'
#' This is a wrapper for performing left joins on Rez data.frames. It *only* changes the data frame, such as by changing field access values, at the moment. If your desired fieldaccess value is flex, this may serve as a drop-in replacement for mutate. Note that apart from the data.frame to be modified and the data.frame you are joining from, all arguments of left_join must be named.
#'
#' By default, if no suffix is specified, the suffixes are c("", "_lower"). That is, if you are joining two data.frames, both with a column called 'name', then the left data.frame's column will still be called 'name' in the new data.frame but the right data.frame's column will get called 'name_lower'.
#'
#' @param df1 The left data.frame.
#' @param df2 The right data.frame.
#' @param fieldaccess The field access status of the field you're addding, either a single character (to apply to all of the new fields) or a vector of characters for each new field. Note that if you are both modifying and adding fields, only the added fields will have access values changed. So if you're specifying an entire vector of field access values, the best practice in using this function is to separate new-field and added-field mutates, otherwise the code will be difficult to read.
#' @param ... Other functions passed onto left_join, i.e. the columns you will be changing or adding.
#' @param df2Address The address to the source rezrDF.
#' @param fkey The foreign key to the df2. If not present, I'll guess with the by-line.
#' @param rezrObj The rezrObj object.
#'
#' @return resultDF
#' @export
rez_left_join = function(df1, df2 = NULL, ..., fieldaccess = "foreign", df2Address = "", df2key = "", fkey = "", rezrObj = NULL){
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
  df2Add = strsplit(df2Address, "/")[[1]]
  if(is.null(df2)){
    if(!is.null(rezrObj) & !(all(df2Address == ""))){
      df2 = listAt(rezrObj, df2Add)
    } else{
      stop("You need to specify either a right data.frame, or a df2Address with a rezrObj.")
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
    if(length(autoBy) == 0){
      result = rez_dfop(df1, left_join, fieldaccess = fieldaccess, updateFunct = updateFunction, df2, suffix = c("", "_lower"), ...)
    } else {
      result = rez_dfop(df1, left_join, fieldaccess = fieldaccess, updateFunct = updateFunction, df2, suffix = c("", "_lower"), ..., by = autoBy)
    }

    #For the update function
    newNames = setdiff(names(result), oldNames)
    rightTblNames = chompSuffix(newNames, "_lower")
  } else {
    #We need to specify oldNames to rez_dfop because the names of fields in df1 may be changed.
    leftSuffix = list(...)[["suffix"]][1]
    oldNames = unique(c(oldNames, paste0(oldNames, leftSuffix)))
    if(length(autoBy) == 0){
      result = rez_dfop(df1, left_join, fieldaccess = fieldaccess, updateFunct = updateFunction, oldNames = oldNames, df2, ...)
    } else {
      #Some bug here that leads to suffixes being ignored ...
      result = rez_dfop(df1, left_join, fieldaccess = fieldaccess, updateFunct = updateFunction, oldNames = oldNames, df2, by = autoBy)
    }

    #For the update function
    newNames = setdiff(names(result), oldNames)
    rightTblNames = chompSuffix(newNames, list(...)[["suffix"]][2])
  }

  #Update function stuff
  if(all(df2Address == "") | all(fkey == "")){
    message("You didn't provide an df2Address and/or foreign key, so I won't be adding an update function automatically. Nuh-uh!")
  } else {
    for(i in 1:length(newNames)){
      newName = newNames[i]
      rightTblName = rightTblNames[i]
      if(all(df2Address == "tokenChunkDF")){
        if(str_ends(rightTblName, "First|Last")){
          updateAddress =
            c("tokenDF" %+% "/" %+% chompSuffix(rightTblName, "First|Last"),
              "chunkDF" %+% "/" %+% names(rezrObj$chunkDF) %+% "/" %+% rightTblName)
        } else {
          updateAddress =
          c("tokenDF" %+% "/" %+% rightTblName,
            "chunkDF" %+% "/" %+% names(rezrObj$chunkDF) %+% "/" %+% rightTblName)
        }
      } else {
        updateAddress = df2Address %+% "/" %+% rightTblName
      }
      updateFunct(result, newName) = createLeftJoinUpdate(updateAddress, fkey, df2key, newName)
    }
  }

  result
}


#' Update a field using a left join.
#'
#' Not normally called by users, but acts as an updateFunct to be called by [rezonateR::reload].
#'
#' @param df1 The rezrDF to be updated.
#' @param rezrObj The full rezrObj.
#' @param address An address to the field from the original DF, from the rezrObj root. For example, the 'word' field of tokenDF has the address 'tokenDF/word', and the 'word' field of the 'verb' layer of chunkDF has the address 'chunkDF/verb/word'. This may be a multiple-entry vector if you want to merge the source DFs.
#' @param fkey The foreign key(s). Should match the number of primary keys in the df you're pulling information from (i.e. fieldaccess set as 'key').
#' @param field The name of the field in the target rezrDF to be updated. If the field names in the source DFs are all the same and also the same as the name in the target DF, you may leave this unspecified.
#' @param df2key The name of the candidate key in the source table that corresponds to the foreign key of the target table. If left unspecified, I will use the primary key.
#'
#' @return The updated data frame.
#' @export
updateLeftJoin = function(df1, rezrObj, address, fkey, df2key = "", field = ""){
  #Get the source table, source field, source primary key, target field if unspecified
  sourceTableInfo = getSourceTableInfo(rezrObj, address, df2key, field)
  unpackList(sourceTableInfo)
  field = sourceTableInfo[["field"]]
  df2key = sourceTableInfo[["df2key"]]

  #Create the by-line
  if(length(fkey) != length(df2key)){
    stop("Number of foreign keys (" %+% paste(fkey, collapse = ", ") %+% " does not match the number of primary keys in df1 (" %+% paste(fkey, collapse = ", ") %+% ").")
  }
  by = character()
  for(i in 1:length(fkey)){
    by[[fkey[[i]]]] = df2key[[i]]
  }

  #Perform the join
  newVals = left_join(df1 %>% select(!!fkey), df2 %>% select(!!df2key, !!df2field), by = by) %>% pull(!!df2field)
  #print(newVals)
  df1 = df1 %>% mutate(!!field := newVals)
  df1
}

#' Create a left join update function.
#'
#' A function factory that allows the user to create an update function based on a left join. Not to be called by most users; it is automatically called by [rezonateR::rez_left_join] if the necessary information is supplied.
#'
#' @param df The rezrDF to be updated.
#' @param rezrObj The full rezrObj.
#' @param address The address of the field you want to get data from in the *source* rezrDF. May be a vector if you have more than one source rezrDF. For example, the 'word' field of tokenDF has the address 'tokenDF/word', and the 'word' field of the 'verb' layer of chunkDF has the address 'chunkDF/verb/word'.
#' @param fkey The name of the foreign key in the target rezrDF.
#' @param field The name of the field in the target rezrDF to be updated. If the field names in the source DFs are all the same and also the same as the name in the target DF, you may leave this unspecified.
#'
#' @return An update function for the left join defined.
#' @export
createLeftJoinUpdate = function(address, fkey, df2key = "", field = ""){
  #Create the function itself (easy!)
  address = eval(address)
  fkey = eval(fkey)
  df2key = eval(df2key)
  field = eval(field)
  funct = function(df, rezrObj) updateLeftJoin(df, rezrObj, address, fkey, df2key, field)

  #Figure out the deps (actually still pretty simple!)
  deps = address

  new_updateFunction(funct, deps)
}

#This file is for rezrDF-related functions about mutations, i.e. creation, editing and updating of fields based on other columns in the SAME table.
#Table of contents:
#1) The rez_mutate function itself: rez_mutate. This correponds also to the update functions in left_join and lowerToUpper, since mutate both adds and changes fields.
#2) Validating a field change: rez_validate_fieldchange
#3) Creating an update function with mutates: createUpdateFunction

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
#' @note If fieldaccess is not set, it will be set to flex for NEW fields. Old fields will remain unchanged. *Note that this is different behaviour from changeField, which changes field access status to 'flex' by default if field access is not given.*
#' @export
rez_mutate = function(df, ..., fieldaccess = ""){
  #Validation
  ops = enexprs(...)
  changedFields = intersect(names(ops), colnames(df))
  rez_validate_fieldchange(df, changedFields, fieldaccess != "", fieldaccess)

  #newFieldaccess = fieldaccess for new fields only
  #fieldaccess
  if(fieldaccess == ""){
    newFieldaccess = "flex"
  } else {
    newFieldaccess = fieldaccess
  }

  #TODO: Automatically create update function for the user if 'auto' is specified.
  result = rez_dfop(df, mutate, fieldaccess = newFieldaccess, ...)
  if(length(changedFields) > 1){
    #Note: rez_dfop only changes fieldaccess for new columns
    fieldaccess(result, changedFields) = fieldaccess #So we change them for changed old columns here
  }

  affected_fields = intersect(names(ops), colnames(result))  #incl BOTH new and changed fields
  if(!("grouped_df" %in% class(df))){
    for(field in affected_fields){
      if(fieldaccess(result, field) == "auto"){
        updateFunct(result, field) = createUpdateFunction(!!parse_expr(field), !!ops[[field]], result)
      }
    }
  } else {
    for(field in affected_fields){
      if(fieldaccess(result, field) == "auto"){
        updateFunct(result, field) = createUpdateFunction(!!parse_expr(field), !!ops[[field]], result, group_vars(df))
      }
    }
  }

  result
}



#' Validate a field change.
#'
#' This function ensures that the fields in a DF you wish to change are actually a good idea to change. It produces an error if a primary key is among the fields you wish to change, and warnings otherwise. This is automatically called if you use a rezonateR-internal function such as rez_mutate.
#'
#' @param df The rezrDF you are planning to change.
#' @param changedFields The fields you want to change.
#'
#' @export

rez_validate_fieldchange = function(df, changedFields, changingStatus = F, fieldaccess = ""){
  for(entry in changedFields){
    if(entry %in% names(fieldaccess(df))){
      if(fieldaccess(df)[entry] == "key"){
        stop("You cannot change a primary key: " %+% entry)
      } else if(fieldaccess(df)[entry] == "core"){
        if(changingStatus){
        warning("Note that you are changing the field access status and value of a core field " %+% entry %+% ". This may break things down the road.")
        } else {
        warning("Note that you are changing the value of a core field " %+% entry %+% ". This may break things down the road.")
        }
      } else if(fieldaccess(df)[entry] == "auto"){
        if(!changingStatus){
          warning("Note that you are changing a field " %+% entry %+% "that is automatically updated. Your change is likely to be overridden by a future update.")
        } else if(fieldaccess == "foreign"){
          message("Note that you are changing a field " %+% entry %+% "from auto to foreign. This will change reload behaviour.")
        } else if(fieldaccess == "flex"){
          message("Note that you are changing a field " %+% entry %+% "from auto to flex. This field will no longer reload.")
        }
      } else if(fieldaccess(df)[entry] == "foreign"){
        if(!changingStatus){
          warning("Note that you are changing a field " %+% entry %+% "that depends on another data.frame. Your change is likely to be overridden by a future update on the data.frame that this data.frame depends on.")
        } else if(fieldaccess == "auto"){
          message("Note that you are changing a field " %+% entry %+% "from foreign to auto This will change reload behaviour.")
        } else if(fieldaccess == "flex"){
          message("Note that you are changing a field " %+% entry %+% "from auto to flex This field will no longer reload.")
        }
      }
    }
  }

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
createUpdateFunction = function(field, x, df, groupField = ""){
  #Create the function itself
  field = enexpr(field)
  x = enexpr(x)

  if(groupField == ""){
    funct = eval(expr(function(df) updateMutate(df, field, x)))
  } else {
    funct = eval(expr(function(df) updateGroupByMutate(df, field, x, groupField)))
  }

  #Figure out dependencies
  deps = character(0)
  x_flat = flatten_expr(x)
  for(item in x_flat){
    if(item %in% colnames(df)){
      deps = c(deps, item)
    }
  }

  new_updateFunction(funct, deps)
}

#Internal function. It serves as a wrapper around mutate. This is to create an environment whose field variable can be changed later. Otherwise, the !! will evaluate the field and expression, preventing us from changing them dynamically.
updateMutate = function(df, field, expr){
  mutate(df, !!field := !!expr)
}

updateGroupByMutate = function(df, field, expr, groupField){
  df = df %>% rez_group_by(!!parse_expr(groupField)) %>% rez_mutate(!!field := !!expr) %>% rez_ungroup(!!parse_expr(groupField))
}

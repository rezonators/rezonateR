#' Concatenate two strings together with nothing in between.
#'
#' This is intended to be an equivalent of Python +.
#'
#' @param a A character object.
#' @param b A character object.
#'
#' @return The concatenated string.
#' @export
#'
#' @examples "3" %+% "a" #You get "3a"
`%+%` = function(a, b){
  paste0(a, b)
}

flatten_expr = function(x, includeFunct = T){
  if(is.symbol(x) | is_syntactic_literal(x)){
    deparse(x)
  } else if(is.call(x)){
    x_list = as.list(x)
    result = character(0)

    if(!includeFunct){
      start = 2
    } else {
      start = 1
    }
    if(length(x_list) >= start){
        for(i in start:length(x_list)){
          item = x[[i]]
          if(length(as.list(item)) > 1){
            #print("Still some ways to pgo!")
            #print(item)p
            result = c(result, flatten_expr(item))
          } else {
            #print("Reached a bottom!")
            #print(item)
            result = c(result, deparse(item))
          }
        }
    } else {
      result = x_list[[1]]
    }
    result
  } else stop("flatten_expr only handles expressions.")
}

#x: The expression
#old, new: Symbols to replace, as strings
replace_expr_element = function(x, old, new){
  x_list = as.list(x)
  old_expr = parse_expr(old)
  new_expr = parse_expr(new)
  result = character(0)

  for(i in 1:length(x_list)){
    item = x[[i]]
    if(length(as.list(item)) == 1){
      #print("bottom")
      if(item == old_expr) x[[i]] = new_expr
    } else {
      #print("Not bottom")
      x[[i]] = replace_expr_element(x[[i]], old, new)
    }
  }
  x
}


listAt = function(list, address, sep = "/"){
  locations = strsplit(address, sep)[[1]]
  currLoc = list
  for(loc in locations){
    currLoc = currLoc[[loc]]
  }
  currLoc
}

unpackList = function(list){
  parentEnv = env_parent(current_env())
  for(name in names(list)){
    parentEnv[[name]] = list[[name]]
  }
}

#Words: Words you want to chomp the suffix from
#Suffix: A regex
chompSuffix = function(words, suffix){
  if(suffix != ""){
    sapply(words, function(x){
      newWord = x
      if(str_ends(x, suffix)){
        locs = str_locate_all(x, suffix)[[1]]
        suffixStart = locs[nrow(locs),1]
        newWord = substr(x, 1, suffixStart - 1)
      }
      newWord
    })
  } else {
    words
  }
}

last = function(vector){
  vector[length(vector)]
}


checkIfOne = function(args, message = ""){
  for (arg in args){
    e = env_parent(environment())
    if(length(e[[arg]]) > 1){
      warning(message %+% "I am taking the first of the following field: " %+% arg)
      e[[arg]] = e[[arg]][1]
    }
  }
}

multi_intersect = function(vecs){
  if(length(vecs) == 1){
    vecs[[1]]
  } else {
    Reduce(intersect, vecs[2:length(vecs)], vecs[[1]])
  }
}

rbind_list = function(dfs){
  if(length(dfs) == 1){
    dfs[[1]]
  } else {
    dfs = Reduce(rbind, dfs[2:length(dfs)], dfs[[1]])
  }
  dfs
}

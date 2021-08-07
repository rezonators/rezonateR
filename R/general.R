#' Concatenate two strings together with nothing in between.
#'
#' This is intended to be an equivalent of Python +.
#'
#' @param a A character object.
#' @param b A character
#'
#' @return The concatenated string.
#' @export
#'
#' @examples "3" %+% "a"
`%+%` = function(a, b){
  paste0(a, b)
}

flatten_expr = function(x, includeFunct = T){
  x_list = as.list(x)
  result = character(0)

  if(!includeFunct){
    start = 2
  } else {
    start = 1
  }
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
  result
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

#' Import a Rez file
#'
#' Import a Rez file, returning an R object containing a the node map, word grid, unit grid and chain grid.
#'
#' @param path
#'
#' @return rezRObject
#' @export

importRez = function(path){
  rezJSON = rjson::fromJSON(file = path)
  nodeMap = rezJSON[["ROOT"]][[1]][["nodeMap"]]
  wordDF = lapply(rezJSON$ROOT[[1]][["tokenImport"]], unlist) %>% data.frame
  colnames(wordDF) = rezJSON$ROOT[[1]][["tokenImportColNameList"]]
  wordDF = wordDF[-1,]

  returnObj = list(wordDF = wordDF, nodeMap = nodeMap)
  return(returnObj)
}


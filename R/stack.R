#' @rdname stackToX
#' @export
stackToUnit = function(rezrObj, layers = character(0)){
  if(length(layers) == 0) layers = names(rezrObj$stackDF)
  for(layer in layers){
    rezrObj$stackDF[[layer]]  = rezrObj$stackDF[[layer]] %>% arrange(docTokenSeqFirst) %>% rez_mutate(stackSeq = 1:length(docTokenSeqFirst))
    rezrObj = rezrObj %>% addFieldForeign("card", layer, "stack", layer, "chain", paste0(layer,"Seq"), "stackSeq")
    rezrObj = rezrObj %>% addFieldForeign("unit", "", "card", layer, "id", paste0(layer,"Seq"), paste0(layer,"Seq"), sourceKey = "unit")
  }
  rezrObj
}

#' Add stack sequence information to tokens and units
#'
#' @rdname stackToX
#' @param rezrObj The `rezrObj` to modify.
#' @param layers Stack layers to be added to tokens and units.
#'
#' @return A `rezrObj` with stack sequence information added. The name of the column will be the column name followed by "Seq". If `stackToToken()` is called, the information will be added to `unitDF` as well (if the information is not already there). Both methods will add the information to `cardDF` (again, if the information is not already there.
#' @export
#'
#' @examples
stackToToken = function(rezrObj, layers = character(0)){
  if(length(layers) == 0) layers = names(rezrObj$stackDF)

    for(layer in layers){
      if(!(paste0(layer,"Seq") %in% names(rezrObj$unitDF))){
        rezrObj = rezrObj %>% stackToUnit(layers = layer)
      }

    rezrObj = rezrObj %>% addFieldForeign("token", "", "unit", "", "unit", paste0(layer,"Seq"),paste0(layer,"Seq"))
  }
  rezrObj
}

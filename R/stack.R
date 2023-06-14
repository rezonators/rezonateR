stackToUnit = function(rezrObj, layers = character(0)){
  if(length(layers) == 0) layers = names(rezrObj$stackDF)
  for(layer in layers){
    rezrObj$stackDF[[layer]]  = rezrObj$stackDF[[layer]] %>% arrange(docTokenSeqFirst) %>% rez_mutate(stackSeq = 1:length(docTokenSeqFirst))
    rezrObj = rezrObj %>% addFieldForeign("card", layer, "stack", layer, "chain", paste0(layer,"Seq"), "stackSeq")
    rezrObj = rezrObj %>% addFieldForeign("unit", "", "card", layer, "id", paste0(layer,"Seq"), paste0(layer,"Seq"), sourceKey = "unit")
  }
  rezrObj
}

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

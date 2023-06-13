

getGantt = function(rezrObj, x = "token", y = "participant", obj = "unit", thickness = 12, stacking = "Turn"){

  if(x == "token"){
    if(obj == "unit"){
      participants = rezrObj$unitDF$participant %>% unique
      participantCol = sapply(participants, function(x) attr(rezrObj$nodeMap, "smallMaps")$colorMap[[x]]) %>% decToCol
      names(participantCol) = participants
      result = ggplot(rezrObj$unitDF %>% rez_mutate(col = participantCol[participant]), aes(x = docTokenSeqFirst, xend = docTokenSeqLast, y = participant, yend = participant, col = col)) + geom_segment(size = thickness) + theme(legend.position = "none")
    }
  }

  if(x == "unit"){
    if(obj == "stack"){
      turnColours = paste0("#", sapply(names(rezrObj$nodeMap$stack), function(x) rezrObj$nodeMap$stack[[x]]$chainColor)[rezrObj$stackDF[[stacking]]$id] %>% as.hexmode %>% format(width = 6, upper.case = T) %>% as.character)
      result = ggplot(rezrObj$stackDF[[stacking]], aes(x = unitSeqFirst - .2, xend = unitSeqLast + .2, y = participant, yend = participant)) + geom_segment(size = thickness, colour = turnColours) + xlab("unit")
    }
  }
  result
}

decToCol = function(dec){
  paste0("#", dec %>% as.hexmode %>% format(width = 6, upper.case = T) %>% as.character)
}

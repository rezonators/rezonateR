

#' Get Gantt charts from rezrObj.
#'
#' @param rezrObj The rezrObj that the Gantt chart is to be created fromm
#' @param x The unit of the x-axis (token, unit ...)
#' @param y The unit of the y-axis (generally participant, I don't support anything else at the moment)
#' @param obj What does each rectangle on the Gantt chart represent? By default, "unit", but can also be "stack".
#' @param thickness Thickness of the rectangles.
#' @param stacking The stacking that you would like the rectangles to be, if obj = "stack".
#'
#' @return
#' @export
#'
#' @examples
getGantt = function(rezrObj, x = "token", y = "participant", obj = "unit", thickness = 12, stacking = "Turn"){

  if(x == "token"){
    if(obj == "unit"){
      participants = rezrObj$unitDF$participant %>% unique
      participantCol = sapply(participants, function(x) attr(rezrObj$nodeMap, "smallMaps")$colorMap[[x]]) %>% decToCol
      names(participantCol) = participants
      result = ggplot(rezrObj$unitDF %>% rez_mutate(col = participantCol[participant]), aes(x = docTokenSeqFirst, xend = docTokenSeqLast, y = participant, yend = participant, col = col)) + geom_segment(size = thickness) + theme(legend.position = "none")
    } else if(obj == "stack"){
      turnColours = paste0("#", sapply(names(rezrObj$nodeMap$stack), function(x) rezrObj$nodeMap$stack[[x]]$chainColor)[rezrObj$stackDF[[stacking]]$id] %>% as.hexmode %>% format(width = 6, upper.case = T) %>% as.character)
      result = ggplot(rezrObj$stackDF[[stacking]]) + geom_segment(aes(x = docTokenSeqFirst - .5, xend = docTokenSeqLast + .5, y = participant, yend = participant), size = thickness, colour = turnColours, alpha = .4) + xlab("unit") + theme_bw() + theme(panel.grid.major.y = element_blank())
    }
  }

  if(x == "unit"){
    if(obj == "stack"){
      turnColours = paste0("#", sapply(names(rezrObj$nodeMap$stack), function(x) rezrObj$nodeMap$stack[[x]]$chainColor)[rezrObj$stackDF[[stacking]]$id] %>% as.hexmode %>% format(width = 6, upper.case = T) %>% as.character)
      result = ggplot(rezrObj$stackDF[[stacking]]) + geom_segment(aes(x = unitSeqFirst - .5, xend = unitSeqLast + .5, y = participant, yend = participant), size = thickness, colour = turnColours, alpha = .5) + xlab("unit") + theme_bw() + theme(panel.grid.major.y = element_blank())
    }
  }
  result
}

decToCol = function(dec){
  paste0("#", dec %>% as.hexmode %>% format(width = 6, upper.case = T) %>% as.character)
}

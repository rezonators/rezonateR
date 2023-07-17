#' Augment unitDF and cliqueDF with resonance-related information.
#'
#' @param rezrObj The `rezrObj` to be augmented.
#'
#' @return A `rezrObj` with the `unitDF` updated to contain information about the level of resonance that unit is involved in, and `cliqueDF` updated to contain information on whether the clique contains other-resonance.
#' @export
#'
#' @examples
augmentRezInfo = function(rezrObj){
  if(!("unit" %in% names(rezrObj$rezDF$default))){
    if(!("unitSeqFirst" %in% names(rezrObj$rezDF$default))){
      rezrObj = addUnitSeq(rezrObj, entity = "rez", layers = layer)
    }
    if(!("unitSeq" %in% names(rezrObj$rezDF$default))){
      rezrObj$rezDF$default = rezrObj$rezDF$default %>% rez_mutate(unitSeq = unitSeqLast)
    }
    if(!("unitSeqFirst" %in% names(rezrObj$cliqueDF))){
      rezrObj = addUnitSeq(rezrObj, entity = "clique")
    }

    #TODO: If participant column doesn't exist in units ...

    rezrObj = rezrObj %>% addFieldForeign("rez", layer, "unit", "", "unitSeq", "unit", "id", sourceKeyName = "unitSeq")
  }

  rezToUnit = rezrObj$rezDF$default$unit
  names(rezToUnit) = rezrObj$rezDF$default$id

  resonanceToUnits = lapply(rezrObj$nodeMap[["resonance"]], function(node){
    rezToUnit[node$setIDList]
  })

  unitsToResonances = lapply(rezrObj$unitDF$id, function(unit){
    names(resonanceToUnits)[sapply(resonanceToUnits, function(x) unit %in% x)]
  })
  names(unitsToResonances) = rezrObj$unitDF$id

  unitsWithResonance = rezrObj$unitDF$id[sapply(1:length(rezrObj$unitDF$id), function(x) length(unitsToResonances[[x]]) > 0)]

  rezLevel = sapply(unitsToResonances, length)
  rezLevelMA = augZoo(rezLevel, 5) %>% rollapply(5, mean)
  rezrObj$unitDF = rezrObj$unitDF %>% rez_mutate(rezLevel, rezLevelMA,
                                                 length = docTokenSeqLast - docTokenSeqFirst + 1, rezLevelNorm = rezLevel / length)

  unitToParticipant = unitDF$participant
  names(unitToParticipant) = unitDF$id
  selfOnly = sapply(rezrObj$nodeMap$clique, function(x) length(unique(unitToParticipant[x$unitList])) == 1)
  rezrObj$cliqueDF = rezrObj$cliqueDF %>% rez_mutate(selfOnly)

  rezrObj
}

getNumRezLinksGrid = function(rezrObj, layer = "default"){
  if(!("unit" %in% names(rezrObj$rezDF$default))){
    if(!("unitSeqFirst" %in% names(rezrObj$rezDF$default))){
      rezrObj = addUnitSeq(rezrObj, entity = "rez", layers = layer)
    }
    if(!("unitSeq" %in% names(rezrObj$rezDF$default))){
      rezrObj$rezDF$default = rezrObj$rezDF$default %>% rez_mutate(unitSeq = unitSeqLast)
    }
    if(!("unitSeqFirst" %in% names(rezrObj$cliqueDF))){
      rezrObj = addUnitSeq(rezrObj, entity = "clique")
    }



    rezrObj = rezrObj %>% addFieldForeign("rez", layer, "unit", "", "unitSeq", "unit", "id", sourceKeyName = "unitSeq")
  }

  rezToUnit = rezrObj$rezDF$default$unit
  names(rezToUnit) = rezrObj$rezDF$default$id

  resonanceToUnits = lapply(rezrObj$nodeMap[["resonance"]], function(node){
    rezToUnit[node$setIDList]
  })

  unitsToResonances = lapply(rezrObj$unitDF$id, function(unit){
    names(resonanceToUnits)[sapply(resonanceToUnits, function(x) unit %in% x)]
  })
  names(unitsToResonances) = rezrObj$unitDF$id

  unitsWithResonance = rezrObj$unitDF$id[sapply(1:length(rezrObj$unitDF$id), function(x) length(unitsToResonances[[x]]) > 0)]

  #Pairs plot
  pairs = purrr::cross_df(data.frame(unitsWithResonance, unitsWithResonance))
  colnames(pairs) = c("unit1", "unit2")
  pairs = pairs %>% filter(unit1 != unit2)
  pairs = pairs %>% left_join(rezrObj$unitDF %>% select(id, unitSeq), by = c(unit1 = "id")) %>% rename(unitSeq1 = unitSeq) %>%
    left_join(rezrObj$unitDF %>% select(unitSeq, id), by = c(unit2 = "id")) %>% rename(unitSeq2 = unitSeq)

  countCommonResonances = function(unit1s, unit2s){
    sapply(1:length(unit1s), function(i)
    intersect(unitsToResonances[[unit1s[i]]], unitsToResonances[[unit2s[i]]]) %>% length
    )
  }
  pairs = pairs %>% mutate(commonResonances = countCommonResonances(unit1, unit2))
  pairs = pairs %>% filter(commonResonances > 0)
  pairs = pairs %>% left_join(rezrObj$unitDF %>% select(id, participant), by = c(unit1= "id")) %>% rename(participant1 = participant) %>%
    left_join(rezrObj$unitDF %>% select(id, participant), by = c(unit2= "id")) %>% rename(participant2 = participant) %>%
    mutate(col = case_when(participant1 == participant2 ~ "#FFFF00",
                          T ~ "#00FFFF"))
  ggplot(pairs) + geom_tile(aes(x = unitSeq1, y = unitSeq2, alpha = commonResonances, fill = col)) + scale_alpha_continuous(range=c(.7,1)) + geom_rect(data = rezrObj$cliqueDF, aes(xmin = unitSeqFirst, xmax = unitSeqLast, ymin = unitSeqFirst, ymax = unitSeqLast))

  #second attempt
  # library(cowplot)
  rezLevel = sapply(unitsToResonances, length)
  rezLevelMA = augZoo(rezLevel, 5) %>% rollapply(5, mean)
  rezrObj$unitDF = rezrObj$unitDF %>% rez_mutate(rezLevel, rezLevelMA,
                length = docTokenSeqLast - docTokenSeqFirst + 1, rezLevelNorm = rezLevel / length)


  unitToParticipant = unitDF$participant
  names(unitToParticipant) = unitDF$id
  selfOnly = sapply(rezrObj$nodeMap$clique, function(x) length(unique(unitToParticipant[x$unitList])) == 1)
  rezrObj$cliqueDF = rezrObj$cliqueDF %>% rez_mutate(selfOnly)

  a = getGantt(sbc007_turn, x = "unit", obj = "stack", stacking = "Turn", thickness = 6) + theme(axis.title = element_blank(), axis.ticks.x.bottom = element_blank(), axis.text.x.bottom = element_blank())+
    theme(plot.margin = unit(c(.1,.1,0,.1), "cm"))
  b = ggplot(rezrObj$unitDF) + geom_smooth(data = rezrObj$unitDF, aes(x = 1:length(rezLevel), y = rezLevelNorm), se = FALSE, span = .2)  + theme_bw()+ theme(axis.title = element_blank()) + scale_y_continuous(breaks = c(0,1), limits = c(0, 1))+
    theme(plot.margin = unit(c(0,.1,0,.1), "cm")) + geom_point(data = rezrObj$unitDF, aes(x = 1:length(rezLevel), y = rezLevelNorm), size = .3)  + theme_bw()+ theme(axis.title = element_blank(), axis.ticks.x.bottom = element_blank(), axis.text.x.bottom = element_blank())
  # c = ggplot(data = rezrObj$cliqueDF %>% mutate(chainCountPolar = case_when(selfOnly ~ chainCount, T ~ -chainCount)))  + geom_segment(aes(y = (cliqueSeq-1) %% 5, yend = (cliqueSeq-1) %% 5, x = unitSeqFirst, xend = unitSeqLast, col = chainCountPolar), size = 3) + scale_colour_gradient2(low = "red", high = "green", mid = "white", midpoint = 0) + theme_bw() + theme(axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "none", axis.ticks = element_blank())+
  #   theme(plot.margin = unit(c(0,.1,0,.1), "cm"))
  c = ggplot(data = rezrObj$cliqueDF)  + geom_segment(aes(y = (cliqueSeq-1) %% 5, yend = (cliqueSeq-1) %% 5, x = unitSeqFirst - .3, xend = unitSeqLast + .3, col = selfOnly, alpha = chainCount), size = 5.5) + scale_alpha_continuous(range=c(.4, 1))  + theme_bw() + theme(axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "none", axis.ticks = element_blank())+
    theme(plot.margin = unit(c(0,.1,0,.1), "cm"))



  # c = ggplot(rezrObj$unitDF) + geom_point(data = rezrObj$unitDF, aes(x = 1:length(rezLevel), y = rezLevel), size = .3)  + theme_bw()+ theme(axis.title = element_blank()) + scale_y_continuous(breaks = c(0,1,2))+
  #   theme(plot.margin = unit(c(0,.1,0,.1), "cm"))+ geom_smooth(data = rezrObj$unitDF, aes(x = 1:length(rezLevel), y = rezLevel), se = FALSE, span = .2)

  library(patchwork)
  a / b / c
  # plot_grid(a, b, ncol = 1, axis = "tblr", align = "hv")
}

augZoo = function(zoo, windowSize = 10, mode = "zero"){
  if(mode=="zero"){
    zoo = c(rep(0, windowSize-1), zoo)
  } else if(mode == "same"){
    zoo = c(rep(zoo[1], windowSize-1), zoo)
  }
  zoo
}


#' Find resonances.between two sets of units.
#'
#' @param rezrObj The rezrObj.
#' @param units_1 A list of vectors of units.
#' @param units_2 A list of vectors of units, same length as units_1.
#' @param rez_layer The resonance layer you're considering.
#'
#' @return A vector containing the number of resonances between the first vector of units_1 and first vector of units_2, second vector of units_1 and second vector of units_2, etc.
#' @export
#'
#' @examples
findResonancesBetween = function(rezrObj, units_1, units_2, rez_layer = "default"){
  #If there's *no* unitSeq in rezDF, add it
  if(!("unitSeqLast" %in% names(rezrObj$rezDF[[rez_layer]]))){
    rezrObj = addUnitSeq(rezrObj, "rez", "default")
  }

  sapply(1:length(units_1), function(i){
    unitSeq_1 = rezrObj$unitDF$unitSeq[(rezrObj$unitDF$id %in% units_1[[i]])]
    unitSeq_2 = rezrObj$unitDF$unitSeq[(rezrObj$unitDF$id %in% units_2[[i]])]

    chains_1 = rezrObj$rezDF$default %>% filter(unitSeqFirst %in% unitSeq_1) %>% pull(chain)
    chains_2 = rezrObj$rezDF$default %>% filter(unitSeqFirst %in% unitSeq_2) %>% pull(chain)
    length(intersect(chains_1, chains_2))
  })
}

test_that("getTurnFromAnnos works", {
  discoName = "sbc007_turns"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  sbc007_turn = importRez(path,
                       concatFields = c("transcript", "text"))
  sbc007_turn = addUnitSeq(sbc007_turn, "token")

  sbc007_turn = sbc007_turn %>% addFieldForeign("token", "", "unit", "", "unit", "participant", "participant")

  sbc007_turn = sbc007_turn %>% addFieldForeign("card", "Turn", "unit", "", "unit", "participant", "participant")
  sbc007_turn = sbc007_turn %>% addFieldForeign("stack", "Turn", "card", "Turn", "card", "participant", "participant", type = "complex", complexAction = function(x) paste0(unique(x), collapse = ","))

 sbc007_turn = sbc007_turn %>% addFieldForeign("card", "TCU", "unit", "", "unit", "participant", "participant")
  sbc007_turn = sbc007_turn %>% addFieldForeign("stack", "TCU", "card", "TCU", "card", "participant", "participant", type = "complex", complexAction = function(x) paste0(unique(x), collapse = ","))
  sbc007_turn$stackDF$TCU = sbc007_turn$stackDF$TCU %>% filter(name != "Paralinguistic")
  getGantt(sbc007_turn, x = "unit", obj = "stack", stacking = "Turn") + theme(axis.title = element_blank())
  getGantt(sbc007_turn, x = "token", obj = "stack", stacking = "Turn")+ theme(axis.title = element_blank()) + geom_point(data = sbc007_turn$tokenDF %>% filter(tolower(text) %in% c("like", "oh")) %>% mutate(docTokenSeqLast = docTokenSeq, docTokenSeqFirst = docTokenSeq), aes(y = participant, x = docTokenSeq, col = tolower(text)), shape = "x", size = 3) + theme(legend.position = "none")

  sbc007_turn = sbc007_turn %>% addFieldForeign("unit", "", "clique", "", "inClique", "resonanceDegreeChains", "chainCount")
  sbc007_turn = sbc007_turn %>% addFieldForeign("unit", "", "clique", "", "inClique", "resonanceDegreeLinks", "linkCount")
  getGantt(sbc007_turn, x = "unit", obj = "stack", stacking = "Turn") + theme(axis.title = element_blank()) +
    geom_path(data = sbc007_turn$unitDF %>% filter(inClique != "") %>% rez_mutate(unitSeqFirst = unitSeq, unitSeqLast = unitSeq), mapping = aes(y = participant, x = unitSeq, group = inClique, col = inClique)) +
    geom_point(data = sbc007_turn$unitDF %>% filter(inClique != "") %>% rez_mutate(unitSeqFirst = unitSeq, unitSeqLast = unitSeq), mapping = aes(y = participant, x = unitSeq, group = inClique, col = inClique), size = .5) + theme(legend.position = "none")

  sbc007_turn = addUnitSeq(sbc007_turn, entity = "clique")
  sbc007_turn$cliqueDF = sbc007_turn$cliqueDF  %>% rez_mutate(cliqueSeq = rank(unitSeqFirst))
  ggplot(data = sbc007_turn$cliqueDF) + geom_segment(aes(y = cliqueSeq %% 5, yend = cliqueSeq %% 5, x = unitSeqFirst, xend = unitSeqLast, col = chainCount), size = 2) + theme_bw() + theme(axis.title = element_blank(), axis.text.y = element_blank(), legend.position = "none", axis.ticks = element_blank())



 })

test_that("getTurnFromAnnos works", {
  discoName = "sbc002-turn"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  turnText = importRez(path,
                    concatFields = c("transcript", "text"))

  lastMarkedIU = 104
  turnTextUnits = turnText$unitDF %>% filter(as.numeric(unitId) <= lastMarkedIU)
  turnTextUnits = turnTextUnits %>% rez_mutate(
    turnId = getTurnFromAnnos(turnTextUnits, c(noTurn = "nonTurn", noTurnStart = "nonTurnStart", noTurnEnd = "nonTurnEnd"))
  )
})

test_that("getTurnFromStack works", {
  discoName = "SBC015_edit"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  turnText = importRez(path,
                       concatFields = c("transcript", "text"))


  turnText = turnText %>% addUnitSeq("stack")
  turnTextMod = turnText %>% addFieldForeign("card", "", "unit", "", "unit", "docTokenSeqFirst", "docTokenSeqFirst") %>% addFieldForeign("card", "", "unit", "", "unit", "docTokenSeqLast", "docTokenSeqLast")
  turnTextMod = turnTextMod %>% addFieldForeign("stack", "", "card", "", "card","docTokenSeqFirst", "docTokenSeqFirst", type = "complex", complexAction = min) %>% addFieldForeign("stack", "", "card", "", "card", "docTokenSeqLast",  "docTokenSeqLast", type = "complex", complexAction = max)
  turnText = turnText %>% addFieldForeign("card", "", "unit", "", "unit", "participant", "participant")
  turnText = turnText %>% addFieldForeign("stack", "", "card", "", "card", "participant", "participant", type = "complex", complexAction = function(x) paste0(unique(x), collapse = ","))
})

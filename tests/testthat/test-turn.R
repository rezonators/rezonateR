test_that("importRez works", {
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

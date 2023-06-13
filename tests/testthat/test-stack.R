test_that("getTurnFromAnnos works", {
  discoName = "sbc007_turns"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  sbc007_turn = importRez(path,
                       concatFields = c("transcript", "text"))
  sbc007_turn = addUnitSeq(sbc007_turn, "stack")


  sbc007_turn = sbc007_turn %>% addFieldForeign("card", "Turn", "unit", "", "unit", "participant", "participant")
  sbc007_turn = sbc007_turn %>% addFieldForeign("stack", "Turn", "card", "Turn", "card", "participant", "participant", type = "complex", complexAction = function(x) paste0(unique(x), collapse = ","))

  sbc007_turn = sbc007_turn %>% addFieldForeign("card", "TCU", "unit", "", "unit", "participant", "participant")
  sbc007_turn = sbc007_turn %>% addFieldForeign("stack", "TCU", "card", "TCU", "card", "participant", "participant", type = "complex", complexAction = function(x) paste0(unique(x), collapse = ","))
  sbc007_turn$stackDF$TCU = sbc007_turn$stackDF$TCU %>% filter(name != "Paralinguistic")

})

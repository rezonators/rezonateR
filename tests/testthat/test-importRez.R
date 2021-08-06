test_that("importRez works", {
  discoName = "three-parting-2569_new"
  path = "C:/Users/User/Documents/GitHub/lhasa-reference-tracking/shanti/3_2_rez_file/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))))
  expect_s3_class(rezEx[["tokenDF"]], "data.frame")
  expect_type(rezEx[["nodeMap"]], "list")
})

test_that("rezrDF modification works", {
  discoName = "three-parting-2569_new"
  path = "C:/Users/User/Documents/GitHub/lhasa-reference-tracking/shanti/3_2_rez_file/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))))
  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% rez_mutate(fieldaccess = "flex", word2 = word %+% ", lol.")

  #A bunch of auto stuff to test out updating.
  #First local updates.
  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% rez_mutate(fieldaccess = "auto", word3 = wordWylie %+% ", lol.")
  updateFunct(rezEx[["tokenDF"]], "word3") = createUpdateFunction(tokenDF, word3, wordWylie %+% ", lol.")
  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% rez_mutate(fieldaccess = "auto", word4 = word3 %+% ", lol.")
  updateFunct(rezEx[["tokenDF"]], "word4") = createUpdateFunction(tokenDF, word4, word3 %+% ", lol.")

  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% rez_mutate(fieldaccess = "auto", word5 = word3 %+% " & " %+% word4 %+% " & " %+% wordWylie)
  updateFunct(rezEx[["tokenDF"]], "word5") = createUpdateFunction(tokenDF, word5, word3 %+% " & " %+% word4 %+% " & " %+% wordWylie)

  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% mutate(word3 = "hohoho", word4 = "hahaha", word5 = "hihihi")
  rezEx[["tokenDF"]] = reload(rezEx[["tokenDF"]], "word3")
  rezEx[["tokenDF"]] = reload(rezEx[["tokenDF"]], "word4")
  rezEx[["tokenDF"]] = reload(rezEx[["tokenDF"]], "word5")
  expect(rezEx[["tokenDF"]]$word3[1] != "hohoho", failure_message = "Reload failed.")
  expect(rezEx[["tokenDF"]]$word4[1] != "hahaha", failure_message = "Reload failed.")
  expect(rezEx[["tokenDF"]]$word5[1] != "hihihi", failure_message = "Reload failed.")

  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% mutate(word3 = "hohoho", word4 = "hahaha", word5 = "hihihi")
  rezEx[["tokenDF"]] = reload(rezEx[["tokenDF"]])
  expect(rezEx[["tokenDF"]]$word3[1] != "hohoho", failure_message = "Reload failed.")
  expect(rezEx[["tokenDF"]]$word4[1] != "hahaha", failure_message = "Reload failed.")
  expect(rezEx[["tokenDF"]]$word5[1] != "hihihi", failure_message = "Reload failed.")

  #Then foreign updates.
  updateFunct(rezEx[["entryDF"]], "word") = createLeftJoinUpdate(rezEx[["entryDF"]], rezEx, "tokenDF/word", "token")
  updateFunct(rezEx[["entryDF"]], "wordWylie") = createLeftJoinUpdate(rezEx[["entryDF"]], rezEx, "tokenDF/wordWylie", "token")
  rezEx[["entryDF"]] = rezEx[["entryDF"]] %>% mutate(word = "hahaha", wordWylie = "hohoho")
  rezEx[["entryDF"]] = reloadForeign(rezEx[["entryDF"]], rezEx, c("word", "wordWylie"))
  expect(rezEx[["entryDF"]]$wordWylie[1] != "hohoho", failure_message = "Reload failed.")
  expect(rezEx[["entryDF"]]$word[1] != "hahaha", failure_message = "Reload failed.")

  rezEx[["entryDF"]] = rezEx[["entryDF"]] %>% mutate(word = "hahaha", wordWylie = "hohoho")
  rezEx[["entryDF"]] = reloadForeign(rezEx[["entryDF"]], rezEx)
  expect(rezEx[["entryDF"]]$wordWylie[1] != "hohoho", failure_message = "Reload failed.")
  expect(rezEx[["tokenDF"]]$word[1] != "hahaha", failure_message = "Reload failed.")

  updateFunct(rezEx[["trackDF"]][["refexpr"]], "word") = createLeftJoinUpdate(rezEx[["trackDF"]][["refexpr"]], rezEx, address = c("tokenDF/tokenSeq", "chunkDF/refexpr/tokenSeqFirst"), fkey = "token", field = "tokenSeqFirst")
  rezEx[["trackDF"]][["refexpr"]] = rezEx[["trackDF"]][["refexpr"]] %>% mutate(tokenSeqFirst = 120)
  rezEx[["trackDF"]][["refexpr"]] = reloadForeign(rezEx[["trackDF"]][["refexpr"]], rezEx)
  expect(rezEx[["trackDF"]][["refexpr"]]$tokenSeqFirst[1] != 120, failure_message = "Reload failed.")



})

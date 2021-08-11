test_that("rezrDF modification works", {
  discoName = "three-parting-2569_new"
  path = "C:/Users/User/Documents/GitHub/lhasa-reference-tracking/shanti/3_2_rez_file/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))))
  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% rez_mutate(fieldaccess = "flex", word2 = word %+% ", lol.")

  #A bunch of auto stuff to test out updating.
  #First local updates.
  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% rez_mutate(fieldaccess = "auto", word3 = wordWylie %+% ", lol.")
  updateFunct(rezEx[["tokenDF"]], "word3") = createUpdateFunction(word3, wordWylie %+% ", lol.", rezEx[["tokenDF"]])
  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% rez_mutate(fieldaccess = "auto", word4 = word3 %+% ", lol.")
  updateFunct(rezEx[["tokenDF"]], "word4") = createUpdateFunction(word4, word3 %+% ", lol.", rezEx[["tokenDF"]])

  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% rez_mutate(fieldaccess = "auto", word5 = word3 %+% " & " %+% word4 %+% " & " %+% wordWylie)
  updateFunct(rezEx[["tokenDF"]], "word5") = createUpdateFunction(word5, word3 %+% " & " %+% word4 %+% " & " %+% wordWylie, rezEx[["tokenDF"]])

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
  updateFunct(rezEx[["entryDF"]], "word") = createLeftJoinUpdate("tokenDF/word", "token", "word")
  updateFunct(rezEx[["entryDF"]], "wordWylie") = createLeftJoinUpdate("tokenDF/wordWylie", "token", "wordWylie")
  rezEx[["entryDF"]] = rezEx[["entryDF"]] %>% mutate(word = "hahaha", wordWylie = "hohoho")
  rezEx[["entryDF"]] = reloadForeign(rezEx[["entryDF"]], rezEx, c("word", "wordWylie"))
  expect(rezEx[["entryDF"]]$wordWylie[1] != "hohoho", failure_message = "Reload failed.")
  expect(rezEx[["entryDF"]]$word[1] != "hahaha", failure_message = "Reload failed.")

  rezEx[["entryDF"]] = rezEx[["entryDF"]] %>% mutate(word = "hahaha", wordWylie = "hohoho")
  rezEx[["entryDF"]] = reloadForeign(rezEx[["entryDF"]], rezEx)
  expect(rezEx[["entryDF"]]$wordWylie[1] != "hohoho", failure_message = "Reload failed.")
  expect(rezEx[["tokenDF"]]$word[1] != "hahaha", failure_message = "Reload failed.")

  updateFunct(rezEx[["trackDF"]][["refexpr"]], "word") = createLeftJoinUpdate(address = c("tokenDF/word", "chunkDF/refexpr/word"), fkey = "token", field = "tokenSeqFirst")
  rezEx[["trackDF"]][["refexpr"]] = rezEx[["trackDF"]][["refexpr"]] %>% mutate(tokenSeqFirst = 120)
  rezEx[["trackDF"]][["refexpr"]] = reloadForeign(rezEx[["trackDF"]][["refexpr"]], rezEx)
  expect(rezEx[["trackDF"]][["refexpr"]]$tokenSeqFirst[1] != 120, failure_message = "Reload failed.")

  updateFunct(rezEx[["chunkDF"]][["refexpr"]], "word") = createLowerToHigherUpdate(address = "tokenDF/word", fkeyAddress = "chunk/tokenList", action = function(x) paste(x, collapse = ""), field = "word", fkeyInDF = FALSE, seqName = "discourseTokenSeq")
  rezEx[["chunkDF"]][["refexpr"]] = rezEx[["chunkDF"]][["refexpr"]] %>% mutate(word = "hahaha")
  rezEx[["chunkDF"]][["refexpr"]] = reloadForeign(rezEx[["chunkDF"]][["refexpr"]], rezEx, c("word"))
  expect(rezEx[["chunkDF"]][["refexpr"]]$word[1] !="hahaha", failure_message = "Reload failed.")

  updateFunct(rezEx[["chunkDF"]][["refexpr"]], "word3") = createUpdateFunction(word3, word %+% ", lol.", rezEx[["chunkDF"]][["refexpr"]])
  fieldaccess(rezEx[["chunkDF"]][["refexpr"]], "word3") = "auto"
  fieldaccess(rezEx[["chunkDF"]][["refexpr"]], "word") = "foreign"
  rezEx[["chunkDF"]][["refexpr"]] = rezEx[["chunkDF"]][["refexpr"]] %>% mutate(word3 = "hohoho")
  rezEx[["chunkDF"]][["refexpr"]] = rezEx[["chunkDF"]][["refexpr"]] %>% mutate(word = "hahaha")
  rezEx[["chunkDF"]][["refexpr"]] = reload(rezEx[["chunkDF"]][["refexpr"]], rezEx)
  expect(rezEx[["chunkDF"]][["refexpr"]]$word[1] !="hahaha", failure_message = "Reload failed.")
  expect(rezEx[["chunkDF"]][["refexpr"]]$word3[1] !="hohoho", failure_message = "Reload failed.")

  #Automatic adding of update functions from rez_left_join and lowerToHigher
  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% rez_left_join(rezEx[["unitDF"]] %>% rez_select(id, unitSeq), by = c(unit = "id"), df2Address = "unitDF", fkey = "unit")
  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% mutate(unitSeq = 0)
  rezEx[["tokenDF"]] = reload(rezEx[["tokenDF"]], rezEx)
  expect(rezEx[["tokenDF"]]$unitSeq[1] != 0, failure_message = "Reload failed.")

  rezEx[["unitDF"]] = lowerToHigher(complexDF = rezEx[["unitDF"]], fieldnames = "word", higherFieldnames = "longestWordLength", action = function(x) max(nchar(x)), simpleDFAddress = "entryDF", complexNodeMapAddress = "unit", rezrObj = rezEx, tokenListName = "entryList")
  rezEx[["unitDF"]]$longestWordLength = -1
  rezEx[["unitDF"]] = reloadForeign(rezEx[["unitDF"]], rezEx)
  expect(rezEx[["unitDF"]]$longestWordLength[1] != -1, failure_message = "Reload failed.")

  #Inherent reload functions from import
  rezEx[["entryDF"]] = rezEx[["entryDF"]] %>% mutate(word = "hahaha") %>% reload(rezEx)
  expect(rezEx[["entryDF"]]$word[1] != "hahaha", failure_message = "Reload failed.")

  rezEx[["unitDF"]] = rezEx[["unitDF"]] %>% mutate(discourseTokenSeqLast = -1, word = "hahaha", lit = "hohoho") %>% reload(rezEx)
  expect(rezEx[["unitDF"]]$discourseTokenSeqLast[1] != -1, failure_message = "Reload failed.")
  expect(rezEx[["unitDF"]]$word[1] != "hahaha", failure_message = "Reload failed.")
  expect(rezEx[["unitDF"]]$lit[1] != "hohoho", failure_message = "Reload failed.")

  rezEx[["chunkDF"]][["adv"]] = rezEx[["chunkDF"]][["adv"]] %>% mutate(discourseTokenSeqLast = -1, word = "hahaha", lit = "hohoho") %>% reload(rezEx)
  expect(rezEx[["chunkDF"]][["adv"]]$discourseTokenSeqLast[1] != -1, failure_message = "Reload failed.")
  expect(rezEx[["chunkDF"]][["adv"]]$word[1] != "hahaha", failure_message = "Reload failed.")
  expect(rezEx[["chunkDF"]][["adv"]]$lit[1] != "hohoho", failure_message = "Reload failed.")

  rezEx[["trackDF"]][["refexpr"]] = rezEx[["trackDF"]][["refexpr"]] %>% mutate(tokenSeqLast = -1, wordWylie = "hahaha", lit = "hohoho") %>% reload(rezEx)
  expect(rezEx[["trackDF"]][["refexpr"]]$tokenSeqLast[1] != -1, failure_message = "Reload failed.")
  expect(rezEx[["trackDF"]][["refexpr"]]$wordWylie[1] != "hahaha", failure_message = "Reload failed.")
  expect(rezEx[["trackDF"]][["refexpr"]]$lit[1] != "hohoho", failure_message = "Reload failed.")
})


test_that("Simple rezrDF operation commands", {
  discoName = "three-parting-2569_new"
  path = "C:/Users/User/Documents/GitHub/lhasa-reference-tracking/shanti/3_2_rez_file/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))))
  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% rez_mutate(fieldaccess = "flex", word2 = word %+% ", lol.")

  a = rezEx$tokenDF %>% addFieldLocal("word6", word %+% "!!!", fieldaccess = "auto") %>% mutate(word6 = "?") %>% reloadLocal()
  updateFunct(a, "word6")(a) %>% pull(word6)
  expect(a$word6[1] != "?", failure_message = "Reload failed.")
  a = rezEx$tokenDF %>% changeFieldLocal("word6", word %+% "???", fieldaccess = "auto") %>% mutate(word6 = "?") %>% reloadLocal()
  expect(a$word6[1] != "?", failure_message = "Reload failed.")

  a = rezEx %>% addFieldLocal(entity = "token", layer = "", fieldName = "word6", expression = word %+% "!!!", fieldaccess = "auto")
  a$tokenDF = a$tokenDF %>% mutate(word6 = "?") %>% reloadLocal()
  expect(a$tokenDF$word6[1] != "?", failure_message = "Reload failed.")

  a = a %>% changeFieldLocal(entity = "token", layer = "", fieldName = "word6", expression = word %+% "???", fieldaccess = "auto")
  a$tokenDF = a$tokenDF %>% mutate(word6 = "?") %>% reloadLocal()
  updateFunct(a$tokenDF, "word6")(a$tokenDF) %>% pull(word6)
  a$tokenDF %>% pull(word6)
  expect(a$tokenDF$word6[1] != "?", failure_message = "Reload failed.")



  a = rezEx$chunkDF$refexpr %>% rez_rename(mot = word)
  expect("mot" %in% names(updateFunct(a)), "Rename failed.")
  expect("mot" %in% names(fieldaccess(a)), "Rename failed.")


  a = rezEx$tokenDF %>% addFieldForeign(rezEx$unitDF, "unit", "unitWord", "word") %>% changeFieldForeign(rezEx$unitDF, "unit", "unitWord", "word")

  a = rezEx$unitDF %>% addFieldForeign(rezEx$entryDF, targetForeignKeyName = "entryList", targetFieldName = "biggestWord", sourceFieldName = "word", type = "complex", fieldaccess = "flex", complexAction = longest, targetNodeMap = rezEx$nodeMap$unit) %>% changeFieldForeign(rezEx$entryDF, targetForeignKeyName = "entryList", targetFieldName = "biggestWord", sourceFieldName = "word", type = "complex", fieldaccess = "flex", complexAction = longest, targetNodeMap = rezEx$nodeMap$unit)

})


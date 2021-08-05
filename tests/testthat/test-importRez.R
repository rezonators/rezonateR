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
  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% rez_mutate(fieldaccess = "auto", word3 = "hahaha")
  updateFunct(rezEx[["tokenDF"]], "word3") = createUpdateFunction(tokenDF, word3, wordWylie %+% ", lol.")
  rezEx[["tokenDF"]] = rezEx[["tokenDF"]] %>% mutate(word3 = "hohoho")
  reload(rezEx[["tokenDF"]], "word3")

  expect_type(rezEx[["tokenDF"]]$word2, "character")
})

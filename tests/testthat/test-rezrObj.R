test_that("Track functions work", {
  discoName = "three-parting-2569_new"
  path = "C:/Users/User/Documents/GitHub/lhasa-reference-tracking/shanti/3_2_rez_file/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))))

  getCombinedChunks(rezEx)
})

test_that("rezrObj save/load", {
  discoName = "three-parting-2569_new"
  path = system.file("inst/extData", discoName %+% "rez", package = "rezonateR")
  rezEx = importRez(path, layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))))

  a = rezEx
  rez_save(a, "inst/extdata/rezEx.Rdata")
  rm(a)
  a = rez_load("inst/extdata/rezEx.Rdata")
  expect_s3_class(a, "rezrObj")
})

test_that("CSV import/export", {
  discoName = "three-parting-2569_new"
  path = "C:/Users/User/Documents/GitHub/lhasa-reference-tracking/shanti/3_2_rez_file/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))))

  rez_write_csv(rezEx$tokenDF, "inst/extdata/token.csv")
  b = rez_read_csv("inst/extdata/chain.csv", inclCols = c("id", "tokenSeq"))
  expect_type(b$id, "character")
  expect_type(b$tokenSeq, "numeric")
  b = rez_read_csv("inst/extdata/chain.csv", a$trackDF, exclCols = c("unit", "tokenSeq"))
  expect(!("unit" %in% names(b)), "Exclusion failed")
})


test_that("getTrackTokens", {
  a = rez_load("inst/extdata/rezEx.Rdata")
  a$trackDF$refexpr %>% rez_mutate(a = getTrackTokens(myRez, "word", myRez$trackDF$refexpr)) %>% select(word, a)
})

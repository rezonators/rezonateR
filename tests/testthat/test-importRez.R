test_that("importRez works", {
  discoName = "three-parting-2569_new"
  path = "C:/Users/User/Documents/GitHub/lhasa-reference-tracking/shanti/3_2_rez_file/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))), concatFields = c("word", "wordWylie", "lit"))
  expect_s3_class(rezEx, "rezrObj")
  expect_s3_class(rezEx[["tokenDF"]], "rezrDF")
  expect_s3_class(rezEx[["tokenDF"]], "data.frame")
  expect_type(rezEx[["nodeMap"]], "list")
})


test_that("importRez works on 0.20.0", {
  discoName = "parting_0.20"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "trailLayer", regex = c("clausearg", "discdeix"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))), concatFields = c("word", "wordWylie", "lit"))
  expect_s3_class(rezEx, "rezrObj")
  expect_s3_class(rezEx[["tokenDF"]], "rezrDF")
  expect_s3_class(rezEx[["tokenDF"]], "data.frame")
  expect_type(rezEx[["nodeMap"]], "list")
})

test_that("importRez works on unannotated doc", {
  skip_on_cran()
  discoName = "sbc010_rez_v2.02"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  rezEx = importRez(path, concatFields = c("text"), separator = " ")
  expect_s3_class(rezEx, "rezrObj")
  expect_s3_class(rezEx[["tokenDF"]], "rezrDF")
  expect_s3_class(rezEx[["tokenDF"]], "data.frame")
  expect_type(rezEx[["nodeMap"]], "list")
})

test_that("Tree node map", {
  importNodeMap = rjson::fromJSON(file = path)[["ROOT"]][[1]][["nodeMap"]]
  a = nodeMap(importNodeMap, "three-parting-2569")
  expect("treeEntry" %in% names(a), "Tree import failed.")
  expect("tree" %in% names(a), "Tree import failed.")

  discoName = "parting_0.20.1"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr")))
  rezEx = importRez(path, layerRegex = layerRegex, concatFields = c("word", "wordWylie", "lit"))
  expect("treeEntryDF" %in% names(rezEx), "Tree import failed.")
  expect("treeDF" %in% names(rezEx), "Tree import failed.")
})

test_that("Tree node map", {
  discoName = "parting_0.20.1"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr")))
  rezEx = importRez(path, layerRegex = layerRegex, concatFields = c("word", "wordWylie"))
})


test_that("Clips", {
  discoName = "Stack 2"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))), concatFields = c("word", "wordWylie"))

  rez_write_csv(rezEx$tokenDF, "inst/extdata/token.csv")
  b = rez_read_csv("inst/extdata/chain.csv", inclCols = c("id", "tokenSeq"))
  expect_type(b$id, "character")
  expect_type(b$tokenSeq, "numeric")
  b = rez_read_csv("inst/extdata/chain.csv", a$trackDF, exclCols = c("unit", "tokenSeq"))
  expect(!("unit" %in% names(b)), "Exclusion failed")
})

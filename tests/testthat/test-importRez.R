test_that("importRez works", {
  discoName = "three-flirt-2501"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  rezEx = importRez(path,
                    layerRegex = list(
                      track = list(field = "trailLayer", regex = c("clausearg", "discdeix", "mandem"), names = c("clausearg", "discdeix", "mandem", "refexpr")),
                      chunk = list(field = "chunkLayer", regex = c("verb", "adv", "clause", "predadj"), names = c("verb", "adv", "clause", "predadj", "refexpr"))),
                    concatFields = c("word", "wordWylie", "lit"))
  expect_s3_class(rezEx, "rezrObj")
  expect_s3_class(rezEx[["tokenDF"]], "rezrDF")
  expect_s3_class(rezEx[["tokenDF"]], "data.frame")
  expect("treeEntryDF" %in% names(rezEx), "Tree import failed.")
  expect("treeDF" %in% names(rezEx), "Tree import failed.")
  expect_type(rezEx[["nodeMap"]], "list")


  a = rezEx
  rez_save(a, "inst/extdata/rezEx.Rdata")
  rm(a)
  a = rez_load("inst/extdata/rezEx.Rdata")
  expect_s3_class(a, "rezrObj")
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

#TODO: Update this
test_that("Clips", {
  discoName = "Stack 2"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))), concatFields = c("word", "wordWylie"))
})

test_that("mergeChunksWithIDs", {
  discoName = "rez007"
  path = "inst/extdata/" %+% discoName %+% ".Rdata"
　a = mergeChunksWithIDs(rez007, "largerChunk", selectCond = NULL)
})

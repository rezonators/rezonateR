test_that("importRez works", {
  discoName = "three-parting-2569_new"
  path = "C:/Users/User/Documents/GitHub/lhasa-reference-tracking/shanti/3_2_rez_file/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))))
  expect_s3_class(rezEx, "rezrObj")
  expect_s3_class(rezEx[["tokenDF"]], "rezrDF")
  expect_s3_class(rezEx[["tokenDF"]], "data.frame")
  expect_type(rezEx[["nodeMap"]], "list")
})


test_that("importRez works on 0.20.0", {
  discoName = "parting_0.20"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "trailLayer", regex = c("clausearg", "discdeix"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))))
  expect_s3_class(rezEx, "rezrObj")
  expect_s3_class(rezEx[["tokenDF"]], "rezrDF")
  expect_s3_class(rezEx[["tokenDF"]], "data.frame")
  expect_type(rezEx[["nodeMap"]], "list")
})

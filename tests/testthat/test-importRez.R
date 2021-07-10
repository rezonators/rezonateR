test_that("importRez works", {
  discoName = "three-luckydreamalt-8296"
  path = "C:/Users/User/Documents/GitHub/lhasa-reference-tracking/shanti/3_2_rez_file/" %+% discoName %+% ".rez"
  rezEx = importRez(path)
  expect_s3_class(rezEx[["wordDF"]], "data.frame")
  expect_type(rezEx[["nodeMap"]], "list")
})

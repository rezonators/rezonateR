test_that("getAllTreeCorrespondences", {
  discoName = "parting_0.20.1"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  layerRegex = list(track = list(field = "trailLayer", regex = c("clausearg", "discdeix"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr")))
  rezEx = importRez(path, layerRegex = layerRegex, concatFields = c("word", "wordWylie"))

  a = getAllTreeCorrespondences(rezEx, entity = "track")
  a = mergeChunksWithTree(a)
  expect("treeEntry" %in% names(a$trackDF[[1]]), "Tree correspondence failed.")
})

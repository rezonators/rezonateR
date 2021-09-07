test_that("getAllTreeCorrespondences", {
  a = rez_load("inst/extdata/rezEx.Rdata")

  a = getAllTreeCorrespondences(a, entity = "token")
  a = getAllTreeCorrespondences(a, entity = "track")
  expect("treeEntry" %in% names(a$trackDF[[1]]), "Tree correspondence failed.")
  a = mergeChunksWithTree(a)
  a = mergedChunksToTrack(a, "refexpr")
})

test_that("addPositionAmongSiblings", {
  a = rez_load("data/rezEx.rda")

  a$chunkDF$refexpr = addPositionAmongSiblings(a$chunkDF$refexpr, a)
  updateFunct(a$chunkDF$refexpr, "siblingPos")
  a$chunkDF$refexpr = a$chunkDF$refexpr %>% mutate(siblingPos = 0) %>% reloadForeign(a)
  expect(any(a$chunkDF$refexpr$siblingPos != 0), "Sibling reload failed.")
})

test_that("container", {
  a = rez_load("data/rezEx.rda")

  a$chunkDF$verb = addContainingChunk(a$chunkDF$verb, a, "chunkDF/clause")
  updateFunct(a$chunkDF$verb, "container")
  a$chunkDF$verb = a$chunkDF$verb %>% mutate(container = "") %>% reloadForeign(a)
  expect(any(a$chunkDF$verb$container != ""), "Container reload failed.")
})


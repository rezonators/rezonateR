test_that("Combination functions", {
  load("data/rezEx.rda")
  a = combineTokenChunk(rezEx)
  expect(all(c("tokenOrderFirst", "docTokenSeqFirst", "tokenOrderLast", "docTokenSeqLast") %in% names(a)), "combineTokenChunk didn't get all the seqs.")
})

test_that("CSV import/export", {
  load("data/rezEx.rda")

  rez_write_csv(rezEx$tokenDF, "inst/extdata/token.csv")
  #TODO: Update later
  #b = rez_read_csv("inst/extdata/chain.csv", inclCols = c("id", "tokenOrder"))
  #expect_type(b$id, "character")
  #expect_type(b$tokenOrder, "numeric")
  #b = rez_read_csv("inst/extdata/chain.csv", a$trackDF, exclCols = c("unit", "tokenOrder"))
  #expect(!("unit" %in% names(b)), "Exclusion failed")
})


test_that("getTrackTokens", {
  load("data/rezEx.rda")
  b = rezEx$trackDF$refexpr %>% rez_mutate(a = getTrackTokens(rezEx, "word", rezEx$trackDF$refexpr)) %>% select(word, a)
  expect(any(lapply(b$a, length) > 1), "getTrackTokens failed.")
})

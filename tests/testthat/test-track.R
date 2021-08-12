test_that("Track functions work", {
  discoName = "three-parting-2569_new"
  path = "C:/Users/User/Documents/GitHub/lhasa-reference-tracking/shanti/3_2_rez_file/" %+% discoName %+% ".rez"
  rezEx = importRez(path, layerRegex = list(track = list(field = "name", regex = c("CLAUSEARG_", "DISCDEIX_"), names = c("clausearg", "discdeix", "refexpr")), chunk = list(field = "chunkLayer", regex = c("verb", "adv", "predadj"), names = c("verb", "adv", "predadj", "refexpr"))))

  #addUnitSeq
  #chunk then track case
  a = addUnitSeq(rezEx, "chunk")
  expect("unitSeqLast" %in% (a$chunkDF$refexpr %>% names), "addUnitSeq fail.")
  a = addUnitSeq(a, "track")
  expect("unitSeqLast" %in% (a$chunkDF$refexpr %>% names), "addUnitSeq fail.")
  #track direct case
  a = addUnitSeq(rezEx, "track")
  expect("unitSeqLast" %in% (a$trackDF$refexpr %>% names), "addUnitSeq fail.")

  #lastMention
  b = a$trackDF$refexpr
  unitSeq = c(1, 1, 2, 3, 4, 4, 5)
  chain = c("a", "b", "a", "b", "a", "c", "a")
  expect(lastMention(unitSeq, chain) == c(4, 3, 2, NA, 1, NA, NA), "lastMention fail")
  #lastAppearance
  chain = b$chain
  unitSeq = b$unitSeqLast
  lastMention(unitSeq, chain)

  b = b %>% rez_mutate(lastMention = lastMention(unitSeq, chain))
  prev_result = b$lastMention
  b = b %>% rez_mutate(lastMention = lastMention())
  expect(all(prev_result == b$lastMention, na.rm = T), "Failed finding colnames in DF environment")




})


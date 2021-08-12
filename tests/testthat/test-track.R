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
  unitSeq = c(5, 4, 4, 3, 2, 1, 1)
  chain = c("a", "b", "a", "b", "a", "c", "a")
  expect(all(lastMention(unitSeq, chain) == c(4, 3, 2, NA, 1, NA, NA)), "lastMention fail")
  expect(all(lastMention(unitSeq, chain) == c(1, 1, 2, NA, 1, NA, NA)), "lastMention fail")

  b = a$trackDF$refexpr
  b = b %>% rez_mutate(lastMention = lastMention(unitSeq, chain), distToLastMention = distToLastMention(unitSeq, chain))
  prev_result = b$lastMention
  prev_result_2 = b$distToLastMention
  b = b %>% rez_mutate(lastMention = lastMention(), distToLastMention = distToLastMention())
  expect(all(prev_result == b$lastMention, na.rm = T), "Failed finding colnames in DF environment")
  expect(all(prev_result_2 == b$distToLastMention, na.rm = T), "Failed finding colnames in DF environment")






})


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

  #lastMentionUnit
  unitSeq = c(5, 4, 4, 3, 2, 1, 1)
  chain = c("a", "b", "a", "b", "a", "c", "a")
  expect(all(lastMentionUnit(unitSeq, chain) == c(4, 3, 2, NA, 1, NA, NA)), "lastMentionUnit fail")
  expect(all(lastMentionUnit(unitSeq, chain) == c(1, 1, 2, NA, 1, NA, NA)), "lastMentionUnit fail")

  b = a$trackDF$refexpr
  b = b %>% rez_mutate(lastMentionUnit = lastMentionUnit(unitSeqLast, chain), unitsToLastMention = unitsToLastMention(unitSeqLast, chain))
  prev_result = b$lastMentionUnit
  prev_result_2 = b$unitsToLastMention
  b = b %>% rez_mutate(lastMentionUnit = lastMentionUnit(), unitsToLastMention = unitsToLastMention())
  expect(all(prev_result == b$lastMentionUnit, na.rm = T), "Failed finding colnames in DF environment")
  expect(all(prev_result_2 == b$unitsToLastMention, na.rm = T), "Failed finding colnames in DF environment")
  #View(viewKeyPlus(b, c("unitSeqLast", "wordWylie", "lastMentionUnit", "unitsToLastMention")))

  b = a$trackDF$refexpr
  b = b %>% rez_mutate(lastMentionToken = lastMentionToken(tokenSeqLast, chain), tokensToLastMention = tokensToLastMention(tokenSeqLast, chain))
  prev_result = b$lastMentionToken
  prev_result_2 = b$tokensToLastMention
  b = b %>% rez_mutate(lastMentionToken = lastMentionToken(), tokensToLastMention = tokensToLastMention())
  expect(all(prev_result == b$lastMentionToken, na.rm = T), "Failed finding colnames in DF environment")
  expect(all(prev_result_2 == b$tokensToLastMention, na.rm = T), "Failed finding colnames in DF environment")
  #View(viewKeyPlus(b, c("tokenSeqLast", "wordWylie", "lastMentionToken", "tokensToLastMention")))

  b = rezEx
  b = addIsWordField(b, !str_detect(wordWylie, "/"))
  expect(is.numeric(b$trackDF$refexpr$discourseWordSeqLast), "Adding isWord field failed.")
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(wordsToLastMention = tokensToLastMention(discourseWordSeqLast))

  b = addUnitSeq(b, "track")
  b$trackDF$refexpr =  b$trackDF$refexpr %>% rez_mutate(wordsToLastMention = tokensToLastMention(discourseWordSeqLast, zeroProtocol = "unitLast", zeroCond = (word == "<0>"), unitDF = b$unitDF))

  b$trackDF$refexpr = b$trackDF$refexpr %>% arrange(chain, discourseTokenSeqLast)

  #noPrevMention
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(noPrevMentionsIn3 = noPrevMentions(3))
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(noPrevZerosIn5 = noPrevMentionsIf(5, word == "<0>"))
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(prevMentionWylie = getPrevMentionField(wordWylie))

  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(nextMentionToken = prevMentionToken(discourseTokenSeqFirst, chain), tokensToNextMention = tokensToLastMention(discourseTokenSeqLast, chain))
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(noNextZeros = noNextMentionsIf(Inf, word == "<0>"))
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(sizeDiff = nchar(getNextMentionField(word)) - nchar(word))
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(noCompetitors = noCompetitors())

})


test_that("Track functions work", {
  load("data/rezEx.rda")

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
  expect(all(lastMentionUnit(unitSeq, chain) == c(4, 3, 2, NA, 1, NA, NA), na.rm = T), "lastMentionUnit fail")

  b = a$trackDF$refexpr
  b = b %>% rez_mutate(lastMentionUnit = lastMentionUnit(unitSeqLast, chain), unitsToLastMention = unitsToLastMention(unitSeqLast, chain))
  prev_result = b$lastMentionUnit
  prev_result_2 = b$unitsToLastMention
  b = b %>% rez_mutate(lastMentionUnit = lastMentionUnit(), unitsToLastMention = unitsToLastMention())
  expect(all(prev_result == b$lastMentionUnit, na.rm = T), "Failed finding colnames in DF environment")
  expect(all(prev_result_2 == b$unitsToLastMention, na.rm = T), "Failed finding colnames in DF environment")
  #View(viewKeyPlus(b, c("unitSeqLast", "wordWylie", "lastMentionUnit", "unitsToLastMention")))

  b = a$trackDF$refexpr
  b = b %>% rez_mutate(lastMentionToken = lastMentionToken(docTokenSeqLast, chain), tokensToLastMention = tokensToLastMention(docTokenSeqLast, chain))
  prev_result = b$lastMentionToken
  prev_result_2 = b$tokensToLastMention
  b = b %>% rez_mutate(lastMentionToken = lastMentionToken(), tokensToLastMention = tokensToLastMention())
  expect(all(prev_result == b$lastMentionToken, na.rm = T), "Failed finding colnames in DF environment")
  expect(all(prev_result_2 == b$tokensToLastMention, na.rm = T), "Failed finding colnames in DF environment")
  #View(viewKeyPlus(b, c("docTokenSeqLast", "wordWylie", "lastMentionToken", "tokensToLastMention")))

  b = rezEx
  b = addIsWordField(b, !str_detect(wordWylie, "/"))
  expect(is.numeric(b$trackDF$refexpr$docWordSeqLast), "Adding isWord field failed.")
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(wordsToLastMention = tokensToLastMention(docWordSeqLast))

  b = addUnitSeq(b, "track")
  b$trackDF$refexpr =  b$trackDF$refexpr %>% rez_mutate(wordsToLastMention = tokensToLastMention(docWordSeqLast, zeroProtocol = "unitFinal", zeroCond = (word == "<0>"), unitDF = b$unitDF))

  b$trackDF$refexpr = b$trackDF$refexpr %>% arrange(chain, docTokenSeqLast)

  #countPrevMention
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(countPrevMentionsIn3 = countPrevMentions(3))
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(countPrevZerosIn5 = countPrevMentionsIf(5, word == "<0>"))
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(prevMentionWylie = getPrevMentionField(wordWylie))

  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(prevMentionToken = lastMentionToken(docTokenSeqFirst, chain), tokensToNextMention = tokensToLastMention(docTokenSeqLast, chain))
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(countNextZeros = countNextMentionsIf(Inf, word == "<0>"))
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(sizeDiff = nchar(getNextMentionField(word)) - nchar(word))
  b$trackDF$refexpr = b$trackDF$refexpr %>% rez_mutate(countCompetitors = countCompetitors())

})



test_that("Bridge functions work", {
  load("data/rezEx.rda")

  a = rezEx
  a = addUnitSeq(rezEx, "track")
  a = undupeLayers(a, "trail", "name")
  a = addFrameMatrix(a)
  expect("frameMatrix" %in% class(frameMatrix(a)), "addFrameMatrix failed.")
  a$trackDF$discdeix = reloadForeign(a$trackDF$discdeix, a)

  #lastBridgeUnit
  b = a$trackDF$refexpr
  b = b %>% rez_mutate(lastBridgeUnit = lastBridgeUnit(frameMatrix(a)),
                       lastBridgeToken = lastBridgeToken(frameMatrix(a)))
  expect(any(!is.na(b$lastBridgeUnit)), "lastBridgeUnit failed.")
  expect(all(b$lastBridgeUnit <= b$lastBridgeToken, na.rm = T), "lastBridgeToken failed.")

  b = b %>% rez_mutate(unitsToLastBridge = unitsToLastBridge(frameMatrix(a)),
                       tokensToLastBridge = tokensToLastBridge(frameMatrix(a)))
  expect(any(!is.na(b$unitsToLastBridge)), "unitsToLastBridge failed.")
  expect(any(!is.na(b$tokensToLastBridge)), "tokensToLastBridge failed.")
  b = b %>% rez_mutate(tokensToLastBridgeUF = tokensToLastBridge(frameMatrix(a), zeroProtocol = "unitFinal", zeroCond = (word == "<0>"), unitDF = a$unitDF))
  expect(any(!is.na(b$tokensToLastBridgeUF)), "tokensToLastBridgeUF (unit-final) failed.")
  expect(all(b$unitsToLastBridge <= b$tokensToLastBridge, na.rm = T), "tokensToLastBridgeUF failed.")
})

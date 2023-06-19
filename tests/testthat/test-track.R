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

test_that("Member chunks ignored", {
  discoName = "rez007"
  path = "inst/extdata/" %+% discoName %+% ".Rdata"
  a = mergeChunksWithIDs(rez007, "largerChunk", selectCond = NULL)
  a = getAllTreeCorrespondences(a, entity = "track")
  a = mergedChunksToTrack(a)
  a = addUnitSeq(a, "track")
  b = a$trackDF$default %>% rez_mutate(lastMention = lastMentionUnit(exclFrag = T))
  b = b %>% rez_mutate(unitsToLastMention = unitsToLastMention(exclFrag = T))
  b = b %>% rez_mutate(nextMention = nextMentionUnit(exclFrag = T))
  b = b %>% rez_mutate(unitsToNextMention = unitsToNextMention(exclFrag = T))

  b = b %>% rez_mutate(lastMentionT = lastMentionToken(exclFrag = T))
  b = b %>% rez_mutate(tokensToLastMention = tokensToLastMention(exclFrag = T, zeroProtocol = "unitInitial", unitDF = a$unitDF, zeroCond = (text == "<0>")))
  b = b %>% rez_mutate(nextMentionT = nextMentionToken(exclFrag = T))
  b = b %>% rez_mutate(tokensToNextMention = tokensToNextMention(exclFrag = T))
  b = b %>% rez_mutate(tokensToNextMention = tokensToNextMention(exclFrag = T, zeroProtocol = "unitInitial", unitDF = a$unitDF, zeroCond = (text == "<0>")))

  b = b %>% rez_mutate(prevMentions = countPrevMentions(10, exclFrag = T))
  b = b %>% rez_mutate(prevMentions = countPrevMentionsIf(10, exclFrag = T, text != "<0>"))
  b = b %>% rez_mutate(prevMentions = getNextMentionField(10, exclFrag = T))
  b = b %>% rez_mutate(competitorsWrong = countCompetitors(window = 10))
  b = b %>% rez_mutate(competitors = countCompetitors(window = 10, exclFrag = T))
  b = b %>% rez_mutate(competitors2 = countMatchingCompetitors(matchCol = layer, window = 10, exclFrag = T))
})

test_that("SEMDIAL stuff works", {
  discoName = "sbc007_animacy"
  path = "inst/extdata/" %+% discoName %+% ".rez"
  layerRegex = list(chunk = list(field = "chunkType",
                                 regex = c("verb"),
                                 names = c("verb", "refexpr")))
  concatFields = c("text", "transcript")
  sbc007_anim = importRez(path,
                     layerRegex = layerRegex,
                     concatFields = concatFields)
  sbc007_anim = addUnitSeq(sbc007_anim, entity = "track")

  sbc007_anim$trackDF$default %>% filter(gapUnits <= 1, charCount > 20) %>% select(id, gapUnits, charCount, text)
  sbc007_anim = getAllTreeCorrespondences(sbc007_anim, entity = "track")
  sbc007_anim = mergeChunksWithIDs(sbc007_anim, "largerChunk")
  sbc007_anim = mergeChunksWithTree(sbc007_anim)
  sbc007_anim = mergedChunksToTrack(sbc007_anim, "default")

  sbc007_anim$trackDF$default = sbc007_anim$trackDF$default %>%
    rez_mutate(noPrevMentionsIn10 = countPrevMentions(10,
                                                      exclFrag = T))

  sbc007_anim = sbc007_anim %>%
    addFieldForeign("track", "default", "treeEntry", "default", "treeEntry", "Relation", "Relation", fieldaccess = "foreign")

  sbc007_anim$trackDF$default = sbc007_anim$trackDF$default %>% mutate(formType = case_when(
    text == "<0>" ~ "zero",
    str_detect(tolower(text), "I|(you)|we|they|he|she|it|me|us|them|him") ~ "pronominal",
    tolower(text) %in% c("her", "this", "that") ~ "pronominal",
    T ~ "lexical"
  ))
  rez_write_csv(sbc007_anim$trackDF$default, "rez007_refform.csv", c("id", "text", "formType"))

  changeDF = rez_read_csv("inst/extdata/rez007_refform_edited.csv", origDF = sbc007_anim$trackDF$default)
  sbc007_anim$trackDF$default = sbc007_anim$trackDF$default %>% updateFromDF(changeDF, changeCols = "formType")

  sbc007_anim$trackDF$default = sbc007_anim$trackDF$default %>%
    rez_mutate(Relation = coalesce(Relation, "NonSubj"), fieldaccess = "flex") %>%
    rez_mutate(Relation = case_when(Relation == "Subj" ~ "subject", Relation == "NonSubj" ~ "non-subject")) %>%
    rez_mutate(Relation = factor(Relation, levels = c("subject", "non-subject")))
  sbc007_anim$trackDF$default = sbc007_anim$trackDF$default %>% rez_mutate(noPrevMentionsBinned = case_when(noPrevMentionsIn10 == 0 ~ "0",noPrevMentionsIn10 == 1 ~ "1", noPrevMentionsIn10 == 2 ~ "2", T ~ "3+"))
  ggplot(sbc007_anim$trackDF$default, aes(fill = formType, x = noPrevMentionsBinned)) +
    geom_bar(position="fill") + facet_grid(rows = vars(Relation), cols = vars(animacy)) + ylab("proportion") + theme(legend.position = "none") + xlab("number of previous mentions")


  sbc007_anim = sbc007_anim %>% addFieldForeign("track", "default", "unit", "", "unitSeqFirst", "participant", "participant", sourceKeyName = "unitSeq")
  sbc007_anim$trackDF$default %>%
    filter(name %in% c("Tim", "Ron", "Tim and Mandy", "The two couples")) %>%
    rez_mutate(name_new = case_when(
      name == "Tim and Mandy" ~ "T+M",
      name == "Tim" ~ "T",
      name == "The two couples" ~ "T+M+A+R",
      name == "Ron" ~ "R"
    ) %>% factor(levels = c("T", "R", "T+M", "T+M+A+R"))) %>%
    rez_mutate(formTypeShort = case_when(formType == "zero" ~ "Z", formType == "pronominal" ~ "P", T ~ "L"),
      formWeight = case_when(formType == "zero" ~ 1, formType == "pronominal" ~ 2, T ~ 3)) %>%
    ggplot(aes(x = docTokenSeqFirst, y = name_new)) + geom_point(aes(col = participant, size = formWeight), shape = "O") + theme_bw() + theme(axis.title = element_blank(), legend.position = "none") + scale_y_discrete(limits=rev)+
    scale_colour_manual(name="Speaker",
    labels = c("ALICE;", "MARY;"),
    values = c("#026440", "purple"))

  rez_save(sbc007_anim, "sbc007_semdial_track.Rdata")

  sbc007_turn =  sbc007_turn %>% stackToToken
  sbc007_turn$tokenDF = sbc007_turn$tokenDF %>% rez_mutate(TurnOrder = getOrderFromSeq(TurnSeq, kind == "word"))
  sbc007_turn$tokenDF = sbc007_turn$tokenDF %>% rez_group_by(TurnSeq) %>% rez_mutate(TurnLength = sum(kind == "word")) %>% rez_ungroup()
  sbc007_turn$tokenDF = sbc007_turn$tokenDF %>% rez_mutate(TurnBack = as.integer(TurnLength) - as.integer(TurnOrder))


  library(ggbreak)
 # bargraph_place =
    sbc007_turn$tokenDF %>% filter(tolower(text) %in% c("oh", "like")) %>% ggplot(aes(x = as.integer(TurnOrder), fill = tolower(text))) + geom_bar(position = "dodge") + theme(legend.position = "none") + xlab("Position from turn start") + ylab("Frequency") + scale_x_continuous(breaks =c(0,20,40,60, 90,120,280)) +scale_x_break(c(60, 85))+scale_x_break(c(95, 115))  + scale_x_break(c(125, 280)) + theme(axis.text.x.top = element_blank(), axis.ticks.x.top = element_blank())
  bargraph_back = sbc007_turn$tokenDF %>% filter(tolower(text) %in% c("oh", "like")) %>% ggplot(aes(x = as.integer(TurnBack), fill = tolower(text))) + geom_bar(position = "dodge") + theme(legend.position = "none") + xlab("Position from turn end") + theme(axis.title.y = element_blank())
  bargraph_place
  library(patchwork)
  bargraph_place + bargraph_back

})

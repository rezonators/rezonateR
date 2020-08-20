library(dplyr)
library(readr)


#Just for organiseWordTable, not intended to be used outside
findPlaceSingle = function(wordTableSinglesRow, wordTableSinglesByIU){
  unitID = wordTableSinglesRow$unitID
  wordSeq = wordTableSinglesRow$wordSeq
  which(wordTableSinglesByIU[[unitID]]$wordSeq == wordSeq)
}

#Just for organiseWordTable, not intended to be used outside
findPlaceChunk = function(wordTableChunksRow, wordTableSinglesByIU){
  startWordSeq = wordTableChunksRow$wordSeq - wordTableChunksRow$Length
  endWordSeq = wordTableChunksRow$wordSeq - 1
  currentSinglesFrame = wordTableSinglesByIU[[wordTableChunksRow$unitID]]
  startLocation = currentSinglesFrame %>% filter(wordSeq == startWordSeq) %>% pull(Location)
  endLocation = currentSinglesFrame %>% filter(wordSeq == endWordSeq) %>% pull(Location)
  startPlace = currentSinglesFrame %>% filter(wordSeq == startWordSeq) %>% pull(Place)
  endPlace = currentSinglesFrame %>% filter(wordSeq == endWordSeq) %>% pull(Place)
  
  c(StartWordSeq = startWordSeq, EndWordSeq = endWordSeq, StartLocation = startLocation, EndLocation = endLocation, StartPlace = startPlace, EndPlace = endPlace)
}

getVoid = function(currUID, wordTable){
  currUnitWordTable = wordTable %>% filter(uID == currUID)
  if(nrow(currUnitWordTable) > 0){
    filledCols = unique(sort(currUnitWordTable$displayCol))
    currUnitWordTable = currUnitWordTable %>% select(wordID) %>% mutate(VoidR = sapply(currUnitWordTable$displayCol, function(x) filledCols[filledCols > x][1] - (x + 1)),
                                                    VoidL = sapply(currUnitWordTable$displayCol, function(x) (x - 1) - c(-1, filledCols[filledCols < x])[sum(filledCols < x) + 1])) %>% mutate(VoidR = case_when(is.na(VoidR) ~ 0, T ~ VoidR)) %>% mutate(VoidL = sapply(VoidL, function(x) if(length(x) == 0) 0 else x))
  }
  currUnitWordTable
}

organiseWordTable = function(wordTable, dynaWordTable){
  wordTable = wordTable %>% mutate(SepStrings = strsplit(wordToken, " ")) %>%
    mutate(Length = sapply(SepStrings, length))
  wordTableSingles = wordTable %>% filter(Length <= 1)
  wordTableSinglesByIU = lapply(1:max(wordTable$unitID), function(x) wordTableSingles %>% filter(unitID == x) %>% arrange(wordSeq))
  wordTableSingles = wordTableSingles %>% mutate(Place = apply(as.matrix(wordTableSingles), 1, function(x) findPlaceSingle(x, wordTableSinglesByIU))) %>% mutate(Location = 1:nrow(wordTableSingles)) %>%
    mutate(StartLocation = Location, EndLocation = Location, StartWordSeq = wordSeq, EndWordSeq = wordSeq, StartPlace = Place, EndPlace = Place)
  
  wordTableSinglesByIU = lapply(1:max(wordTable$unitID), function(x) wordTableSingles %>% filter(unitID == x) %>% arrange(wordSeq))
  wordTableChunks = wordTable %>% filter(Length > 1)
  
  if(nrow(wordTableChunks) > 0){
    wordTableChunks = wordTableChunks %>% cbind(apply(as.matrix(wordTableChunks), 1, function(x) findPlaceChunk(x, wordTableSinglesByIU)) %>% t) %>% mutate(Location = StartLocation, Place = StartPlace)
    wordTable = rbind(wordTableSingles, wordTableChunks)
  } else{
    wordTable = wordTableSingles
  }
  
  unitLengths = wordTableSingles %>% group_by(unitID) %>% summarise(Length = max(Place))
  wordTable = wordTable %>% mutate(UnitLength = sapply(unitID, function(x) unitLengths %>% filter(unitID == x) %>% pull(Length))) %>%
                           mutate(PlaceBack = UnitLength - Place + 1, StartPlaceBack = UnitLength - StartPlace + 1, EndPlaceBack = UnitLength - EndPlace + 1) %>% select(-UnitLength)
  
  wordTable = wordTable %>% left_join(dynaWordTable, by = c("wordID", "wID", "uID"))
  void = Reduce(rbind, lapply(unique(wordTable$unitID), getVoid, wordTable))
  wordTable =  wordTable %>% left_join(void, by = "wordID")
}

#Get X from word ID
getLocationFromWordID = function(wordIDs, wordTable){
  sapply(wordIDs, function(currID){
    result = wordTable %>% filter(wordID == currID) %>% pull(Location)
    if(length(result) == 1) return(result) else return(NA)
  })
}

getUnitIDFromWordID = function(wordIDs, wordTable){
  sapply(wordIDs, function(currID){
    result = wordTable %>% filter(wordID == currID) %>% pull(unitID)
    if(length(result) == 1) return(result) else return(NA)
  })
}

getPlaceFromWordID = function(wordIDs, wordTable){
  sapply(wordIDs, function(currID){
    result = wordTable %>% filter(wordID == currID) %>% pull(Place)
    if(length(result) == 1) return(result) else return(NA)
  })
}

getPlaceBackFromWordID = function(wordIDs, wordTable) getPropFromWordID(wordIDs, wordTable, "PlaceBack")


getPropFromWordID = function(wordIDs, wordTable, property){
  sapply(wordIDs, function(currID){
    result = wordTable %>% filter(wordID == currID) %>% pull(property)
    if(length(result) == 1) return(result) else return(NA)
  })
}

detectOverlap = function(sourceStarts, goalStarts, sourceEnds, goalEnds){
  sapply(1:length(sourceStarts), function(x){
    if((!is.na(goalStarts[x]) && (!is.na(goalEnds[x])))){
      intersect(sourceStarts[x]:sourceEnds[x], goalStarts[x]:goalEnds[x]) %>% length != 0
    } else {
      F
    }
  })
}

organiseUnitTable = function(unitTable, wordTable){
  wordTableSingles = wordTable %>% filter(Length == 1)
  #One way to speed this up: use the max and min of wordTableSingles. Disadvantage: we can't later decide that word IDs between the max and min can be chunks.
  unitTable = unitTable %>% mutate(wordListVector = strsplit(substring(wordIDList, 2, nchar(wordIDList)-1), ", ")) %>%
    mutate(WordCount = sapply(wordListVector, function(x) x %in% wordTableSingles$wordID %>% as.integer %>% sum),
           TokenCount = sapply(wordListVector, function(x) x %in% wordTableSingles$wordID %>% as.integer %>% sum)) %>% #Update later
    mutate(VoidMax = sapply(wordListVector, function(x) wordTableSingles %>% filter(wordID %in% x) %>% pull(VoidL) %>% max),
           VoidSum = sapply(wordListVector, function(x) wordTableSingles %>% filter(wordID %in% x) %>% pull(VoidL) %>% sum)) %>%
    mutate(VoidMax = case_when(VoidMax == -Inf ~ 0, T ~ VoidMax)) %>%
    mutate(Text = sapply(wordListVector, function(x) wordTableSingles %>% filter(wordID %in% x) %>% pull(wordTranscript) %>% paste(collapse = " ")))
  unitTable
}


organiseLinkTable = function(linkTable, wordTable){
  linkTable = linkTable %>% mutate(Range = abs(getUnitIDFromWordID(goal, wordTable) - getUnitIDFromWordID(source, wordTable)), Length = 2) %>% rename(TiltAlign = tilt) %>%
    mutate(SourcePlace = getPlaceFromWordID(source, wordTable), GoalPlace = getPlaceFromWordID(goal, wordTable))%>%
    mutate(SourceStartLocation = getPropFromWordID(source, wordTable, "StartLocation"), GoalStartLocation = getPropFromWordID(goal, wordTable, "StartLocation"))%>%
    mutate(SourceEndLocation = getPropFromWordID(source, wordTable, "EndLocation"), GoalEndLocation = getPropFromWordID(goal, wordTable, "EndLocation")) %>%
     mutate(TiltRaw = GoalPlace - SourcePlace) %>% mutate(TiltDiff = TiltRaw - TiltAlign) %>% mutate(Gap = sapply(Range, function(x) max(x - 1, 0)), Width = TiltRaw) %>%
    mutate(Flip = (goal < source & goal != -1), Lap = detectOverlap(SourceStartLocation, GoalStartLocation, SourceEndLocation, GoalEndLocation), Side = case_when(is.na(Range) ~ F, T ~ Range == 0)) %>%
    mutate(SourcePlaceBack = getPlaceBackFromWordID(source, wordTable), GoalPlaceBack = getPlaceBackFromWordID(goal, wordTable)) %>%
    mutate(TiltJustifyR = GoalPlaceBack - SourcePlaceBack) %>%
    mutate(TiltWrap = case_when(SourcePlace > GoalPlace ~ SourcePlace - GoalPlaceBack,
                                SourcePlace < GoalPlace ~ GoalPlace - SourcePlaceBack,
                                T ~ 0))
  linkTable
}

max0 = function(values, na.rm = T) suppressWarnings(max(max(values %>% unlist, na.rm = na.rm), 0))

findRuns = function(vector){
  runs = integer(0)
  values = integer(0)
  
  if(length(vector) > 1){
    values = vector[1]
    runs = 1
    for(x in 2:length(vector)){
      if(vector[x-1] == vector[x]){
        runs[length(runs)] = runs[length(runs)] + 1
      } else{
        runs = c(runs, 1)
        values = c(values, vector[x])
      }
    }
  } else if(length(vector) == 1){
    values = vector
    runs = 1
  }
  
  list(runs = runs, values = values)
}

#Can accept track and rez chain tables
organiseChainTable = function(chainTable, wordTable, linkTable){
  chainTable = chainTable %>% mutate(wordListVector = strsplit(substring(wordList, 2, nchar(wordList)-1), ", ")) %>%
    mutate(wordLocations = lapply(wordListVector, function(x) sapply(x, function(y) wordTable %>% filter(wordID == y) %>% pull(Location)))) %>%
    mutate(topUnitLocation = sapply(wordLocations, min), bottomUnitLocation = sapply(wordLocations, max)) %>%
    mutate(unitListVector = sapply(wordLocations, function(x) getUnitIDFromWordID(x, wordTable))) %>%
    mutate(topUnitID = sapply(unitListVector, min),
           bottomUnitID = sapply(unitListVector, max)) %>%
    mutate(Range = as.integer(bottomUnitID) - as.integer(topUnitID), Length = sapply(wordListVector, length)) %>%
    mutate(wordPlacesVector = sapply(wordListVector, function(x) getPlaceFromWordID(x, wordTable))) %>%
    mutate(PlaceAvg = sapply(wordPlacesVector, mean), PlaceStDev = sapply(wordPlacesVector, function(x) sqrt(mean((x - mean(x))^2)))) %>%
    mutate(wordPlaceBacksVector = sapply(wordListVector, function(x) getPlaceBackFromWordID(x, wordTable))) %>%
    mutate(PlaceBackAvg = sapply(wordPlaceBacksVector, mean), PlaceBackStDev = sapply(wordPlaceBacksVector, function(x) sqrt(mean((x - mean(x))^2)))) %>%
    mutate(chainID = as.integer(chainID)) #in case when imported the chain wasn't classified as int
  linkInfo = linkTable %>% group_by(chainID) %>% summarise(TiltMaxAlign = max0(abs(TiltAlign), na.rm = T),
                                                           TiltSumAlign = sum(TiltAlign, na.rm = T),
                                                           TiltMaxRaw = max0(abs(TiltRaw), na.rm = T),
                                                           TiltSumRaw = sum(TiltRaw, na.rm = T),
                                                           TiltMaxJustifyR = max0(abs(TiltJustifyR), na.rm = T),
                                                           TiltSumJustifyR = sum(TiltJustifyR, na.rm = T),
                                                           FlipCount = sum(Flip),
                                                           LapCount = sum(Lap),
                                                           SideLinkCount = sum(Side),
                                                           GapCount = sum(Gap != 0, na.rm = T),
                                                           GapMax = max0(Gap, na.rm = T),
                                                           GapSum = sum(Gap, na.rm = T) 
                                                           ) %>% ungroup
  sideChainInfo = linkTable  %>% group_by(chainID) %>% summarise(SideChainMax = findRuns(Side)$runs[findRuns(Side)$values] %>% max0,
                                                                 SideChainCount = findRuns(Side)$runs[findRuns(Side)$values] %>% length)
  chainTable = chainTable %>% left_join(linkInfo, by = "chainID") %>% left_join(sideChainInfo, by = "chainID")
  #  wordChainTable = lapply(chainTable$chainID, getWordTableForChain, chainTable, wordTable) %>% bind_rows()
  chainTable
}

#A function, not currently used, for getting a word table with chain info
getWordTableForChain = function(id, chainTable, wordTable){
  row = chainTable %>% filter(chainID == id)
  wordlist_str = row$wordList
  wordlist = as.integer(strsplit(wordlist_str, "[{}}(,)]")[[1]][-1])
  wordSubset = wordTable %>% filter(wordID %in% wordlist) %>% arrange(unitID)
  wordSubset = wordSubset %>%
    mutate(distFromPrev = c(NA, wordSubset$unitID[-1] - wordSubset$unitID[-nrow(wordSubset)]))
  cbind(row, wordSubset)
}

#Get X from chain IDs
getPropFromChainID = function(chainIDs, chainTable, property){
  sapply(chainIDs, function(currID){
    result = chainTable %>% filter(chainID == currID) %>% pull(property)
    if(length(result) == 1) return(result) else return(NA)
  })
}

getPropFromUnitID = function(unitIDs, unitTable, property){
  sapply(unitIDs, function(currID){
    result = unitTable %>% filter(unitID == currID) %>% pull(property)
    if(length(result) == 1) return(result) else return(NA)
  })
}

findGaps = function(lines){
  lines = as.integer(lines)
  if(length(lines) > 1){
    lapply(1:(length(lines)-1), function(x){
      if(lines[x+1] - lines[x] > 1) (lines[x]+1):(lines[x+1]-1) else integer(0)
    })
  } else integer(0)
}

findUnitListWordCount = function(unitList, unitTable){
  if(length(unitList) > 0){
    sapply(unitList, function(x) unitTable %>% filter(unitID %in% x) %>% pull(WordCount) %>% sum)
  } else {
    0
  }
}

#organising cliques
organiseCliqueTable = function(cliqueTable, wordTable, unitTable, linkTable, rezTable){
  cliqueTable = cliqueTable %>% mutate(chainListVector = strsplit(substring(chainIDList, 2, nchar(chainIDList)-1), ", ")) %>%
                                mutate(unitListVector = strsplit(substring(unitIDList, 2, nchar(unitIDList)-1), ", ")) %>%
                                mutate(unitListVector = sapply(unitListVector, as.integer)) %>%
                                mutate(topUnitID = sapply(unitListVector, min) %>% as.integer, bottomUnitID = sapply(unitListVector, max) %>% as.integer) %>%
                                mutate(Range = bottomUnitID - topUnitID, Length = sapply(unitIDList, function(x) length(unique(x)) )) %>%
                                mutate(TiltMaxAlign = sapply(chainListVector, function(x) max0(getPropFromChainID(x, rezTable, "TiltMaxAlign")) )) %>%
                                mutate(TiltSumAlign = sapply(chainListVector, function(x) sum(getPropFromChainID(x, rezTable, "TiltSumAlign")) )) %>%
                                mutate(TiltMaxRaw = sapply(chainListVector, function(x) max0(getPropFromChainID(x, rezTable, "TiltMaxRaw")) )) %>%
                                mutate(TiltSumRaw = sapply(chainListVector, function(x) sum(getPropFromChainID(x, rezTable, "TiltSumRaw")) )) %>%
                          mutate(TiltMaxJustifyR = sapply(chainListVector, function(x) max0(getPropFromChainID(x, rezTable, "TiltMaxJustifyR")))) %>%
                          mutate(TiltSumJustifyR = sapply(chainListVector, function(x) sum(getPropFromChainID(x, rezTable, "TiltSumJustifyR")))) %>%
                                mutate(RezChainCount = sapply(chainListVector, length)) %>%
                                mutate(RezChainMax = sapply(chainListVector, function(x) max0(getPropFromChainID(x, rezTable, "Length")) )) %>%
                                mutate(LapCount = sapply(chainListVector, function(x) sum(getPropFromChainID(x, rezTable, "SideChainCount")) )) %>% #
                                mutate(Width = sapply(unitListVector, function(x) max0(getPropFromUnitID(x, unitTable, "WordCount")) ))%>%
                                mutate(WordCount = sapply(unitListVector, function(x) sum(getPropFromUnitID(x, unitTable, "WordCount")) )) %>%
                                mutate(ChainWordCount = sapply(chainListVector, function(x) length(Reduce(union, getPropFromChainID(x, rezTable, "wordListVector"))))) %>%
                                mutate(VoidSum = sapply(unitListVector, function(x) getPropFromUnitID(x, unitTable, "VoidSum") %>% sum)) %>%
                                mutate(gapsList = lapply(unitListVector, findGaps)) %>%
                                mutate(GapLineMax = lapply(gapsList, function(x) sapply(x, length) %>% max) %>% unlist) %>%
                                mutate(GapLineSum = lapply(gapsList, function(x) sapply(x, length) %>% sum) %>% unlist) %>%
                                mutate(GapWordMax = sapply(gapsList, function(x) sapply(x, findUnitListWordCount, unitTable) %>% max)) %>%
                                mutate(GapWordSum = sapply(gapsList, function(x) sapply(x, findUnitListWordCount, unitTable) %>% sum))
    cliqueTable
}

#organising cliques
organiseStackTable = function(stackTable, unitTable){
  stackTable = stackTable %>% mutate(unitListVector = strsplit(substring(unitIDList, 2, nchar(unitIDList)-1), ", ")) %>%
    mutate(unitListVector = sapply(unitListVector, as.integer)) %>%
    mutate(topUnitID = sapply(unitListVector, min) %>% as.integer, bottomUnitID = sapply(unitListVector, max) %>% as.integer) %>%
    mutate(Range = bottomUnitID - topUnitID, Length = sapply(unitIDList, function(x) length(unique(x)) )) %>%
    mutate(LapCount = sapply(unitListVector, function(x) (summary(as.factor(x)) > 1) %>% sum )) %>% 
    mutate(Width = sapply(unitListVector, function(x) max0(getPropFromUnitID(x, unitTable, "WordCount")) ))%>%
    mutate(WordCount = sapply(unitListVector, function(x) sum(getPropFromUnitID(x, unitTable, "WordCount")) )) %>%
    mutate(gapsList = lapply(unitListVector, findGaps)) %>%
    mutate(GapLineMax = lapply(gapsList, function(x) sapply(x, length) %>% max0) %>% unlist) %>%
    mutate(GapLineSum = lapply(gapsList, function(x) sapply(x, length) %>% unlist %>% sum) %>% unlist) %>%
    mutate(GapWordMax = sapply(gapsList, function(x) sapply(x, findUnitListWordCount, unitTable) %>% max0)) %>%
    mutate(GapWordSum = sapply(gapsList, function(x) sapply(x, findUnitListWordCount, unitTable) %>% unlist %>% sum))
  stackTable
}



timesAppeared = function(vals, vec) sapply(vals, function(x) vec[vec == x] %>% length)
chainsAppeared = function(wordIDs, linkTable) sapply(wordIDs, function(x) linkTable %>% filter(goal == x | source == x) %>% pull(chainID) %>% unique)
cliquesAppeared = function(chainIDs, cliqueTable) sapply(chainIDs, function(x) cliqueTable %>% filter(sapply(chainListVector, function(y) intersect(as.integer(x), as.integer(y)) %>% length != 0) ) %>% pull(cliqueID) %>% unique)

vecToString = function(vec) paste0("{",paste0(vec, collapse = ", "),"}")

#wordTable = organiseWordTable(csvs$wordTable, csvs$dynaWordTable)
organiseWordTablePass2 = function(wordTable, linkTable, rezTable, trackTable, cliqueTable){
  wordTable = wordTable %>% mutate(RezChainCount = timesAppeared(wordID, Reduce(c, rezTable$wordListVector) %>% as.integer),
                    TrackChainCount = timesAppeared(wordID, Reduce(c, trackTable$wordListVector) %>% as.integer)) %>%
              mutate(InChainIDVec = chainsAppeared(wordID, linkTable)) %>%
              mutate(InCliqueIDVec = cliquesAppeared(InChainIDVec, cliqueTable)) %>%
              mutate(InChainID = sapply(InChainIDVec, vecToString), InCliqueID = sapply(InCliqueIDVec, vecToString))
  wordTable
}


organiseUnitTablePass2 = function(unitTable, wordTable, rezTable, trackTable){
  unitTable = unitTable %>% mutate(RezChainCount = timesAppeared(unitID, Reduce(c, rezTable$unitListVector) %>% as.integer),
                                   TrackChainCount = timesAppeared(unitID, Reduce(c, trackTable$unitListVector) %>% as.integer))
  bridgeInfo = wordTable %>% group_by(unitID) %>% filter(RezChainCount != 0) %>%
    summarise(BridgeWidth = n(), BridgeRange = max(EndPlace) - min(StartPlace) + 1L, WordsMultipleChain = sum(RezChainCount > 1))
  unitTable = unitTable %>% left_join(bridgeInfo) %>% mutate(BridgeWidth = case_when(is.na(BridgeWidth) ~ 0L, T ~ as.integer(BridgeWidth)),
                                                 BridgeRange = case_when(is.na(BridgeRange) ~ 0L, T ~ as.integer(BridgeRange)),
                                                 WordsMultipleChain = case_when(is.na(WordsMultipleChain) ~ 0L, T ~ as.integer(WordsMultipleChain)))
  unitTable
}

organiseCliqueTablePass2 = function(cliqueTable, wordTable, unitTable, rezTable, trackTable){
  cliqueTable = cliqueTable %>% mutate(ChainWidth = getPropFromUnitID(1:10, unitTable, "WordsMultipleChain") %>% max)
  cliqueTable
}


#The actual function to be called by the user
importCSVs = function(foldername){
  #Will do some preliminary processing for each one as I get to it.
  
  cliqueTable = read_csv(paste0(foldername,"/clique.csv"))
  dynaWordTable = read_csv(paste0(foldername,"/dynaWord.csv"))
  filterTable = read_csv(paste0(foldername,"/filter.csv"))
  hitTable = read_csv(paste0(foldername,"/hit.csv"))
  lineTable = read_csv(paste0(foldername,"/line.csv"))
  linkTable = read_csv(paste0(foldername,"/link.csv"))
  rezTable = read_csv(paste0(foldername,"/rez.csv"))
  searchTable = read_csv(paste0(foldername,"/search.csv"))
  stackTable = read_csv(paste0(foldername,"/stack.csv"))
  trackTable = read_csv(paste0(foldername,"/track.csv"))
  unitTable = read_csv(paste0(foldername,"/unit.csv"))
  vizCliqueTable = read_csv(paste0(foldername,"/vizClique.csv"))
  vizLinkTable = read_csv(paste0(foldername,"/vizLink.csv"))
  wordTable = read_csv(paste0(foldername,"/word.csv"))
  
  allTables = list(cliqueTable = cliqueTable, dynaWordTable = dynaWordTable, filterTable = filterTable, hitTable = hitTable, lineTable = lineTable, linkTable = linkTable, rezTable = rezTable, searchTable = searchTable, stackTable = stackTable, trackTable = trackTable, unitTable = unitTable, vizCliqueTable = vizCliqueTable, vizLinkTable = vizLinkTable, wordTable = wordTable)
  attr(allTables, "foldername") = foldername  
  class(allTables) = "RawRezonatorInfo"
  allTables
}

organiseCSVs = function(allTables){
  allTables$wordTable = organiseWordTable(allTables$wordTable, allTables$dynaWordTable)
  allTables$unitTable = organiseUnitTable(allTables$unitTable, allTables$wordTable)
  allTables$linkTable = organiseLinkTable(allTables$linkTable, allTables$wordTable)
  allTables$trackTable = organiseChainTable(allTables$trackTable, allTables$wordTable, allTables$linkTable)
  allTables$rezTable = organiseChainTable(allTables$rezTable, allTables$wordTable, allTables$linkTable)
  allTables$cliqueTable = organiseCliqueTable(allTables$cliqueTable, allTables$wordTable, allTables$unitTable, allTables$linkTable, allTables$rezTable)
  allTables$stackTable = organiseStackTable(allTables$stackTable, allTables$unitTable)
  
  allTables$wordTable = organiseWordTablePass2(allTables$wordTable, allTables$linkTable, allTables$rezTable, allTables$trackTable, allTables$cliqueTable)
  allTables$unitTable = organiseUnitTablePass2(allTables$unitTable, allTables$wordTable, allTables$rezTable, allTables$trackTable)
  
  class(allTables) = "RezonatoRInfo"
  allTables
}

#Export a RezonatoRInfo object
exportInfo = function(allTables, foldername){
  allTables$wordTable = allTables$wordTable %>% select(-c(SepStrings, InChainIDVec, InCliqueIDVec))
  allTables$unitTable = allTables$unitTable %>% select(-c(wordListVector))
  allTables$rezTable = allTables$rezTable %>% select(-c(wordListVector, wordLocations, wordPlacesVector, wordPlaceBacksVector, unitListVector))
  allTables$trackTable = allTables$trackTable %>% select(-c(wordListVector, wordLocations, wordPlacesVector, wordPlaceBacksVector, unitListVector))
  allTables$cliqueTable = allTables$cliqueTable %>% select(-c(chainListVector, unitListVector, gapsList))
  allTables$stackTable = allTables$stackTable %>% select(-c(unitListVector, gapsList))
  
  if(!dir.exists(foldername)) dir.create(foldername)
  write_csv(allTables$cliqueTable, paste0(foldername,"/clique.csv"))
  write_csv(allTables$filterTable, paste0(foldername,"/filter.csv"))
  write_csv(allTables$hitTable, paste0(foldername,"/hit.csv"))
  write_csv(allTables$lineTable, paste0(foldername,"/line.csv"))
  write_csv(allTables$linkTable, paste0(foldername,"/link.csv"))
  write_csv(allTables$rezTable, paste0(foldername,"/rez.csv"))
  write_csv(allTables$searchTable, paste0(foldername,"/search.csv"))
  write_csv(allTables$stackTable, paste0(foldername,"/stack.csv"))
  write_csv(allTables$trackTable, paste0(foldername,"/track.csv"))
  write_csv(allTables$unitTable, paste0(foldername,"/unit.csv"))
  write_csv(allTables$vizCliqueTable, paste0(foldername,"/vizClique.csv"))
  write_csv(allTables$vizLinkTable, paste0(foldername,"/vizLink.csv"))
  write_csv(allTables$wordTable, paste0(foldername,"/word.csv"))
}

#Testing
setwd("G:/§Úªº¶³ºÝµwºÐ/1. Current research/RezonatoR") #Change this to G:/§Úªº¶³ºÝµwºÐ/1. Current research/RezonatoR/testfiles/raceTestFiles
csvs = importCSVs("sbc003rez CSV") #\SR200rez CSV / sbc003rez CSV
output = organiseCSVs(csvs)
exportInfo(output, "sbc003rez_NEW") #sbc003rez_NEW

#Other stuff, please ignore
wordTable = csvs$wordTable
wordTable = organiseWordTable(csvs$wordTable, csvs$dynaWordTable)
unitTable = organiseUnitTable(csvs$unitTable, wordTable)
linkTable = organiseLinkTable(csvs$linkTable, wordTable)

trackTable = organiseChainTable(csvs$trackTable, wordTable, linkTable)
rezTable = organiseChainTable(csvs$rezTable, wordTable, linkTable)

cliqueTable = organiseCliqueTable(csvs$cliqueTable, wordTable, unitTable, linkTable, rezTable)
stackTable = organiseStackTable(csvs$stackTable, unitTable)

wordTable = organiseWordTablePass2(wordTable, linkTable, rezTable, trackTable, cliqueTable)
unitTable = organiseUnitTablePass2(unitTable, wordTable, rezTable, trackTable)
cliqueTable = organiseCliqueTablePass2(cliqueTable, wordTable, unitTable, linkTable, rezTable)

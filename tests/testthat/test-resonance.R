# Only run this on your first time
# install.packages("devtools")
# devtools::install_github("kayaulai/rezonateR")

library(dplyr)
library(rezonateR)

obj = importRez("C:/Users/kayau/Documents/GitHub/rezonateR/inst/extData/sbc002-marked.rez", "sbc002", c("text", "transcript"))

obj = getAllTreeCorrespondences(obj, entity = "track")
obj = getAllTreeCorrespondences(obj, entity = "rez")
#Using the current file there will be some scary warning messages. If you replace the space in 'Chunk type' with a _ or something, this will be resolved.

obj$trackDF$default = obj$trackDF$default %>%
  rez_mutate(containerPred = findContainingChunk(obj$trackDF$default, obj$rezDF$default, proper = T))

obj = obj %>% addIsWordField(kind == "Word")

findSubject = function(verbDF, mentionDF, treeEntryDF){
  sapply(1:length(verbDF$Impersonal), function(x){
    if(!is.na(verbDF$Impersonal[x])){
      NA
    } else if(verbDF$id[x] %in% (mentionDF$containerPred)){
      mentionDF %>% filter(containerPred == verbDF$id[x]) %>% slice(1) %>% pull(id)
    } else if(verbDF$treeEntry[x] %in% treeEntryDF$parent){
      subjectTreeID = treeEntryDF %>% filter(parent == verbDF$treeEntry[x]) %>% slice(1) %>% pull(id)
      mentionDF %>% filter(treeEntry == subjectTreeID) %>% slice(1) %>% pull(id)
    } else {
      mentionsBefore = mentionDF %>% filter(docTokenSeqLast < verbDF$docTokenSeqFirst[x])
      mentionsBefore %>% filter(mentionsBefore$docTokenSeqLast == max(mentionsBefore$docTokenSeqLast)) %>% slice(1) %>% pull(id)
    }
  })
}

obj$rezDF$default = obj$rezDF$default %>% rez_mutate(subject = findSubject(obj$rezDF$default, obj$trackDF$default, obj$treeEntryDF$default))

obj$trackDF$default = obj$trackDF$default %>% rez_mutate(gapWordsTrue = tokensToLastMention(docWordSeqLast, zeroCond = "<0>"))

predDF = obj$rezDF$default %>% rez_left_join(obj$trackDF$default, df2Address = "trackDF/default", fkey = "subject", suffix = c("", "_subj"), by = c(subject = "id"))

predDF = predDF %>% mutate(verbSubjectGap = case_when(
  docWordSeqLast < docWordSeqFirst_subj ~ docWordSeqFirst_subj - docWordSeqLast,
  docWordSeqFirst > docWordSeqLast_subj ~  docWordSeqFirst - docWordSeqLast_subj,
  word == "<0>" ~ NA,
  T ~ 0
))

write_csv(predDF, "sbc002-pred.csv")

#verb-subject gap; export; exclude pause, laughter etc

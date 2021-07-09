library(devtools)
library(dplyr)
library(readr)
library(stringr)

setwd("C:/Users/User/Documents/GitHub/rezonateR")

#Modified from the original interlineaR function (I sent a pull request but it has not been responded to)
read.pangloss <- function(url,
                          DOI = NULL,
                          get.texts = TRUE,
                          get.sentences = TRUE,
                          get.words = TRUE,
                          get.morphemes = TRUE)
{
  corpusdoc <- read_xml(url);
  
  interlinearized <- list();
  
  texts <- xml_find_all(corpusdoc, "/TEXT")
  if (get.texts) {
    textsdf <- data.frame(text_id = xml_text(xml_find_first(texts, "@id")),
                          audio = xml_text(xml_find_first(texts, "HEADER/SOURDFILE/@href")));
    interlinearized$texts <- textsdf;
  }
  
  sentences <- xml_find_all(corpusdoc, "/TEXT/S");
  sentence_id <- xml_text(xml_find_first(sentences, "./@id"));
  if (get.sentences) {
    sentence.by.texts <- xml_find_num(texts, "count(./S)");
    
    sentencesdf <- data.frame(
      sentence_id = sentence_id,
      text_id     = rep(1:length(texts), times = sentence.by.texts),
      audio_start = xml_text(xml_find_all(texts, "/TEXT/S/AUDIO/@start")),
      audio_end   = xml_text(xml_find_all(texts, "/TEXT/S/AUDIO/@end")),
      form = xml_text(xml_find_all(texts, "/TEXT/S/FORM")),
      translation = xml_text(xml_find_all(texts, "/TEXT/S/TRANSL"))
    );
    interlinearized$sentences <- sentencesdf;
  }
  
  words <- xml_find_all(corpusdoc, "/TEXT/S/W");
  if (get.words) {
    word.by.texts <- xml_find_num(texts, "count(./S/W)");
    word.by.sentences <- xml_find_num(sentences, "count(./W)");
    wordsdf <- data.frame(
      word_id = 1:length(words),
      text_id     = rep(1:length(texts), times = word.by.texts),
      sentence_id = rep(sentencesdf$sentence_id, times = word.by.sentences)
    );
    interlinearized$words <- wordsdf;
  }
  
  morphemes <- xml_find_all(corpusdoc, "/TEXT/S/W/M");
  if (get.morphemes) {
    morph.by.texts <- xml_find_num(texts, "count(./S/W/M)");
    morph.by.sentences <- xml_find_num(sentences, "count(./W/M)");
    morph.by.words <- xml_find_num(words, "count(./M)");
    morphemesdf <- data.frame(
      morphem_id   = 1:length(morphemes),
      text_id      = rep(1:length(texts), times = morph.by.texts),
      sentence_id  = rep(sentencesdf$sentence_id, times = morph.by.sentences),
      word_id      = rep(1:length(words), times = morph.by.words),
      token        = xml_text(xml_find_first(morphemes, "./FORM")),
      gloss        = xml_text(xml_find_first(morphemes, "./TRANSL"))
    );
    interlinearized$morphemes <- morphemesdf;
  }
  return(interlinearized);
}

rez_pangloss = function(url, filename){
  infile = read.pangloss(url)
  rez_interlineaR(infile, filename)
}


rez_interlineaR = function(infile, filename){
  rezImport = infile$words
  sentences = infile$sentences %>% rename(sent_form = form, sent_translation = translation, sent_audio_start = audio_start, sent_audio_end = audio_end)
  rezImport = rezImport %>% left_join(infile$morphemes %>% group_by(word_id, text_id) %>% summarise(word_form = paste0(token, collapse = "-")), by = c("word_id", "text_id"))
  rezImport = rezImport %>% left_join(infile$morphemes %>% filter(!is.na(gloss)) %>% group_by(word_id, text_id) %>% summarise(word_gloss = paste0(gloss, collapse = "-")), by = c("word_id", "text_id"))
  rezImport = rezImport %>% left_join(sentences, by = c("sentence_id", "text_id"))
  write_tsv(rezImport, filename)
  rezImport
}

tamang1_rez = rez_pangloss("https://cocoon.huma-num.fr/data/mazaudon/masters/crdo-TAJ_ORIGTAM.xml", "tamang1.txt")

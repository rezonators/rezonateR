library(rezonateR)
setwd("C:/Users/kayau/Documents/GitHub/taiwan-mandarin-corpus-annotation")
filename_ryan = "8_manual_split/NCCU-TM025-CN-FM_Ryan.rez"
filename_orig = "7_rez_file/NCCU-TM025-CN-FM.rez"
nccu_tm025_orig = importRez(filename_orig, concatFields = "Utterance")
nccu_tm025_ryan = importRez(filename_ryan, concatFields = "Utterance")

nccu_tm025_ryan$unitDF = nccu_tm025_ryan$unitDF %>%
  rez_mutate(Utterance_nopunct = str_replace(Utterance, " (\\.|,|\\?|--)", ""))

rez_write_csv(nccu_tm025_orig$unitDF, "temp/tm025_orig.csv")
rez_write_csv(nccu_tm025_ryan$unitDF, "temp/tm025_ryan.csv")

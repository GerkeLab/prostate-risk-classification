library(fs)
library(SEERreadr)
library(tidyverse)

# importing SEER data ---------------------------------------------------------

tx <- fs::path("","Volumes","Lab_Gerke","SEER", "Nov2018", "SEER_1975_2016_TEXTDATA", "incidence")

col_pos <- seer_read_col_positions(paste0(tx, "/read.seer.research.nov2018.sas"))
col_pos
b <- seer_read_fwf(paste0(tx,"/yr1975_2016.seer9/MALEGEN.TXT"),
                   col_positions = col_pos)
c <- seer_read_fwf(paste0(tx,"/yr1992_2016.sj_lx_rg_ak/MALEGEN.TXT"),
                   col_positions = col_pos)
d <- seer_read_fwf(paste0(tx,"/yr2000_2016.gc_ky_la_nj_gg/MALEGEN.TXT"),
                   col_positions = col_pos)
e <- seer_read_fwf(paste0(tx,"/yr2005.la_2nd_half/MALEGEN.TXT"),
                   col_positions = col_pos)

seer <- b %>%
  bind_rows(c) %>%
  bind_rows(d) %>%
  bind_rows(e) 

rm(tx,col_pos, b, c, d, e)


# create risk classification variables --------------------------------------
# create percent of positive core SF12/SF13*100
seer$CS12SITE <- str_replace_all(seer$CS12SITE, c("991"= "NA", "988"= "NA", "998"= "NA", "999" = "NA"))
seer$CS13SITE <- str_replace_all(seer$CS13SITE, c("991"= "NA", "988"= "NA", "998"= "NA", "999" = "NA"))
seer$CS12SITE <- as.numeric(seer$CS12SITE)
seer$CS13SITE <- as.numeric(seer$CS13SITE)
seer$pos_core_percent <- pos_core_percent <- (seer$CS12SITE / seer$CS13SITE) * 100
rm(pos_core_percent)

seer <- seer %>%
  # D'Amico score : PSA, gleason, clinical t stage 
  # info : https://www.mdcalc.com/damico-risk-classification-prostate-cancer#evidence
  mutate(PSA = as.numeric(CS1SITE)/10) %>% ################################## be careful (PSA > 10 & PSA = 10 & PSA <= 20) | for EAU
  mutate(gleason = case_when(
    CS8SITE %in% c("988", "998", "999")               ~ NA_character_,
    CS8SITE %in% c("002", "003", "004", "005", "006") ~ "<=6",
    CS8SITE == "007"                                  ~ "7",
    CS8SITE == "008"                                  ~ "8",
    CS8SITE %in% c("009", "010")                      ~ "9-10"
  )) %>%
  mutate(tstage = case_when(
    # need to double check what to do with TX NOS
    #   DAJCCT: 19, 29, 39, 00, 01, 05
    DAJCCT %in% c("99", "00", "01", "05", "88") ~ NA_character_,
    DAJCCT %in% c("10", "19") ~ "T1",
    DAJCCT == "12" ~ "T1a",
    DAJCCT == "15" ~ "T1b",
    DAJCCT == "18" ~ "T1c",
    DAJCCT %in% c("20", "29") ~ "T2",
    DAJCCT == "21" ~ "T2a",
    DAJCCT == "22" ~ "T2b",
    DAJCCT == "23" ~ "T2c",
    DAJCCT %in% c("30", "39") ~ "T3",
    DAJCCT == "31" ~ "T3a",
    DAJCCT == "32" ~ "T3b",
    DAJCCT == "33" ~ "T3c",
    DAJCCT == "40" ~ "T4",
    DAJCCT == "41" ~ "T4a",
    DAJCCT == "42" ~ "T4b"
  )) %>%
  mutate(
    isup = case_when(
      GRADE == "9"    ~ NA_character_,
      GRADE == "1"    ~ "1",
      GRADE == "2"    ~ "2",
      GRADE == "3"    ~ "3",
      GRADE == "4"    ~ "4",
      GRADE == "5"    ~ "5"
    )
  )
# ISUP DONE :)--------------

# mutate ISUP------------------------
# Use GRADE from seer
# Code Grade Description
# 1 Grade Group 1: Gleason score less than or equal to 6
# 2 Grade Group 2: Gleason score 7
# Gleason pattern 3+4
# 3 Grade Group 3: Gleason score 7
# Gleason pattern 4+3
# 4 Grade Group 4: Gleason score 8
# 5 Grade Group 5: Gleason score 9 or 10


#mutate PSAD----------------- CANT
# 
# seer$CSTUMSIZ
# seer$CSTSEVAL
# seer$TUMSIZS

# GG1



seer <- seer %>% # For capra score
  mutate(
    cpsa = case_when(
      PSA <= 6                  ~ 0,
      (PSA > 6 & PSA <= 10)                  ~ 1,
      (PSA > 10 & PSA <= 20)           ~ 2,
      (PSA > 20 & PSA <= 30)             ~ 3,
      PSA > 30                    ~ 4,
      TRUE                                                                           ~ as.double(NA) 
    )) %>%
  mutate(
    cgleasan = case_when(
      CS9SITE %in% c("019", "029", "039", "049", "059", "099", "988", "998", "999")  ~ as.double(NA),
      CS9SITE %in% c("11", "12", "13", "21", "22", "23", "31", "32" , "33")          ~ 0,
      CS9SITE %in% c("15", "15", "24", "25", "34", "35")                             ~ 1,
      CS9SITE %in% c("41", "42", "43", "44", "45", "51", "52", "53", "54", "55")           ~ 3,
      TRUE                                                                           ~ as.double(NA) 
    )) %>%
  mutate(
    ctstage = case_when(
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a")                                ~ 0,
      tstage %in% c("T3a", "T3b","T3c", "T4", "T4a", "T4b", "T4c")                    ~ 1,
      TRUE                                                            ~ as.double(NA)
    )) %>%
  mutate(
    cper_core = case_when(
      pos_core_percent < "34"              ~ 0,
      pos_core_percent >= "34"             ~ 1,
      TRUE                                 ~ as.double(NA) 
    )) %>%
  mutate(
    cage = case_when(
     AGE_DX < 050                           ~ 0,
     AGE_DX >= 050                          ~ 1,
     TRUE                                 ~ as.double(NA) 
     )) %>%
  mutate(
    sum_point_capra = rowSums(.[149:152])
    )


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

# importing NCDB data ---------------------------------------------------------

ncdb_path <- path("", "Volumes", "Lab_Gerke", "prostateWorkGroup", "riskClassification", "data")
load(paste0(ncdb_path,"/raw.Rdata"))

ncdb <- dat

rm(dat)

# creating risk classification variables --------------------------------------

seer <- seer %>%
  # D'Amico score : PSA, gleason, clinical t stage 
  # info : https://www.mdcalc.com/damico-risk-classification-prostate-cancer#evidence
  mutate(PSA = as.numeric(CS1SITE)/10) %>%
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
  mutate(damico = case_when(
    tstage %in% c("T2c", "T3", "T4", "T3a", "T3b", "T3c", "T4a", "T4b") | 
      PSA > 20 | gleason %in% c("8", "9-10")                        ~ "High",
    tstage %in% c("T2b") |  (PSA > 10 & PSA <= 20) | gleason == "7" ~ "Intermediate",
    tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a") & PSA <=10 & 
      gleason == "<=6"                                              ~ "Low",
    TRUE                                                            ~ NA_character_
  )) %>%
  mutate(nice = case_when(
    tstage %in% c("T3", "T3a", "T3b", "T3c", "T4a", "T4b") | 
      PSA > 20 | gleason %in% c("8", "9-10") ~ "High",
    tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a") & PSA <=10 & 
      gleason == "<=6" ~ "Low",
    tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a", "T2", "T2b", "T2c") |
      gleason %in% c("<=6", "7") | PSA <=20 ~ "Intermediate",
    TRUE ~ NA_character_
  )) %>%
  mutate(eau = damico) %>% ################################################## Is not equal for PSA =10


ncdb <- ncdb %>%
  mutate(PSA = CS_SITESPECIFIC_FACTOR_1/10) %>%
  mutate(gleason = case_when(
    CS_SITESPECIFIC_FACTOR_8 <= 6         ~ "<=6",
    CS_SITESPECIFIC_FACTOR_8 == 7         ~ "7",
    CS_SITESPECIFIC_FACTOR_8 == 8         ~ "8",
    CS_SITESPECIFIC_FACTOR_8 %in% 9:10    ~ "9-10",
    CS_SITESPECIFIC_FACTOR_8 %in% 988:999 ~ NA_character_
  )) %>%
  mutate(tstage = case_when(
    TNM_CLIN_T %in% c("", "88", "c0", "cX", "pA", "pIS") ~ NA_character_,
    TNM_CLIN_T == "c1"                                   ~ "T1",
    TNM_CLIN_T == "c1A"                                  ~ "T1a",
    TNM_CLIN_T == "c1B"                                  ~ "T1b",
    TNM_CLIN_T == "c1C"                                  ~ "T1c",
    TNM_CLIN_T == "c2"                                   ~ "T2",
    TNM_CLIN_T == "c2A"                                  ~ "T2a",
    TNM_CLIN_T == "c2B"                                  ~ "T2b",
    TNM_CLIN_T == "c2C"                                  ~ "T2c",
    TNM_CLIN_T == "c3"                                   ~ "T3",
    TNM_CLIN_T == "c3A"                                  ~ "T3a",
    TNM_CLIN_T == "c3B"                                  ~ "T3b",
    TNM_CLIN_T == "c4"                                   ~ "T4"
  )) %>%
  mutate(damico = case_when(
    tstage %in% c("T2c", "T3", "T4", "T3a", "T3b", "T3c", "T4a", "T4b") | 
      PSA > 20 | gleason %in% c("8", "9-10")                        ~ "High",
    tstage %in% c("T2b") |  (PSA > 10 & PSA <= 20) | gleason == "7" ~ "Intermediate",
    tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a") & PSA <=10 & 
      gleason == "<=6"                                              ~ "Low",
    TRUE                                                            ~ NA_character_
  )) %>%
  mutate(nice = case_when(
    tstage %in% c("T3", "T3a", "T3b", "T3c", "T4a", "T4b") | 
      PSA > 20 | gleason %in% c("8", "9-10") ~ "High",
    tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a") & PSA <=10 & 
      gleason == "<=6" ~ "Low",
    tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a", "T2", "T2b", "T2c") |
      gleason %in% c("<=6", "7") | PSA <=20 ~ "Intermediate",
    TRUE ~ NA_character_
  )) %>%
  mutate(eau = damico)

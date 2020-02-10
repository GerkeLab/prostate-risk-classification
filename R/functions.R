# data import functions -------------------------------------------------------
seer_import <- function(seer_path){
  
  col_pos <- seer_read_col_positions(fs::path(seer_path, "read.seer.research.nov2018.sas"))
  
  # beyond the primary seer9 "male genital cancer" file, there are 3 additional
  # files which are needed to capture staggered SEER entry for certain regions
  seer <- bind_rows(
    seer_read_fwf(fs::path(seer_path,"yr1975_2016.seer9", "MALEGEN.TXT"),
                  col_positions = col_pos),
    seer_read_fwf(fs::path(seer_path,"yr1992_2016.sj_lx_rg_ak", "MALEGEN.TXT"),
                  col_positions = col_pos),
    seer_read_fwf(fs::path(seer_path,"yr2000_2016.gc_ky_la_nj_gg", "MALEGEN.TXT"),
                  col_positions = col_pos),
    seer_read_fwf(fs::path(seer_path,"yr2005.la_2nd_half", "MALEGEN.TXT"),
                  col_positions = col_pos)
  )

}

ncdb_import <- function(ncdb_path){
  
  ncdb <- haven::read_sas(fs::path(ncdb_path,"ncdb_puf.sas7bdat"), NULL)
  
}

# data cleaning functions -----------------------------------------------------

seer_classification <- function(seer_raw){
  # step 1 : create the necessary codings for calculating the risk scores
      # would it be clearer to do so in a big chunk at the beginning or to 
      # place them near the scores they are associated with? Or does it not
      # matter? 
  
  seer <- seer_raw %>% 
    # create variables for calulating risk scores -------------------
    mutate(psa = as.numeric(CS1SITE)/10) %>% 
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
    mutate(isup = case_when(
      GRADE == "9"    ~ NA_character_,
      GRADE == "1"    ~ "1",
      GRADE == "2"    ~ "2",
      GRADE == "3"    ~ "3",
      GRADE == "4"    ~ "4",
      GRADE == "5"    ~ "5"
    )) %>%
    mutate_at(c("CS12SITE", "CS13SITE"), 
              ~ case_when(
                . %in% c("991", "988", "998", "999") ~ NA_real_,
                TRUE ~ as.numeric(.)
              )) %>% 
    mutate(percent_pos_cores = (CS12SITE / CS13SITE) * 100) %>%
    # create capra specific groups to add together - starting each 
    # variable name with "capra_" so they can be easily filtered
    # out later if need be and cleaning up spacing so easier to read 
    mutate(capra_psa = case_when(
      psa <= 6               ~ 0,
      (psa > 6 & psa <= 10)  ~ 1,
      (psa > 10 & psa <= 20) ~ 2,
      (psa > 20 & psa <= 30) ~ 3,
      psa > 30               ~ 4,
      TRUE ~ NA_real_
    )) %>%
    mutate(capra_gleasan = case_when(
      CS9SITE %in% c("11", "12", "13", "21", "22", "23",
                     "31", "32" , "33")                  ~ 0,
      CS9SITE %in% c("15", "15", "24", "25", "34", "35") ~ 1,
      CS9SITE %in% c("41", "42", "43", "44", "45", "51",
                     "52", "53", "54", "55")             ~ 3,
      TRUE                                               ~ NA_real_
    )) %>%
    mutate(capra_tstage = case_when(
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a")              ~ 0,
      tstage %in% c("T3a", "T3b","T3c", "T4", "T4a", "T4b", "T4c") ~ 1,
      TRUE                                                         ~ NA_real_
    )) %>%
    mutate(capra_per_pos = case_when(
      percent_pos_cores < 34  ~ 0,
      percent_pos_cores >= 34 ~ 1,
      TRUE                   ~ NA_real_ 
    )) %>%
    mutate(capra_age = case_when(
      as.numeric(AGE_DX) < 50                             ~  0,
      as.numeric(AGE_DX) >= 50 & as.numeric(AGE_DX) < 131 ~ 1,
      TRUE                                                ~ NA_real_
    )) %>%
    mutate(capra_score = rowSums(select(.,capra_psa:capra_age))) %>% 
    # creating actual risk scores -----------------------------------
    mutate()
  
}
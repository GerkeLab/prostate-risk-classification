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

seer_recoding <- function(seer_raw){
  # create the necessary codings for calculating the risk scores -
      # would it be clearer to do so do all the recoding and cleanining in 
      # one step and the actual classification in another? Or does it not
      # even matter? 
  
  seer <- seer_raw %>% 
    # create variables for calulating risk scores -------------------
    mutate_at(("CS1SITE"), 
            ~ case_when(
              . %in% c("000","988", "989","990", "997", "998", "999") ~ NA_real_, #------Check good
              TRUE ~ as.numeric(.)
            )) %>% 
    mutate(psa = as.numeric(CS1SITE)/10) %>%
    mutate(gleason = case_when(
      as.numeric(CS8SITE) > 10                          ~ NA_character_,#----------------Check good
      CS8SITE %in% c("002", "003", "004", "005", "006") ~ "<=6",
      CS8SITE == "007"                                  ~ "7",
      CS8SITE == "008"                                  ~ "8",
      CS8SITE %in% c("009", "010")                      ~ "9-10"
    )) %>%
    mutate(tstage = case_when(
      # need to double check what to do with TX NOS
      #   DAJCCT: 19, 29, 39, 00, 01, 05
      DAJCCT %in% c("99", "00", "01", "05", "88") ~ NA_character_,#--------------Need update (almost ready)
      DAJCCT %in% c("10", "19") ~ "T1",
      (DAJCCT >= "12" & DAJCCT < "15") ~ "T1a",
      (DAJCCT >= "15" & DAJCCT < "18") ~ "T1b",
      DAJCCT == "18" ~ "T1c",
      DAJCCT %in% c("20", "29") ~ "T2",
      (DAJCCT >= "21" & DAJCCT < "22") ~ "T2a",
      DAJCCT == "22" ~ "T2b",
      DAJCCT == "23" ~ "T2c",
      DAJCCT %in% c("30", "39") ~ "T3",
      DAJCCT == "31" ~ "T3a",
      DAJCCT == "32" ~ "T3b",
      DAJCCT == "33" ~ "T3c",
      DAJCCT %in% c("40", "49") ~ "T4",
      DAJCCT == "41" ~ "T4a",
      DAJCCT == "42" ~ "T4b",
      DAJCCT == "43" ~ "T4c",
      DAJCCT == "44" ~ "T4d"
    )) %>%
    mutate(isup = case_when(
      GRADE == "9"    ~ NA_real_,#----------------Check good
      TRUE ~ as.numeric(GRADE)
    )) %>%
    mutate_at(c("CS12SITE", "CS13SITE"), 
              ~ case_when(
                . %in% c("991", "988", "998", "999") ~ NA_real_,#----------------Doesn't work
                TRUE ~ as.numeric(.)
              )) %>% 
    mutate(percent_pos_cores = (CS12SITE / CS13SITE) * 100) %>%
    # create capra specific groups to add together - starting each 
    # variable name with "capra_" so they can be easily filtered
    # out later if need be and cleaning up spacing so easier to read 
    mutate(capra_psa = case_when(
      psa <= 6               ~ 0,#----------------Check good
      (psa > 6 & psa <= 10)  ~ 1,
      (psa > 10 & psa <= 20) ~ 2,
      (psa > 20 & psa <= 30) ~ 3,
      psa > 30               ~ 4,
      TRUE ~ NA_real_
    )) %>%
    mutate(capra_gleasan = case_when( #-------------------------------------------is actually now working
      CS9SITE %in% c("011", "012", "013", "021", "022", "023",
                     "031", "032" , "033")                  ~ 0,
      CS9SITE %in% c("014", "015", "024", "025", "034", "035") ~ 1,
      CS9SITE %in% c("041", "042", "043", "044", "045", "051",
                     "052", "053", "054", "055")             ~ 3,
      TRUE                                               ~ NA_real_
    )) %>%
    mutate(capra_tstage = case_when(
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a")              ~ 0,
      tstage %in% c("T3a", "T3b","T3c", "T4", "T4a", "T4b", "T4c") ~ 1,
      TRUE                                                         ~ NA_real_
    )) %>%
    mutate(capra_per_pos = case_when(
      as.numeric(percent_pos_cores) < 34  ~ 0,
      as.numeric(percent_pos_cores) >= 34 ~ 1,#----------------Will have too much one beacuse 991 etc are not removed in CS12 and 13
      TRUE                   ~ NA_real_ 
    )) %>%
    mutate(capra_age = case_when(
      as.numeric(AGE_DX) < 50                             ~  0,
      as.numeric(AGE_DX) >= 50 & as.numeric(AGE_DX) < 131 ~ 1,
      TRUE                                                ~ NA_real_
    )) %>%
    # not sure if the below is the correct method - currently we dont have any cases 
    # with all the info complete to calculate capra so I calculate as much as we can ... 
    # other option with the other paper did was to impute all missing and then calculate
    mutate(capra_score = rowSums(select(.,capra_psa:capra_age), na.rm = TRUE)) %>%
    # create risk classifications -----------------------------------
    mutate(damico = case_when( # need to figure out T2 - stages less than T1c and greater than T2c were included as low and high
      tstage %in% c("T2c", "T3", "T4", "T3a", "T3b", "T3c", "T4a", "T4b") | 
        psa > 20 | 
        gleason %in% c("8", "9-10")                                    ~ "High",
      tstage == "T2b" |  
        (psa > 10 & psa <= 20) | 
        gleason == "7"                                                 ~ "Intermediate",
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a") &
        psa <= 10 & 
        gleason == "<=6"            ~ "Low",
      TRUE                          ~ NA_character_
    )) %>% 
    mutate(nice = case_when( # need to figure out T2
      tstage %in% c("T3", "T3a", "T3b", "T3c", "T4a", "T4b", "T2c") | 
        psa > 20 |
        gleason %in% c("8", "9-10") ~ "High",
      tstage == "T2b" |
        gleason == "7" |
        between(psa, 10, 20)        ~ "Intermediate",
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a") & 
        psa < 10 & 
        gleason == "<=6"            ~ "Low",
      TRUE ~ NA_character_
    )) %>% 
    mutate(eau = nice) %>%
    # create numeric versions of categories -------------------------
    mutate_at(c("damico", "nice"),
              .funs = list(num = ~ case_when(
                . == "Low" ~ 1,
                . == "Intermediate" ~ 2,
                . == "High" ~ 3,
                TRUE ~ NA_real_
              ))) %>% 
    # misc cleaning -------------------------------------------------
    mutate(os = case_when(
      STAT_REC == 0 ~ 1,
      STAT_REC == 1 ~ 0,
      TRUE ~ NA_real_
    )) %>% 
    mutate(SRV_TIME_MON = as.numeric(SRV_TIME_MON)) %>%
    mutate(SRV_TIME_MON = case_when(
      SRV_TIME_MON == 9999 ~ NA_real_,
      TRUE ~ SRV_TIME_MON
    ))
  
}

ncdb_recoding <- function(ncdb_raw){
  # create the necessary codings for calculating the risk scores -
      # would it be clearer to do so do all the recoding and cleanining in 
      # one step and the actual classification in another? Or does it not
      # even matter? 
  
  ncdb <- ncdb_raw %>% 
    # create variables for calulating risk scores -------------------
    mutate(psa = CS_SITESPECIFIC_FACTOR_1/10) %>% 
    mutate(gleason = case_when(
      CS_SITESPECIFIC_FACTOR_8 %in% c(988:999) ~ NA_character_,
      CS_SITESPECIFIC_FACTOR_8 %in% c(2:6)     ~ "<=6",
      CS_SITESPECIFIC_FACTOR_8 == 7            ~ "7",
      CS_SITESPECIFIC_FACTOR_8 == 8            ~ "8",
      CS_SITESPECIFIC_FACTOR_8 %in% c(9, 10)   ~ "9-10"
    )) %>%
    mutate(tstage = case_when(
      # need to double check what to do with : 0, 0A, 0IS
      TNM_CLIN_T %in% c("", "88", "c0", "cX", "pA", "pIS") ~ NA_character_,
      TNM_CLIN_T == "c1"  ~ "T1",
      TNM_CLIN_T == "c1A" ~ "T1a",
      TNM_CLIN_T == "c1B" ~ "T1b",
      TNM_CLIN_T == "c1C" ~ "T1c",
      TNM_CLIN_T == "c2"  ~ "T2",
      TNM_CLIN_T == "c2A" ~ "T2a",
      TNM_CLIN_T == "c2B" ~ "T2b",
      TNM_CLIN_T == "c2C" ~ "T2c",
      TNM_CLIN_T == "c3"  ~ "T3",
      TNM_CLIN_T == "c3A" ~ "T3a",
      TNM_CLIN_T == "c3B" ~ "T3b",
      TNM_CLIN_T == "c3C" ~ "T3c",
      TNM_CLIN_T == "c4"  ~ "T4"
    )) %>%
    mutate(isup = case_when(
      GRADE == 9 ~ NA_real_,
      TRUE       ~ GRADE
    )) %>%
    mutate_at(c("CS_SITESPECIFIC_FACTOR_12", "CS_SITESPECIFIC_FACTOR_13"), 
              ~ case_when(
                . %in% c(991:999) ~ NA_real_,
                TRUE ~ .
              )) %>% 
    mutate(percent_pos_cores = (CS_SITESPECIFIC_FACTOR_12 / CS_SITESPECIFIC_FACTOR_13) * 100) %>%
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
      CS_SITESPECIFIC_FACTOR_9 %in% c(11:13, 21:23, 31:33) ~ 0,
      CS_SITESPECIFIC_FACTOR_9 %in% c(14:15, 24:25, 34:35) ~ 1,
      CS_SITESPECIFIC_FACTOR_9 %in% c(41:45, 51:55)        ~ 3,
      TRUE                                                 ~ NA_real_
    )) %>%
    mutate(capra_tstage = case_when(
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a")              ~ 0,
      tstage %in% c("T3a", "T3b","T3c", "T4", "T4a", "T4b", "T4c") ~ 1,
      TRUE                                                         ~ NA_real_
    )) %>%
    mutate(capra_per_pos = case_when(
      percent_pos_cores < 34  ~ 0,
      percent_pos_cores >= 34 ~ 1,
      TRUE                    ~ NA_real_ 
    )) %>%
    mutate(capra_age = case_when(
      AGE < 50              ~  0,
      AGE >= 50 & AGE < 131 ~ 1,
      TRUE                  ~ NA_real_
    )) %>%
    mutate(capra_score = rowSums(select(.,capra_psa:capra_age), na.rm=TRUE)) %>%
    # create risk classifications -----------------------------------
    mutate(damico = case_when( # need to figure out T2 - stages less than T1c and greater than T2c were included as low and high
      tstage %in% c("T2c", "T3", "T4", "T3a", "T3b", "T3c", "T4a", "T4b") | 
        psa > 20 | 
        gleason %in% c("8", "9-10") ~ "High",
      tstage == "T2b" |  
        (psa > 10 & psa <= 20) | 
        gleason == "7"              ~ "Intermediate",
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a") &
        psa <= 10 & 
        gleason == "<=6"            ~ "Low",
      TRUE                          ~ NA_character_
    )) %>% 
    mutate(nice = case_when( # need to figure out T2
      tstage %in% c("T3", "T3a", "T3b", "T3c", "T4a", "T4b", "T2c") | 
        psa > 20 |
        gleason %in% c("8", "9-10") ~ "High",
      tstage == "T2b" |
        gleason == "7" |
        between(psa, 10, 20)        ~ "Intermediate",
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a") & 
        psa < 10 & 
        gleason == "<=6"            ~ "Low",
      TRUE ~ NA_character_
    )) %>% 
    mutate(eau = nice) %>% 
    # create numeric versions of categories -------------------------
    mutate_at(c("damico", "nice"),
              .funs = list(num = ~ case_when(
                . == "Low" ~ 1,
                . == "Intermediate" ~ 2,
                . == "High" ~ 3,
                TRUE ~ NA_real_
              ))) %>% 
    # misc cleaning -------------------------------------------------
    mutate(os = case_when(
      PUF_VITAL_STATUS == 1 ~ 0,
      PUF_VITAL_STATUS == 0 ~ 1,
      TRUE ~ NA_real_
    ))
  
}

make_structured_noise <- function(data,
                                  identifier, 
                                  outcome, 
                                  outcome_time, 
                                  numeric_vars){
  
  data_ml <- data %>%
    select({{identifier}}, {{outcome}}, {{outcome_time}}, contains("capra_"),
           {{numeric_vars}}) %>% 
    # mulitple versions of all numeric variables (not survival related)
    mutate_at(c(contains("capra_"), numeric_vars), .funs = list(logged = ~ log(.))) %>% 
    mutate_at(c(contains("capra_"), numeric_vars), .funs = list(frac_poly_2 = ~ .^(1/2))) %>% 
    mutate_at(c(contains("capra_"), numeric_vars), .funs = list(frac_poly_3 = ~ .^(1/3))) 
}

# performance measures of predicting overall survival -------------------------

ncdb_auc <- function(data,
                     training_percentage = 0.70,
                     outcome = "os",
                     time_to_outcome = "DX_LASTCONTACT_DEATH_MONTHS",
                     classifiers = c("capra_score")){

  training_data <- data %>% 
    drop_na({{outcome}}, {{time_to_outcome}}, {{classifiers}}) %>% 
    sample_frac(training_percentage) 
  
  testing_data <- data %>% 
    drop_na({{outcome}}, {{time_to_outcome}}, {{classifiers}}) %>% 
    sample_frac(1-training_percentage) 
  
  basic_model <- coxph(Surv(DX_LASTCONTACT_DEATH_MONTHS, os) ~ capra_score,
                       data = training_data,
                       x = TRUE, y = TRUE)
  
  predicted_values <- predict(basic_model, newdata = testing_data)
  Surv.training <- Surv(training_data$DX_LASTCONTACT_DEATH_MONTHS, training_data$os)
  Surv.testing <- Surv(testing_data$DX_LASTCONTACT_DEATH_MONTHS, testing_data$os)
  times <- seq(0, 150, 1)
  auc.uno <- AUC.uno(Surv.training, Surv.testing, predicted_values, times)
  auc.uno$iauc
  
  sens.uno(Surv.training, Surv.testing, predicted_values, times)
  spec.uno(Surv.testing, predicted_values, times)
 
}
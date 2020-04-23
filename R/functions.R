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
    mutate_at(("CS1SITE"), ~ case_when( 
      . %in% c("000","988", "989","990", "997", "998", "999") ~ NA_real_,
      TRUE ~ as.numeric(.) 
    )) %>%
    mutate(psa = CS1SITE/10) %>% 
    mutate(gleason = case_when(
      as.numeric(CS8SITE) > 10                          ~ NA_character_,
      CS8SITE %in% c("002", "003", "004", "005", "006") ~ "<=6",
      CS8SITE == "007"                                  ~ "7",
      CS8SITE == "008"                                  ~ "8",
      CS8SITE %in% c("009", "010")                      ~ "9-10"
    )) %>%
    mutate(tstage = case_when(
      # Remove TX and T0
      # Added the NOS to their corresponding stage
      # Choose DAJCCT var over DAJCC7T because includes all patients (6-7) vs patients in edition 7th only
      DAJCCT %in% c("99", "00", "01", "05", "06", "07", "88") ~ NA_character_,
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
      GRADE == "9" ~ NA_real_,
      TRUE         ~ as.numeric(GRADE)
    )) %>%
    mutate_at(c("CS12SITE", "CS13SITE", "CS7SITE", "AGE_DX", "YEAR_DX"), 
              ~ case_when(
                . %in% c("991", "988", "998", "999", "990", "992", "993", "995", "997") ~ NA_real_,
                TRUE ~ as.numeric(.)
              )) %>% 
    mutate_at(c("MAR_STAT"), 
              ~ case_when(
                . %in% c("9") ~ NA_real_,
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
    mutate(capra_gleason = case_when(
      CS7SITE %in% c(11:13, 21:23, 31:33) ~ 0,
      CS7SITE %in% c(14:15, 24:25, 34:35) ~ 1,
      CS7SITE %in% c(41:45, 51:55)        ~ 3,
      TRUE                                ~ NA_real_
    )) %>%
    mutate(capra_tstage = case_when(
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2", "T2a", "T2b", "T2c") ~ 0,
      tstage %in% c("T3", "T3a", "T3b","T3c", "T4", "T4a", "T4b", "T4c")  ~ 1,
      TRUE                                                                ~ NA_real_
    )) %>%
    mutate(capra_per_pos = case_when(
      percent_pos_cores < 34  ~ 0,
      percent_pos_cores >= 34 ~ 1,
      TRUE                   ~ NA_real_ 
    )) %>%
    mutate(capra_age = case_when(
      AGE_DX < 50                 ~  0,
      AGE_DX >= 50 & AGE_DX < 131 ~ 1,
      TRUE                        ~ NA_real_
    )) %>%
    # misc cleaning -------------------------------------------------
    mutate(race = case_when(
      RAC_RECA == 1 ~ "white",
      RAC_RECA == 2 ~ "black",
      RAC_RECA == 3 ~ "other",
      RAC_RECA == 7 ~ "other - unspecified",
      TRUE ~ "unknown"
    )) %>%
    mutate(marital_status = case_when(
      MAR_STAT %in% c(2,6) ~ 1,
      TRUE ~ 0
    )) %>%
    mutate(os = case_when(
      STAT_REC == "0" ~ 1,
      STAT_REC == "1" ~ 0,
      TRUE ~ NA_real_
    )) %>% 
    mutate(os_pca = case_when(
      PRIMSITE == "C619" & VSRTSADX == "1" ~ 1,
      TRUE ~ 0
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
    mutate_at(("CS_SITESPECIFIC_FACTOR_1"), ~ case_when( 
      . > 990 ~ NA_real_, # need to check on ncdb ########################################## CCL 980 etc
      TRUE ~ . 
    )) %>%
    mutate(psa = CS_SITESPECIFIC_FACTOR_1/10) %>% 
    mutate(gleason = case_when(
      CS_SITESPECIFIC_FACTOR_8 > 10 ~ NA_character_,
      CS_SITESPECIFIC_FACTOR_8 <= 6 ~ "<=6",
      CS_SITESPECIFIC_FACTOR_8 == 7 ~ "7",
      CS_SITESPECIFIC_FACTOR_8 == 8 ~ "8",
      CS_SITESPECIFIC_FACTOR_8 <= 10 ~ "9-10"
    )) %>%
    mutate(tstage = case_when(
      # need to double check what to do with : 0, 0A, 0IS -> removed because non detectable
      TNM_CLIN_T %in% c("","88", "c0", "cX", "pA", "pIS") ~ NA_character_,
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
    mutate_at(c("CS_SITESPECIFIC_FACTOR_12", "CS_SITESPECIFIC_FACTOR_13",
                "CS_SITESPECIFIC_FACTOR_7"),
              ~ case_when(
                . %in% c(988:999) ~ NA_real_,
                TRUE ~ .
              )) %>% 
    mutate(percent_pos_cores = CS_SITESPECIFIC_FACTOR_12 / CS_SITESPECIFIC_FACTOR_13 * 100) %>%
    mutate_at(("percent_pos_cores"), ~ case_when( # when nb core examined < nb core positve = wrong record
      . > 100 ~ NA_real_,
      TRUE ~ .
    )) %>% 
  # Check table afetr rerun
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
    mutate(capra_gleason = case_when(
      CS_SITESPECIFIC_FACTOR_7 %in% c(11:13, 21:23, 31:33) ~ 0, 
      CS_SITESPECIFIC_FACTOR_7 %in% c(14:15, 24:25, 34:35) ~ 1,
      CS_SITESPECIFIC_FACTOR_7 %in% c(41:45, 51:55)        ~ 3,
      TRUE                                                 ~ NA_real_
    )) %>%
    mutate(capra_tstage = case_when(
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2", "T2a", "T2b", "T2c")    ~ 0,
      tstage %in% c("T3a", "T3b","T3c", "T4", "T4a", "T4b", "T4c")           ~ 1,
      TRUE                                                                   ~ NA_real_
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
    # treatment information  ----------------------------------------
    mutate(surgery = case_when(
      RX_SUMM_SURG_PRIM_SITE == 0 ~ "No Surgery",
      RX_SUMM_SURG_PRIM_SITE %in% c(30, 50, 70, 80) ~ "Prostatectomy",
      TRUE ~ "Other/Unknown"
    )) %>%
    mutate(radiation = case_when(
      RX_SUMM_RADIATION == 0 ~ "No Radiation",
      RX_SUMM_RADIATION == 9 ~ "Unknown",
      TRUE ~ "Radiation"
    )) %>%
    mutate(radiation_cat = case_when(
      RAD_BOOST_RX_MODALITY %in% 50:54 & RAD_REGIONAL_RX_MODALITY %in% c(51:54, 0, 99) ~ "Brachy",
      RAD_BOOST_RX_MODALITY %in% c(0, 99) & RAD_REGIONAL_RX_MODALITY %in% 50:54 ~ "Brachy",
      RAD_BOOST_RX_MODALITY %in% 50:54 & RAD_REGIONAL_RX_MODALITY %in% c(20, 22:27, 31:32) ~ "EBRT + brachy",
      RAD_BOOST_RX_MODALITY %in% c(20, 22:27, 31:32) & RAD_REGIONAL_RX_MODALITY %in% 50:54 ~ "EBRT + brachy",
      RAD_BOOST_RX_MODALITY %in% c(20, 22:27, 31:32) & RAD_REGIONAL_RX_MODALITY %in% c(20, 22:27, 31:32, 0, 99) ~ "EBRT",
      RAD_BOOST_RX_MODALITY %in% c(0, 99) & RAD_REGIONAL_RX_MODALITY %in% c(20, 22:27, 31:32) ~ "EBRT",
      RAD_BOOST_RX_MODALITY %in% c(28:30, 40:43, 55, 60:62, 80, 85, 98) & 
        RAD_REGIONAL_RX_MODALITY %in% c(28:30, 40:43, 55, 60:62, 80, 85, 98) ~ "Other",
      TRUE ~ "Unknown"
    )) %>%
    mutate(trt_typeI = case_when(
      radiation_cat %in% c("Brachy", "EBRT", "EBRT + brachy", "Other") & surgery == "No Surgery" ~ "Radiation",
      radiation_cat == "Unknown" & surgery == "No Surgery" ~ "No RX",
      radiation_cat == "Unknown" & surgery == "Prostatectomy" ~ "Prostatectomy",
      radiation_cat == "Unknown" & surgery == "Other/Unknown" ~ "Other Surgery",
      radiation_cat == "Brachy" & surgery == "Other/Unknown" &
        DX_RAD_STARTED_DAYS > 0 & DX_DEFSURG_STARTED_DAYS == 0 ~ "Radiation - secondary",
      radiation_cat == "Brachy" & surgery == "Other/Unknown" &
        DX_RAD_STARTED_DAYS > 0 & is.na(DX_DEFSURG_STARTED_DAYS) ~ "Radiation",
      radiation_cat == "Brachy" & surgery == "Other/Unknown" &
        DX_RAD_STARTED_DAYS == DX_DEFSURG_STARTED_DAYS ~ "Radiation",
      radiation_cat == "EBRT" & surgery == "Prostatectomy" & 
        DX_DEFSURG_STARTED_DAYS < DX_RAD_STARTED_DAYS ~ "Prostatectomy/Radiation",
      radiation_cat == "EBRT" & surgery == "Prostatectomy" & 
        is.na(DX_DEFSURG_STARTED_DAYS) & is.na(DX_RAD_STARTED_DAYS) ~ "Other"
    )) %>% 
    mutate(trt_typeII = case_when(
      trt_typeI %in% c("Prostatectomy", "Prostatectomy/Radiation") ~ "Prostatectomy",
      trt_typeI == "Radiation" ~ "Radiation",
      trt_typeI == "No RX" ~ "No Rx",
      TRUE ~ "Other/Cant Determine"
    )) %>%
    mutate(trt_typeIII = case_when(
      RX_SUMM_TREATMENT_STATUS %in% c(0,9) & trt_typeII == "No RX" ~ "No RX",
      RX_SUMM_TREATMENT_STATUS == 1 & trt_typeII == "No RX" ~ "Other/Cant determine",
      RX_SUMM_TREATMENT_STATUS == 2 & trt_typeII == "No RX" ~ "Active Surveillance",
      TRUE ~ trt_typeII
    )) %>%
    # misc cleaning -------------------------------------------------
    mutate(os = case_when(
      PUF_VITAL_STATUS == 1 ~ 0,
      PUF_VITAL_STATUS == 0 ~ 1,
      TRUE ~ NA_real_
    ))
  
}

# imputing variables for calculating risk scores ------------------------------

# impute_data <- function(data, 
#                         method = "mean",
#                         id = "PUBCSNUM",
#                         varlist = c("psa", "tstage", "gleason", "isup",
#                                     "percent_pos_cores", "CS7SITE",
#                                     "CS12SITE", "capra_psa",
#                                     "capra_gleason", "capra_tstage",
#                                     "capra_per_pos", "capra_age")){
#   
#   non_imp_vars <- setdiff(colnames(data), varlist)
#   imp_vars <- c(id, varlist)
#   
#   if (method == "mean"){
#     
#     data_imp <- data %>%
#       select(!!!imp_vars) %>%
#       mutate_if(is.numeric, funs(case_when(
#         is.na(.) ~ round(mean(., na.rm=TRUE)),
#         TRUE ~ .
#       ))) %>%
#       mutate_if(negate(is.numeric), funs(case_when(
#         is.na(.) ~ unique(na.omit(.))[which.max(tabulate(match(.,unique(na.omit(.)))))],
#         TRUE ~ .
#       ))) %>%
#       left_join(data %>% 
#                   select(!!!non_imp_vars), by = id)
#     
#   }  else if (method == "median") {
#     
#     data_imp <- data %>%
#       select(!!!imp_vars) %>%
#       mutate_if(is.numeric, funs(case_when(
#         is.na(.) ~ median(., na.rm=TRUE),
#         TRUE ~ .
#       ))) %>%
#       mutate_if(negate(is.numeric), funs(case_when(
#         is.na(.) ~ unique(na.omit(.))[which.max(tabulate(match(.,unique(na.omit(.)))))],
#         TRUE ~ .
#       ))) %>%
#       left_join(data %>% 
#                   select(!!!non_imp_vars), by = id)
#   } else { # needs work -----------------------------------
#     imp <- data %>%
#       select(!!!imp_vars) %>%
#       mice(m = 3, method = c('polyreg', 'pmm', 'polyreg', 'polyreg'),
#            seed = 8675309)
#   }
# }

# calculating risk scores -----------------------------------------------------

risk_scores <- function(data,
                        gleason_var = "CS7SITE",
                        pos_cores_var = "CS12SITE"){
  risk_data <- data %>% 
    # Include T2 with T2a stage by reading SEER-NIH rules for abstraction
    # https://staging.seer.cancer.gov/tnm/input/1.9/prostate/clin_t/?breadcrumbs=(~schema_list~),(~view_schema~,~prostate~)
    mutate(damico = case_when(
      tstage %in% c("T2c", "T3", "T4", "T3a", "T3b", "T3c", "T4a", "T4b") |
        psa > 20 |
        gleason %in% c("8", "9-10") ~ "High",
      tstage == "T2b" |
        (psa > 10 & psa <= 20) |
        gleason == "7"              ~ "Intermediate",
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a", "T2") &
        psa <= 10 &
        gleason == "<=6"            ~ "Low",
      TRUE                          ~ NA_character_
    )) %>%
    mutate(nice = case_when( 
      tstage %in% c("T2c", "T3", "T3a", "T3b", "T3c", "T4", "T4a", "T4b") | 
        psa > 20 |
        gleason %in% c("8", "9-10")                                               ~ "High",
      tstage == "T2b" |
        gleason == "7" |
        between(psa, 10, 20)                                                      ~ "Intermediate",
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2", "T2a") & 
        psa < 10 & 
        gleason == "<=6"                                                          ~ "Low",
      TRUE                                                                        ~ NA_character_
    )) %>%
    mutate(eau = case_when(
      tstage %in% c("T2c", "T3", "T4", "T3a", "T3b", "T3c", "T4a", "T4b") |
        psa > 20 |
        gleason %in% c("8", "9-10") ~ "High",
      tstage == "T2b" |
        (psa >= 10 & psa <= 20) |
        gleason == "7"              ~ "Intermediate",
      tstage %in% c("T1", "T1a", "T1b", "T1c", "T2a", "T2") &
        psa < 10 &
        gleason == "<=6"            ~ "Low",
      TRUE                          ~ NA_character_
    )) %>%
    mutate(GUROC = case_when(
        psa > 20 |
          gleason %in% c("8", "9-10") |
          tstage %in% c("T3", "T3a", "T3b", "T3c", "T4a", "T4b", "T4", "T4c") ~ "High",
        # wriie low before intermediate to compare with "not otherwise low risk
        psa <= 10 &
          gleason == "<=6" &
          tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a")               ~ "Low", 
        psa <= 20 &
          gleason %in% c("<=6", "7") &
          tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a", "T2b", "T2c") ~ "Intermediate", 
        TRUE                                                                   ~ NA_character_
    )) %>% 
    # dont have PSAD values -----------------------------
    # and dont have number of cores with >50% cancer ----
    mutate(AUA = case_when( 
      psa >= 20 |
        isup %in% 4:5 |
        tstage %in% c("T3", "T3a", "T3b", "T3c", "T4a", "T4b", "T4", "T4c") ~ "High",
      (psa >= 10 & psa < 20) |
        isup %in% 2:3 |
        tstage %in% c("T2b", "T2c")                                         ~ "Intermediate",
      psa < 10 &
        isup == 1 &
        tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a") &
        percent_pos_cores < 34                                              ~ "Very Low",
      psa < 10 &
        isup == 1 &
        tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a")              ~ "Low",
      TRUE                                                                  ~ NA_character_
    )) %>% 
    # still need to find reference for this -------------
    # no information of PSAD ----------------------------
    # no information on number of cores with >50% cancer 
    mutate(AUAi = case_when(
        psa >= 20 |
          isup %in% 4:5 |
          tstage %in% c("T3", "T3a", "T3b", "T3c", "T4", "T4a", "T4b", "T4c") ~ "High",
        (isup == 2 & ((psa >= 10 & psa < 20) | tstage %in% c("T2b", "T2c")))  |
          (isup == 3 & psa < 20)                                              ~ "Intermediate Unfavorable",
        (isup == 1 & psa >= 10 & psa < 20) | (isup == 2 & psa < 10)           ~ "Intermediate Favorable",
        psa < 10 & isup == 1 &
          tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a") &
          percent_pos_cores < 34                                              ~ "Very Low",
        psa < 10 & isup == 1 & tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a") ~ "Low",
        TRUE ~ NA_character_
    )) %>%
    # Followed NCCN guidelines (not zelic) : https://www.nccn.org/patients/guidelines/content/PDF/prostate-patient.pdf#page=52
    # do not have info on individual cores gleason grades 
    # dont have PSAD ----------------------------------
    mutate(NCCN = case_when(
        tstage %in% c("T3b", "T3c", "T4", "T4a", "T4b", "T4c") |
          {{gleason_var}} %in% 51:55                          ~ "Very High", 
        psa > 20 | isup %in% 4:5 | tstage %in% c("T3", "T3a") ~ "High", 
        (psa >= 10 & psa <= 20 & isup %in% 2:3) | (psa >= 10 & psa <= 20 & tstage %in% c("T2b", "T2c")) |
          (isup  %in% 2:3 & tstage %in% c("T2b", "T2c")) |
          isup == 3 |
          percent_pos_cores > 50                            ~ "Intermediate Unfavorable",
        ((psa >= 10 & psa <= 20) | isup %in% 2:3 | tstage %in% c("T2b", "T2c")) &
          percent_pos_cores < 50 &
          isup %in% 1:2                                    ~ "Intermediate Favorable",
        psa < 10 & isup == 1 &
          tstage %in% c("T1", "T1a", "T1b" , "T1c") &  
          {{pos_cores_var}} %in% 1:2 ~ "Very Low",
        psa < 10 & isup == 1 &
          tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a") ~ "Low",
        TRUE  ~ NA_character_
    )) %>% 
    # https://www.ncbi.nlm.nih.gov/pubmed/27483464/ 
    mutate(CPG = case_when( 
      (psa > 20 & isup == 4) |
        (psa > 20 & tstage %in% c("T3", "T3a", "T3b", "T3c")) |
        (isup == 4 &tstage %in% c("T3", "T3a", "T3b", "T3c")) |
        isup == 5 |
        tstage %in% c("T4", "T4a", "T4b", "T4c") ~ "Very High",
      psa > 20 | isup == 4 | tstage %in% c("T3", "T3a", "T3b", "T3c") ~ "High",
      ((psa >= 10 & psa <= 20) & isup  == 2 &
         tstage %in% c("T1", "T1a", "T1b", "T1c", "T2", "T2a", "T2b", "T2c")) |
        (isup == 3 &
           tstage %in% c("T1", "T1a", "T1b", "T1c", "T2", "T2a", "T2b", "T2c")) ~ "Intermediate Unfavorable",                                             
      isup == 2 | (psa >= 10 & psa <= 20) &
        tstage %in% c("T1", "T1a", "T1b", "T1c", "T2", "T2a", "T2b", "T2c")    ~ "Intermediate Favorable",
      psa < 10 & isup == 1 &
        tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a", "T2b", "T2c")    ~ "Low",
      TRUE                                                                      ~ NA_character_
    )) %>%
    mutate(capra_score = rowSums(select(.,capra_psa:capra_age), na.rm = TRUE)) 
}

# making noisy data for ML ----------------------------------------------------
# make_structured_noise <- function(data,
#                                   identifier, 
#                                   outcome, 
#                                   outcome_time, 
#                                   numeric_vars){
#   
#   data_ml <- data %>%
#     select({{identifier}}, {{outcome}}, {{outcome_time}}, contains("capra_"),
#            {{numeric_vars}}) %>% 
#     # mulitple versions of all numeric variables (not survival related)
#     mutate_at(c(contains("capra_"), numeric_vars), .funs = list(logged = ~ log(.))) %>% 
#     mutate_at(c(contains("capra_"), numeric_vars), .funs = list(frac_poly_2 = ~ .^(1/2))) %>% 
#     mutate_at(c(contains("capra_"), numeric_vars), .funs = list(frac_poly_3 = ~ .^(1/3))) 
# }

# performance measures of predicting overall survival -------------------------

calulate_c_index <- function(data,
                             split = 0.7,
                             outcome = "os",
                             time_to_outcome = "DX_LASTCONTACT_DEATH_MONTHS",
                             classifiers = c("capra_score", "damico", "nice",
                                             "eau", "GUROC", "AUA", "AUAi", 
                                             "NCCN", "CPG"),
                             covariates = c("AGE")){

  split_data <- data %>% 
    drop_na({{outcome}}, {{time_to_outcome}}, {{classifiers}}) %>% 
    rsample::initial_split(prop = split)
  
  coxph_formula <- function(outcome, classifiers) {
    ## construct the call to coxph()
    rlang::new_formula(
      rlang::parse_expr(paste0(
          "Surv(", time_to_outcome, ", " , outcome, ")")
        ),
      rlang::parse_expr(classifiers)
    )
  }
  
  coxph_formula_adjusted <- function(outcome, classifiers, covariates) {
    ## construct the call to coxph()
    rlang::new_formula(
      rlang::parse_expr(paste0(
        "Surv(", time_to_outcome, ", " , outcome, ")")
      ),
      rlang::parse_expr(paste0(classifiers," + ", paste(covariates, collapse = " + ")))
    )
  }
  
  
  coxph_model <- function(formula, data) {
    eval(rlang::expr(survival::coxph(!!formula, data = data)))
  }
  
  coxph_concordance <- function(coxph_model, data, ...){
    x <- update(coxph_model, data = data)
    y <- concordance(x, ...)
    return(y$concordance)
  }
  
  # coxph_concordance_time <- function(coxph_model, data, times){
  #   x <- update(coxph_model, data = data)
  #   y <- concordance(x, ymin = 0, ymax = times)
  #   return(y$concordance)
  # }
  
  c_data <- tibble(outcome = outcome, classifiers = classifiers) %>%
    # create formulas
    mutate(formula_crude = pmap(., coxph_formula)) %>%
    mutate(formula_adjusted = map2(outcome, classifiers, coxph_formula_adjusted,
                                   covariates = covariates)) %>%
    # eval models 
    mutate(model_crude = map(formula_crude, coxph_model,
                             data = training(split_data))) %>%
    mutate(model_adjusted = map(formula_adjusted, coxph_model,
                                data = training(split_data))) %>%
    # calculate concordance 
    mutate(concord_crude = map(model_crude, coxph_concordance,
                               data = testing(split_data))) %>%
    mutate(concord_adjusted = map(model_adjusted, coxph_concordance,
                               data = testing(split_data))) 
  return(c_data)

 
}
# There are two variables in the SEER data for estimating cause-specific survival probability 
# due to ‘cancer’ or due to ‘other-causes’. The idea is to use these variables independently 
# to estimate survival of specified cause of death (e.g. cancer, non-cancer). The ‘SEER 
# cause-specific death classification’ variable is used to obtain cancer-specific survival 
# probability for a given cohort of cancer patients. While, ‘SEER other cause of death 
# classification’ variable is used to obtain the other-cause survival probability for the 
# same cohort of patients. In the first variable, (SEER cause-specific death classification) 
# deaths attributed to the cancer of interest are treated as events and deaths from other 
# causes are treated as censored observation. The event of interest in the second variable 
# (the ‘SEER other cause of death classification’) is the reverse, i.e., deaths attributed 
# to causes other than cancer are treated as events and deaths from cancer are treated as 
# censored observation. The causes of deaths codes are the same in both variables.

# STAT_REC aka "Vital Status recode" 1 Alive 0 Dead is GENERAL
# When some patients died from other cancer or other causes
# or prostate cancer is not their FIRST cancer

# The imputation on all os increase the number of patient who died by prostate cancer
# CODPUB aka "Cause of Death to SEER site recode" 
# Prostate Recode= 28010 -> We have 155903 death per prostate
seer_imputed[seer_imputed$CODPUB == "28010",] # 155,903 patients
seer[seer$CODPUB == "28010",] # 155,239 patients

# And it attributes values to all patients
seer_imputed[, c("gleason", "CODPUB", "VSRTSADX")]  
seer[, c("gleason", "CODPUB", "VSRTSADX")] 


# Step 1 ------------ Filter for Prostate as PRIMARY site
# C619 prostate as primary site in PRIMSITE -> 1,309,913 patients
ProstatePrimSite <- seer[seer$PRIMSITE == "C619",]

# Step 2 ------------ Evaluate death CAUSED by Prostate cancer
# https://seer.cancer.gov/causespecific/
# VSRTSADX "SEER Cause-Specific Death Classification"
# 0 = Alive or dead of other cause
# 1 = Dead (attributable to this cancer dx)
# 8 = Dead (missing/unknown COD)*
# 9 = N/A not first tumor
ProstatePrimSite <- ProstatePrimSite %>% 
  mutate(death_by_PC = case_when(
    VSRTSADX == "1" ~ 1,
    TRUE ~ 0
  ))
# Will not use
# ODTHCLASS "SEER Other Cause of Death Classification"
# 0 = Alive or dead due to cancer
# 1 = Dead (attributable to causes other than this cancer dx)
# 8 = Dead (missing/unknown COD)*
# 9 = N/A not first tumor

# Here we have the number of death which are certain to be caused by PC as Primary tumor
table(ProstatePrimSite$death_by_PC)
# Here we have the number of all_cause_of_death with PC as Primary tumor
table(ProstatePrimSite$STAT_REC)
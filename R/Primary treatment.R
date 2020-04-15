# colnames(seer)
# seer$SURGSITE
# 
# RX SUMM—SURG PRIM SITE
# NAACCR Item #: 1290
# SEER*Stat Name: RX Summ--Surg Prim Site (1998+) Item Length: 2
# Field Description:
#   Surgery of Primary Site describes a surgical procedure that removes and/or destroys tissue of the primary site performed as part of the initial work-up or first course of therapy.
# The actual coding schemes for individual sites and diagnosis years can be viewed at http://seer.cancer.gov/tools/codingmanuals/historical.html
# General Coding Structure
# Code Description
# 00 
# 10-19
# 20-80 
# 90
# 98
# 99
# 
# None; no surgical procedure of primary site; diagnosed at autopsy only
# Site-specific codes. Tumor destruction; no pathologic specimen or unknown whether there is a pathologic specimen
# Site-specific codes. Resection; pathologic specimen Surgery, NOS. A surgical procedure to the primary site
# was done, but no information on the type of surgical procedure is provided.
# Special codes for hematopoietic, reticuloendothelial, immunoproliferative, myeloproliferative diseases; ill- defined sites; and unknown primaries (See site-specific codes for the sites and histologies), except death certificate only
# Unknown if surgery performed; death certificate only

################################################################################################ SEER ################ 
# To get acces to chemo and radiation data need to ask for 1 access
# To get acces to active sur data need to ask for 1 other access
  
# col_treat <- seer_read_col_positions(fs::path(seer_path, "read.seer.research.and.treatment.nov2018.sas"))
# seer_treat <- seer_read_fwf(paste0(tx,"/.TXT"),
#                    col_positions = col_treat)
# Waiting for the access

################################################################################################ NCDB ################ 
# All patients are prostate cancer as primary site so no need to filter

treatment_ncdb <- ncdb %>% 
  mutate(active_surv = case_when(
    RX_SUMM_TREATMENT_STATUS == 2 ~ "active_surv", # I verified that all active surv are not given a treatment
    RX_SUMM_TREATMENT_STATUS == 1 | # is missing a lot of value compare to the next bc reported for >= 2010
      !is.na(DX_RX_STARTED_DAYS) ~ "treatment_given",
    TRUE ~ NA_character_
  )) %>% 
  #mutate(last_prostate_surg = case_when(
   # !is.na(DX_DEFSURG_STARTED_DAYS) ~ "last_prostate_surg",
    # not recorded before 2003
    #!is.na(DX_SURG_STARTED_DAYS) ~ "other_site",
    # DX_SURG_STARTED_DAYS could be (minus primary site) lymph node and other surgery but not biopsies
    # REASON_FOR_NO_SURGERY == 9 ~ "diag_at_autopsy or died before surgery"
  # )) %>% 
  # mutate(prossurg = case_when(
  #   RX_SUMM_SURG_PRIM_SITE %in% c(10:90) ~ "prossurg"
  # )) %>% 
  mutate(dx_of_first_surg_prostate_only = case_when( 
    # I filtered the dx of all first surgery by when patient had prostate surg first
    REASON_FOR_NO_SURGERY == 0 |
      RX_SUMM_SURG_PRIM_SITE %in% c(10:90) ~ as.numeric(DX_SURG_STARTED_DAYS),
    TRUE ~NA_real_
  )) %>% 
  mutate(died_prior_surgery_planned_recommended = case_when(
    REASON_FOR_NO_SURGERY == 5 ~ "death_prior_surg",
    TRUE ~NA_character_
  )) %>% 
  mutate(dx_of_first_rad_prostate_only = case_when(
    # filtered the dx of all first rad by when patient had prostate rad first
    REASON_FOR_NO_RADIATION == 0 |
      RAD_REGIONAL_RX_MODALITY %in% c(20:98) |
      RX_SUMM_RADIATION %in% c(1:5) ~ as.numeric(DX_RAD_STARTED_DAYS),
    TRUE ~ NA_real_
  )) %>% 
  mutate(died_prior_radiation_planned_recommended = case_when(
    REASON_FOR_NO_RADIATION == 5 |
      RAD_REGIONAL_RX_MODALITY == 99 ~ "death_prior_rad",
    TRUE ~ NA_real_
  )) %>% 
  mutate(diag_at_autopsy = case_when(
    RAD_REGIONAL_DOSE_CGY == 00000 |
      RAD_NUM_TREAT_VOL == 000 ~ "diagnose_at_autopsy",
    TRUE ~ NA_real_
  )) %>% 
  
  )) %>% 
  # We cannot use rx_summ_sungraded_seq to determine what treatment came first bc include all surg
  mutate(time_of_rad = case_when(
    RX_SUMM_SURGRAD_SEQ == 2 ~ "before surgery",
    RX_SUMM_SURGRAD_SEQ == 3 ~ "after surgery",
    RX_SUMM_SURGRAD_SEQ == 4 ~ "before and after",
    RX_SUMM_SURGRAD_SEQ == 5 ~ "during surgery"
  )) %>% 
  

  mutate(rad_site = case_when(
    RAD_TREAT_VOL %in% c(1:98) & # rad as first course treatment
      RAD_TREAT_VOL != 41 ~ "other_site",
    RAD_TREAT_VOL %in% c(1:98) &    
      RAD_TREAT_VOL == 41 ~ "prostate"
  )) %>%
  mutate(norad = case_when(
    REASON_FOR_NO_RADIATION %in% c(5, 9) ~ "diag_at_autopsy or died before surgery", # WRONG
    RAD_TREAT_VOL == 00 ~ "raddiag_at_autopsy"
  )) %>% 
  
  
  mutate(systemic)
  
  
  mutate(systemic_treatment = case_when(
    !is.na(DX_SYSTEMIC_STARTED_DAYS) ~ "systemic treatment administered"
  )) %>% 
  mutate(primary_treat = case_when(
    DX_SYSTEMIC_STARTED_DAYS < DX_SURG_STARTED_DAYS & # all surgery
      DX_SYSTEMIC_STARTED_DAYS < DX_RAD_STARTED_DAYS # all radiation
    ~ "systemic_treatment",
    DX_SURG_STARTED_DAYS < DX_SYSTEMIC_STARTED_DAYS &
      DX_SURG_STARTED_DAYS < DX_RAD_STARTED_DAYS
    ~ "surgery_prim_site",
    DX_RAD_STARTED_DAYS < DX_SURG_STARTED_DAYS &
      DX_RAD_STARTED_DAYS < DX_SYSTEMIC_STARTED_DAYS
    ~ "radiation",
    RX_SUMM_SURGRAD_SEQ == 5 ~ "radiation during surgery"
    # RX_SUMM_SURGRAD_SEQ == 2 ~ " rad before surgery",
  )) %>% 
  mutate(type_systemic_treatment1 = case_when(
    RX_SUMM_CHEMO %in% c(01, 02, 03) ~ "chemo"))%>% 
  mutate(type_systemic_treatment2 = case_when(
    RX_SUMM_HORMONE == 01 ~ "Hormone therapy"))%>% 
  mutate(type_systemic_treatment3 = case_when(
    RX_SUMM_IMMUNOTHERAPY == 01 ~ "Immunotherapy"))%>% 
  mutate(type_systemic_treatment4 = case_when(
    RX_SUMM_TRNSPLNT_ENDO %in% c(10, 11,12, 20) ~ "Hematologic transplant"))%>% 
  mutate(type_systemic_treatment5 = case_when(
    RX_SUMM_OTHER %in% c(1:3) ~ "Other recommended treatments"
  )) %>% 
  mutate(Palliative_care = case_when( # Can do a mutate_at
    # This include palliative surg, rad, systemic and the 3 together 
    # but not the pain management (can be added if we wnat by coding 1:6 instead of 1:5)
    PALLIATIVE_CARE %in% c(1:5) ~ "Received_palliative" 
  ))



# Check active
a <- treatment_ncdb[treatment_ncdb$treatment_given1 == "treatment given",]
table(treatment_ncdb$active_surv == "active")
tail(table(ncdb$DX_RX_STARTED_DAYS))
which(ncdb$DX_RX_STARTED_DAYS == "444")

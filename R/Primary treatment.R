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
    TRUE ~ NA_real_
  )) %>% 
  mutate(died_prior_surgery_planned_recommended = case_when(
    REASON_FOR_NO_SURGERY == 5 ~ "death_prior_surg",
    TRUE ~ NA_character_
  )) %>% 
  mutate(dx_of_first_rad_prostate_only = case_when(
    # filtered the dx of all first rad by when patient had prostate rad first
    REASON_FOR_NO_RADIATION == 0 |
      RAD_REGIONAL_RX_MODALITY %in% c(20:98) |
      RX_SUMM_RADIATION %in% c(1:5) |
      RAD_TREAT_VOL == 41 ~ as.numeric(DX_RAD_STARTED_DAYS),
    TRUE ~ NA_real_
  )) %>% 
  mutate(died_prior_radiation_planned_recommended = case_when(
    REASON_FOR_NO_RADIATION == 5 |
      RAD_REGIONAL_RX_MODALITY == 99 ~ "death_prior_rad",
    TRUE ~ NA_character_
  )) %>% 
  # mutate(diag_at_autopsy = case_when( # Cannot get that
  # )) %>% 
  mutate(rad_site = case_when(
    # rad site for the first course treatment # will probably not use as is
    RAD_TREAT_VOL %in% c(1:98) & 
      RAD_TREAT_VOL != 41 ~ "other_site",
    RAD_TREAT_VOL == 41 ~ "prostate",
    TRUE ~ NA_character_
  )) %>% 
  mutate(systemic_treatment = case_when( # Will probably not use as is
    !is.na(DX_SYSTEMIC_STARTED_DAYS) ~ "systemic_treatment_administered",
    TRUE ~ NA_character_
  )) %>% 
  mutate(dx_of_first_chemo_only = case_when( 
    # I filtered to have the dx of first chemo
    RX_SUMM_CHEMO %in% c(1:3) ~ as.numeric(DX_CHEMO_STARTED_DAYS),
    TRUE ~NA_real_
    )) %>%
  mutate(dx_of_first_homoneT_only = case_when( 
    # I filtered to have the dx of first hormone therapy
    RX_SUMM_HORMONE == 1 ~ as.numeric(DX_HORMONE_STARTED_DAYS),
    TRUE ~NA_real_
  )) %>%
  mutate(dx_of_first_immunoT_only = case_when( 
    # I filtered to have the dx of first immunotherapy
    RX_SUMM_IMMUNOTHERAPY == 1 ~ as.numeric(DX_IMMUNO_STARTED_DAYS),
    TRUE ~NA_real_
  )) %>%
  mutate(rad_vs_surg_vs_sys_as_primary_treatment = case_when(
    # We cannot use RX_SUMM_SURGRAD_SEQ or RX_SUMM_SYSTEMIC_SUR_SEQ 
    # to determine what treatment came first bc include all surg 
    # not only prostate as first surgery
    # So need to compare what 1st course happened first
      (dx_of_first_chemo_only &
        dx_of_first_homoneT_only &
        dx_of_first_immunoT_only) < 
        (dx_of_first_surg_prostate_only & 
        dx_of_first_rad_prostate_only) ~ "systemic_as_first",
      dx_of_first_surg_prostate_only < 
        (dx_of_first_chemo_only &
           dx_of_first_homoneT_only &
           dx_of_first_immunoT_only) &
        dx_of_first_surg_prostate_only < dx_of_first_rad_prostate_only ~ "surgery_prim_site_as_first",
      dx_of_first_rad_prostate_only < dx_of_first_surg_prostate_only &
        dx_of_first_rad_prostate_only < 
        (dx_of_first_chemo_only &
           dx_of_first_homoneT_only &
           dx_of_first_immunoT_only) ~ "radiation_as_first",
      ((dx_of_first_chemo_only &
         dx_of_first_homoneT_only &
         dx_of_first_immunoT_only) == 
        dx_of_first_surg_prostate_only | 
           dx_of_first_rad_prostate_only) |
        (dx_of_first_surg_prostate_only == 
           dx_of_first_rad_prostate_only) ~ "same_time", # I can separate them if needed
      TRUE ~ NA_character_
  )) %>% 
  mutate(type_systemic_received1 = case_when(
    RX_SUMM_CHEMO %in% c(01, 02, 03) ~ "chemotherapy",
    TRUE ~ NA_character_
    )) %>%
  mutate(type_systemic_received2 = case_when(
    RX_SUMM_HORMONE == 01 ~ "Hormone_therapy",
    TRUE ~ NA_character_
    )) %>%
  mutate(type_systemic_received3 = case_when(
    RX_SUMM_IMMUNOTHERAPY == 01 ~ "Immunotherapy",
    TRUE ~ NA_character_
    )) %>%
  mutate(type_systemic_received4 = case_when(
    RX_SUMM_TRNSPLNT_ENDO %in% c(10, 11, 12, 20) ~ "Hematologic_transplant",
    TRUE ~ NA_character_
    )) %>%
  mutate(type_systemic_received5 = case_when(
    RX_SUMM_OTHER %in% c(1:3) ~ "Other_recommended_treatments",
    TRUE ~ NA_character_
    )) %>% 
  mutate(died_prior_systemic__planned_recommended = case_when(
    RX_SUMM_CHEMO == 85 &
      RX_SUMM_HORMONE == 85 &
      RX_SUMM_IMMUNOTHERAPY == 85 &
      RX_SUMM_TRNSPLNT_ENDO == 85 ~ "death_prior_systemic", # Can separate them if needed
    TRUE ~ NA_character_
    )) %>% 
  mutate(palliative_care = case_when( # Can do a mutate_at
    # This include palliative surg, rad, systemic or the 3 together 
    # but not pain management (can be added if we wnat by coding 1:6 instead of 1:5)
    PALLIATIVE_CARE %in% c(1:5) ~ "Received_palliative",
    TRUE ~ NA_character_
  )) %>% 
  mutate(palliative_care_only = case_when(
    palliative_care == "Received_palliative" &
      is.na(dx_of_first_surg_prostate_only) &
      is.na(dx_of_first_rad_prostate_only) &
      is.na(dx_of_first_chemo_only) &
      is.na(dx_of_first_homoneT_only) &
      is.na(dx_of_first_immunoT_only) &
      is.na(DX_RX_STARTED_DAYS) ~ "Received_palliative_only",
    TRUE ~ NA_character_
  ))

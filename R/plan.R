plan <- drake_plan(
  
  # import raw SEER and NCDB data -----------------------------------
  seer_raw = seer_import(fs::path("","Volumes","Lab_Gerke","SEER",
                                  "Nov2018", "SEER_1975_2016_TEXTDATA",
                                  "incidence")),
  
  ncdb_raw = ncdb_import(fs::path("","Volumes","Lab_Gerke","prostateWorkGroup",
                                  "riskClassification", "data", "NCDB")),
  
  # cleaning the data files and creating risk classifications -------
  seer = seer_recoding(seer_raw),
  ncdb = ncdb_recoding(ncdb_raw), 
  
  # imputation of risk score variables ------------------------------
  seer_imputed = impute_data(data = seer,
                             method = "mean"),
  ncdb_imputed = impute_data(data = ncdb,
                             method = "mean",
                             id = "PUF_CASE_ID",
                             varlist = c("psa", "tstage", "gleason", "isup",
                                         "percent_pos_cores",
                                         "CS_SITESPECIFIC_FACTOR_7",
                                         "CS_SITESPECIFIC_FACTOR_12",
                                         "capra_psa","capra_gleason",
                                         "capra_tstage", "capra_per_pos", "capra_age")),
  
  # calculating risk scores -----------------------------------------
  risk_seer = risk_scores(seer, gleason_var = "CS7SITE",
                          pos_cores_var = "CS12SITE"),
  risk_seer_imputed = risk_scores(seer_imputed,
                                  gleason_var = "CS7SITE",
                                  pos_cores_var = "CS12SITE"),
  risk_ncdb = risk_scores(ncdb, 
                          gleason_var = "CS_SITESPECIFIC_FACTOR_7",
                          pos_cores_var = "CS_SITESPECIFIC_FACTOR_12"),
  risk_ncdb_imputed = risk_scores(ncdb_imputed, 
                                  gleason_var = "CS_SITESPECIFIC_FACTOR_7",
                                  pos_cores_var = "CS_SITESPECIFIC_FACTOR_12")
  
  
  # creating dataset for machine learning proxy ---------------------
  # seer_ml = make_structured_noise(data = seer,
  #                                 identifier = , 
  #                                 outcome = , 
  #                                 outcome_time = ,
  #                                 numeric_vars = c("psa","isup","percent_pos_cores")),
  # ncdb_ml = make_structured_noise(ncdb,
  #                                 identifier = , 
  #                                 outcome = , 
  #                                 outcome_time = ,
  #                                 numeric_vars = c("psa","isup","percent_pos_cores"))
)

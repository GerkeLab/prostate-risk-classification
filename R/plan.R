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
  
  # calculating risk scores -----------------------------------------
  risk_seer = risk_scores(seer, gleason_var = "CS7SITE",
                          pos_cores_var = "CS12SITE"),

  risk_ncdb = risk_scores(ncdb, 
                          gleason_var = "CS_SITESPECIFIC_FACTOR_7",
                          pos_cores_var = "CS_SITESPECIFIC_FACTOR_12"),
  
  # calculate crude c index for risk scores overall ----------------------
  
  seer_c_index = calulate_c_index(data = risk_seer,
                                  split = 0.7,
                                  outcome = "os",
                                  time_to_outcome = "SRV_TIME_MON",
                                  classifiers = c("capra_score", "damico", "nice",
                                                  "eau", "GUROC", "AUA", "AUAi", 
                                                  "NCCN", "CPG"),
                                  covariates = c("AGE_DX", "YEAR_DX", 
                                                 "marital_status")),
  
  ncdb_c_index = calulate_c_index(data = risk_ncdb,
                                  split = 0.7,
                                  outcome = "os",
                                  time_to_outcome = "DX_LASTCONTACT_DEATH_MONTHS",
                                  classifiers = c("capra_score", "damico", "nice",
                                                  "eau", "GUROC", "AUA", "AUAi", 
                                                  "NCCN", "CPG"),
                                  covariates = c("AGE", "YEAR_OF_DIAGNOSIS",
                                                 "CDCC_TOTAL_BEST","NO_HSD_QUAR_12")),
  
  
  # output manuscript -----------------------------------------------
  
  final_report = rmarkdown::render(
    knitr_in("manuscript/manuscript.Rmd"),
    output_file = file_out("manuscript.html")
  )

)

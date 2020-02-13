plan <- drake_plan(
  
  # import raw SEER and NCDB data -----------------------------------
  seer_raw = seer_import(fs::path("","Volumes","Lab_Gerke","SEER",
                                  "Nov2018", "SEER_1975_2016_TEXTDATA",
                                  "incidence")),
  
  ncdb_raw = ncdb_import(fs::path("","Volumes","Lab_Gerke","prostateWorkGroup",
                                  "riskClassification", "data", "NCDB")),
  
  # cleaning the data files and creating risk classifications -------
  seer = seer_recoding(seer_raw),
  ncdb = ncdb_recoding(ncdb_raw)#, 
  
  # creating dataset for machine learning proxy ---------------------
#   seer_ml = make_structured_noise(data = seer,
#                                   identifier = , 
#                                   outcome = , 
#                                   outcome_time = ,
#                                   numeric_vars = c("psa","isup","percent_pos_cores")),
#   ncdb_ml = make_structured_noise(ncdb,
#                                   identifier = , 
#                                   outcome = , 
#                                   outcome_time = ,
#                                   numeric_vars = c("psa","isup","percent_pos_cores"))
)

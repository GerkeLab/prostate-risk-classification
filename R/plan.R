plan <- drake_plan(
  
  # import raw SEER and NCDB data -----------------------------------
  seer_raw = seer_import(fs::path("","Volumes","Lab_Gerke","SEER",
                                  "Nov2018", "SEER_1975_2016_TEXTDATA",
                                  "incidence")),
  
  ncdb_raw = ncdb_import(fs::path("","Volumes","data","DATASET"))
  
  # cleaning the data files and creating risk classifications -------
  
)

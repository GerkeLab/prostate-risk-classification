
# seer <- mutate(damico = case_when( ---------Will change seer later but create a risk_risk for the time


risk_risk <- seer %>%
  mutate(damico = case_when(
  tstage %in% c("T2c", "T3", "T4", "T3a", "T3b", "T3c", "T4a", "T4b") | 
    PSA > 20 |
    gleason %in% c("8", "9-10")                                   ~ "High",
  tstage %in% c("T2b") |
    (PSA > 10 & PSA <= 20) |
    gleason == "7"                                                ~ "Intermediate",
  tstage %in% c("T1c", "T2a") &##################################### what about T2 and remove "T1", "T1a", "T1b", 
    PSA <= 10 & 
    gleason == "<=6"                                              ~ "Low",
  TRUE                                                            ~ NA_character_
)) %>% 
  mutate(
    EAU = case_when(
      PSA > 20 |
        gleason %in% c("8", "9-10") |
        tstage %in% c("T2c", "T3", "T4", "T3a", "T3b", "T3c", "T4a", "T4b")          ~ "High",
      (PSA >= 10 & PSA <= 20) |
        gleason == "7" |
        tstage %in% c("T2b")                                                         ~ "Intermediate",
      PSA <= 10 &
        gleason == "<=6" &
        tstage %in% c("T1c", "T2a")                                                  ~ "Low",
      ##################################### what about T2 and remove "T1", "T1a", "T1b", 
      TRUE                                                                           ~ NA_character_ 
  )) %>%
  mutate(NICE = EAU
  ) %>%
  mutate(
    GUROC = case_when(
      ########################################### check guroc what is ct1 ct2 and what means not otehrwise low risk
      PSA > 20 |
        gleason %in% c("8", "9-10") |
        tstage %in% c("T3a", "T3b", "T3c", "T4a", "T4b")                 ~ "High",
      (PSA > 10 & PSA <= 20) &
        gleason == "7" &
        tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a")           ~ "Intermediate", ######### I add T2
      PSA <= 10 &
        gleason == "<=6" &
        tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a")           ~ "Low",           ######### I add T2
      TRUE                                                               ~ NA_character_
  )) %>% 
  mutate(AUA = case_when(
    (PSA >= 20) |
      isup %in% c("4", "5") |
      tstage %in% c("T3", "T3a", "T3b", "T3c", "T4a", "T4b")           ~ "High",
    (PSA >= 10 & PSA < 20) |
      isup %in% c("2", "3") |
      tstage %in% c("T2b", "T2c")                                      ~ "Intermediate",
    PSA < 10 &
      isup == "1" &
      tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a")           ~ "Low",
    PSA < 10 &
      isup == "1" &
      tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a") &
      pos_core_percent < "34" #&
      #PSAD < "0.15"  -------------------------------------------------------------------- Problem                                                  
    ~ "Very low Low",
    TRUE                                                               ~ NA_character_
  )) %>% 
  mutate(
    AUAi = case_when(
      (PSA >= 20) |
        isup %in% c("4", "5") |
        tstage %in% c("T3", "T3a", "T3b", "T3c", "T4a", "T4b")           ~ "High",
      (isup == "2" &
         ((PSA == 10 & PSA < 20) |
            tstage %in% c("T2b", "T2c")
         )) |
        (isup == "3" &
           PSA < 20)                                                     ~ "Intermediate unfavorable",# Check if they mean all <20 or between 10 and 20
      (isup == "1" &
         (PSA == 10 & PSA < 20)) |
        (isup == "2" &
           PSA < 10)                                                     ~ "Intermediate favorable",
      PSA < 10 &
        isup == "1" &
        tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a")           ~ "Low",
      PSA < 10 &
        isup == "1" &
        tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a") &
        pos_core_percent < "34" #&
        #PSAD < "0.15"  -------------------------------------------------------------------- Problem 
      ~ "Very low",
      TRUE                                                               ~ NA_character_
  )) %>% 
  mutate(
    NCCN = case_when(
      # GG1 = "5" | ------------------------------------------------------------------ DO NOT HAVE YET
      tstage %in% c("T3b", "T3c", "T4a", "T4b")                        ~ "Very High",
      PSA > 20 |
        isup %in% c("4", "5") |
        tstage == "T3a"                                                   ~ "High", ############## and T3?
      (PSA >= 10 & PSA <= 20) |
        isup  %in% c("2", "3") |
        tstage %in% c("T2b", "T2c")                                      ~ "Intermediate unfavorable",
      (PSA >= 10 & PSA <= 20) |
        isup == "2" |
        (tstage %in% c("T2b", "T2c")) &
        pos_core_percent < "50"                                            ~ "Intermediate favorable",
      PSA < 10 &
        isup == "1" &
        tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a")           ~ "Low",
      PSA < 10 &
        isup == "1" &
        tstage %in% c("T1", "T1a", "T1b" , "T1c") &                             #################### only ct1c...
        CS12SITE < "3" #&
        #PSAD < "0.15" ------------------------------------------------------ Problem                                                    
      ~ "Very low",
      TRUE                                                               ~ NA_character_
    )) %>% 
  mutate(
    CPG = case_when(
      (PSA > "20" &
          isup == "4" &
          tstage %in% c("T3", "T3a", "T3b", "T3c", "T4a", "T4b")
      ) |
        isup == "5" |
        tstage %in% c("T4", "T4a", "T4b")                                ~ "Very High",
      PSA > 20 |
        isup == "4" |
        tstage %in% c("T3", "T3a", "T3b", "T3c")                         ~ "High",############### I add a b c
      ((PSA >= 10 & PSA <= 20) &
         isup  == "2" &
         tstage %in% c("T1", "T1a", "T1b", "T1c", "T2", "T2a", "T2b", "T2c")
      ) |
        (isup == "3" &
            tstage %in% c("T1", "T1a", "T1b", "T1c", "T2", "T2a", "T2b", "T2c")
        )
      ~ "Intermediate unfavorable",                                              ################## I add more
      (PSA >= 10 & PSA <= 20) |
        (isup == "2" &
            tstage %in% c("T1", "T1a", "T1b", "T1c", "T2", "T2a", "T2b", "T2c")
        )
      ~ "Intermediate favorable",
      PSA < 10 &
        isup == "1" &
        tstage %in% c("T1", "T1a", "T1b" , "T1c", "T2", "T2a", "T2b", "T2c")
      ~ "Low",
      TRUE                                                               ~ NA_character_
    )) %>% ##################  WORKS UNTIL HERE!!!!!!!!!
  mutate(
    capra = case_when(
      sum_point_capra <= "2"                           ~ "Low",
      (sum_point_capra >= "3" & sum_point_capra <= "5")                                             ~ "Intermediate ",
      sum_point_capra >= '6'           ~ "High",
      TRUE                                                               ~ NA_character_
    ))
  #  ))% >% 
  # mutate(
  #   MSKCC = case_when(
  #     
  #   ))

    
  
  
  
  
  
  
    
    
    



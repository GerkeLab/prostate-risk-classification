


# progression = 1
# none = 0
# 
# gleason  increase = 6-10 = CS10-CS8

a <- risk_ncdb_imputed
b <- risk_seer_imputed
# b$CODPUB "Cause of Death to SEER site recode" #Prostate=	ICD-9=185,	ICD-10=C61,	Recode=28010
# -> We have 155903 death per prostate
c <- b[b$CODPUB == "28010",]
table(b$CS8SITE)
class(b$CS10SITE)
table(b$gleason)
colnames(b)
b <- b %>%
  mutate_at(c("CS8SITE", "CS10SITE"), 
            ~ case_when(
              . > 10 ~ NA_real_,
              TRUE ~ as.numeric(.)
            )) %>% 
  mutate(gleaincr = (CS10SITE-CS8SITE)) %>% 
  mutate(death_by_prostate = case_when(
    # NO NOT take STAT_REC "Vital Status recode" 1 Alive 0 Dead is GENERAL
    # 
    # CODPUB "Cause of Death to SEER site recode" #Prostate=	ICD-9=185,	ICD-10=C61,	Recode=28010
    # -> We have 155903 death per prostate
    CODPUB == "28010" &
      # VSRTSADX "SEER Cause-Specific Death Classification"
      # 0 = Alive or dead of other cause
      # 1 = Dead (attributable to this cancer dx)
      # 8 = Dead (missing/unknown COD)*
      # 9 = N/A not first tumor
      VSRTSADX == "1" ~ 1, # Yes
    CODPUB != "28010" |
      VSRTSADX %in% c("0", "8", "9") ~ 0 # No
  )) %>% 
  mutate_at(c("damico", "nice", "capra_score"),
            .funs = list(num = ~ case_when(
              . == "Low"                                                        ~ 1,
              . == "Intermediate"                                               ~ 2,
              . == "High"                                                       ~ 3,
              TRUE                                                              ~ NA_real_
            )))
  
class(b$damico_num)
#cor with risk compare to death or increase gleason




# Test normality
qqnorm(b$gleaincr)
qqline(b$gleaincr)

#par(mfrow = c(2,2)) tot see a 2 by 2 plot
pairs(b[, c("capra_score_num", "nice_num", "damico_num", "gleaincr")], na.omit = TRUE)

library(lattice)
bwplot(b$damico_num,b$gleaincr,b)

xyplot(b$damico_num~b$nice_num| b$gleaincr, data=b)

xyplot(callsVSarray50$BIRC5[callsVSarray50$Call2 == "Normal"]~
         callsVSarray50$CDC20[callsVSarray50$Call2 == "Basal"], data=callsVSarray50)

library(psych)
pairs.panels(callsVSarray50$Call2 == "LumB" [6:6])
pairs.panels(b$damico_num [6:6])

cor(callsVSarray50$ANLN[callsVSarray50$Call2 == "LumB"])
cor((b[, c("capra_score_num", "nice_num", "damico_num", "gleaincr")])
# save it as a matrix
# mat <- 
corrplot(matrix)
hc <- callsVSarray50 %>%
  callsVSarray50$Call2 %>%
  hclust


# PART 2
##################################
# import library
##################################
#install.packages("corrplot")

library(dplyr)
library(corrplot)

####################################################
# SAVE_Array50 plot
####################################################
str(SAVE_Array50)
sapply(SAVE_Array50, class)
head(SAVE_Array50)
Array50[,497:ncol(Array50)-1]

Ccor_Array50 <- cor(SAVE_Array50[,350:ncol(SAVE_Array50)])
cor_Array50 <- cor(SAVE_Array50[,2:ncol(SAVE_Array50)])
c1 <- cor(x = SAVE_Array50[,2:250], y = SAVE_Array50[,250:ncol(SAVE_Array50)])
c2 <- cor(x = SAVE_Array50[6:9,480:ncol(SAVE_Array50)], y= SAVE_Array50[2:5,480:ncol(SAVE_Array50)])

corrplot(Ccor_Array50)
corrplot(cor_Array50)
corrplot(c1)
corrplot(c2)

##################################




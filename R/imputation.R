colnames(seer)
library(mice)

# missing values in risk
seer_risk_missing <- seer[, c("PUBCSNUM", "capra_score", "damico", "nice", "eau", "GUROC", "AUA",
                              "AUAi", "NCCN", "CPG")]
md.pattern(seer_risk_missing)
# 1 means no missing values. 0 means it is a missing values

seer_missing <- seer[, c("psa", "gleason", "tstage", "isup", "percent_pos_cores", "capra_psa", "capra_gleasan",
                         "capra_tstage", "capra_per_pos", "capra_age")]
output <- md.pattern(seer_missing)
output

seer_m1 <- seer[, c("tstage", "isup", "capra_tstage")]
# imputed_seer_m1 <- mice(seer_m1, m=5, maxit = 50, method = 'pmm', seed = 500)

# install.packages("missForest")
library(missForest)
seer.imp <- missForest(seer_m1 )


         
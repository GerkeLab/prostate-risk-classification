# Data manipulation

plot_seer <- risk_seer %>%
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
  mutate(capra = case_when(
            capra_score %in% c(0:2) ~ "Low",
            capra_score %in% c(3:5) ~ "Intermediate",
            capra_score %in% c(6:10) ~ "High",
            TRUE ~ NA_character_
  )) %>%
  mutate_at(c("damico", "nice", "capra", "eau", "GUROC", "AUA", "AUAi", "NCCN", "CPG"),
    .funs = list(
      num = ~ case_when(. %in% c("Low", "Very Low")                                       ~ 1,
                        . %in% c("Intermediate", 
                               "Intermediate Favorable", 
                               "Intermediate Unfavorable")                                ~ 2,
                        . %in% c("High", "Very High")                                     ~ 3,
                        TRUE                                                              ~ NA_real_
  )))
  
  




# Test normality
qqnorm(b$gleaincr)
qqline(b$gleaincr)

pairs(b[, c("capra_score_num", "nice_num", "damico_num", "gleaincr")], na.omit = TRUE)

library(lattice)
bwplot(b$damico,b$gleaincr,b)

xyplot(b$damico_num~b$nice_num| b$gleaincr, data=b)


library(psych)

pairs.panels(b$damico [6:6])


# PART 2
##################################
# import library
##################################

library(corrplot)
library(ggcorrplot)


mat <- cor(plot_seer[, c("damico_num", "nice_num", "capra_score", "eau_num", "GUROC_num", 
                         "AUA_num", "AUAi_num", "NCCN_num", "CPG_num", "os", "gleaincr")], 
           use = "pairwise.complete.obs")
mat1 <- cor_pmat(d)
corrplot(mat)
corrplot.mixed(mat)

ggcorrplot(mat, hc.order = TRUE, method = "circle", # outline.col = "darkblue", # the outline of the circle or sqare
           hc.method = "complete",
           type = "lower",
           lab = TRUE, 
           title = "Correlation between score risk",
           show.legend = TRUE, legend.title = "Correlation", show.diag = FALSE,
           colors = viridis::inferno(n=3),
           lab_col = "darkblue",
           lab_size = 3,
           # p.mat = NULL,
           # sig.level = 0.05,
           # insig = c("pch", "blank"), 
           # pch = 4, pch.col = "black", 
           # pch.cex = 10, 
           tl.cex = 10, tl.col = "red", tl.srt = 270,
           digits = 1
           )

# Compare risk to os
#cor with risk compare to death or increase gleason


library(psych)
pairs.panels(df[c("os", "damico_num")])

df <- plot_seer[c(1:500), c("damico_num", "nice_num", "capra_score", "eau_num", "GUROC_num", 
                            "AUA_num", "AUAi_num", "NCCN_num", "CPG_num", "os", "gleaincr")]

# cor.prob <- function(X, dfr = nrow(X) - 2) {
#   R <- cor(X, use = "pairwise.complete.obs")
#   above <- row(R) < col(R)
#   r2 <- R[above]^2
#   Fstat <- r2 * dfr/(1 - r2)
#   R[above] <- 1 - pf(Fstat, 1 , dfr)
#   R[row(R) == col(R)] <- NA
#   R
# }
# flatten_square_mat <- function(m) {
#   if((class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a sqare matrix")
#   if(!identical(rownames(m), colnames(m))) stop("Row and col names must be equal")
#   ut <- upper.tri(m)
#   data.frame(i= rownames(m)[row(m)[ut]],
#              j = rownames(m)[col(m)[ut]],
#              cor=t(m)[ut],
#              p=m[ut])
# }
# cor_matrix_square <- flatten_square_mat(cor.prob(df))
# cor_list <- cor_matrix_square[order(-abs(cor_matrix_square$cor)),]
# selectedSub <- subset(cor_list, (abs(cor) > 0.2 & j == "os"))
# str(selectedSub)
# cor_list$i <- as.character(cor_list$i)

pairs.panels(df)







cor(b$capra_score == 3, b$damico == "High", use = "pairwise.complete.obs")
cor(b$nice == "High", b$damico == "High", use = "pairwise.complete.obs")

cor((b[, c("capra_score_num", "nice_num", "damico_num", "gleaincr")]))



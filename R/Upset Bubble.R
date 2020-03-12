####################################### UpSet ####################################### 
library(UpSetR)
################################################################################# I ### seer_risk
#count the difference
DAM <- which(!is.na(seer_risk$damico_num))
CAP <- which(!is.na(seer_risk$capra_score_num))
NIC <- which(!is.na(seer_risk$nice_num))
NC <- which(seer_risk$nice_num == seer_risk$capra_score_num)
CD <- which(seer_risk$capra_score_num == seer_risk$damico_num)
ND <- which(seer_risk$damico_num == seer_risk$nice_num)
aF <- seer_risk[ND,]
NDC <- which(aF$damico_num == aF$capra_score_num)

input <- c(
  damico = NROW(DAM),
  capra = NROW(CAP),
  nice = NROW(NIC),
  "nice&damico" = NROW(ND),
  "nice&capra" = NROW(NC),
  "capra&damico" = NROW(CD),
  "nice&damico&capra" = NROW(NDC)
)
upset(fromExpression(input), 
      nintersects = NA, 
      nsets = 3, 
      keep.order = T,
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 0,
      #order.by = "freq", 
      #decreasing = T, 
      text.scale = 1.5, 
      point.size = 3, 
      line.size = 1
)


# With Jordan's code
list_global <- list(DAM_low = c(unique(seer_risk[seer_risk$damico_num == 1,]$PUBCSNUM)),
                 NIC_low = c(unique(seer_risk[seer_risk$nice_num == 1,]$PUBCSNUM)),
                 CAP_low = c(unique(seer_risk[seer_risk$capra_score_num == 1,]$PUBCSNUM)),
                 DAM_int = c(unique(seer_risk[seer_risk$damico_num == 2,]$PUBCSNUM)),
                 NIC_int = c(unique(seer_risk[seer_risk$nice_num == 2,]$PUBCSNUM)),
                 CAP_int = c(unique(seer_risk[seer_risk$capra_score_num == 2,]$PUBCSNUM)),
                 DAM_high = c(unique(seer_risk[seer_risk$damico_num == 3,]$PUBCSNUM)),
                 NIC_high = c(unique(seer_risk[seer_risk$nice_num == 3,]$PUBCSNUM)),
                 CAP_high = c(unique(seer_risk[seer_risk$capra_score_num == 3,]$PUBCSNUM)))
upset(fromList(list_global),
      nsets = 9,
      order.by = "freq",
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 0, 
      text.scale = 1.3 
      #sets.bar.color = c("purple","sienna1","deepskyblue","yellowgreen","violet"),
)

list_low <- list(DAM_low = c(unique(seer_risk[seer_risk$damico_num == 1,]$PUBCSNUM)),
                 NIC_low = c(unique(seer_risk[seer_risk$nice_num == 1,]$PUBCSNUM)),
                 CAP_low = c(unique(seer_risk[seer_risk$capra_score_num == 1,]$PUBCSNUM)))
upset(fromList(list_low),
      nintersects = NA,
      nsets = 3,
      order.by = "freq",
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 0, 
      text.scale = 1.3 
      #sets.bar.color = c("purple","sienna1","deepskyblue","yellowgreen","violet"),
)
list_intermediate <- list(DAM_int = c(unique(seer_risk[seer_risk$damico_num == 2,]$PUBCSNUM)),
                          NIC_int = c(unique(seer_risk[seer_risk$nice_num == 2,]$PUBCSNUM)),
                          CAP_int = c(unique(seer_risk[seer_risk$capra_score_num == 2,]$PUBCSNUM)))
upset(fromList(list_intermediate),
      nintersects = NA,
      nsets = 3,
      order.by = "freq",
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 0 , 
      text.scale = 1.3
      #sets.bar.color = c("purple","sienna1","deepskyblue","yellowgreen","violet"),
)
                    
list_high <- list(DAM_high = c(unique(seer_risk[seer_risk$damico_num == 3,]$PUBCSNUM)),
                  NIC_high = c(unique(seer_risk[seer_risk$nice_num == 3,]$PUBCSNUM)),
                  CAP_high = c(unique(seer_risk[seer_risk$capra_score_num == 3,]$PUBCSNUM)))
upset(fromList(list_high),
      nintersects = NA,
      nsets = 3,
      order.by = "freq",
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 0, 
      text.scale = 1.3
      #sets.bar.color = c("purple","sienna1","deepskyblue","yellowgreen","violet"),
)

################################################################################ II ### NCDB -- Will continue later
# #count the difference
# DAM <- which(!is.na(ncdb$damico_num))
# CAP <- which(!is.na(ncdb$capra_score_num))
# NIC <- which(!is.na(ncdb$nice_num))
# 
# NC <- which(ncdb$nice_num == ncdb$capra_score_num)
# CD <- which(ncdb$capra_score_num == ncdb$damico_num)
# 
# ND <- which(ncdb$damico_num == ncdb$nice_num)
# aF <- ncdb[ND,]
# #aF[159:161]
# 
# NDC <- which(aF$damico_num == aF$capra_score_num)
# 
# input <- c(
#   damico = NROW(DAM),
#   capra = NROW(CAP),
#   nice = NROW(NIC),
#   "nice&damico" = NROW(ND),
#   "nice&capra" = NROW(NC),
#   "capra&damico" = NROW(CD),
#   "nice&damico&capra" = NROW(NDC)
# )
# upset(fromExpression(input), 
#       nintersects = NA, 
#       nsets = 3, 
#       keep.order = T,
#       mainbar.y.label = "Risk Intersections",
#       sets.x.label = "Patients Per Risk",
#       number.angles = 0,
#       #order.by = "freq", 
#       #decreasing = T, 
#       text.scale = 1.5, 
#       point.size = 3, 
#       line.size = 1
# )

rm(NC, CD, ND, aF, NCD)
############################################################################### III ### seer_risk only capra+ DF

positive_capra_score <- seer_risk[which(!is.na(seer_risk$capra_score_num)),]
cap_DAM <- which(!is.na(positive_capra_score$damico_num))
cap_CAP <- which(!is.na(positive_capra_score$capra_score_num))
cap_NIC <- which(!is.na(positive_capra_score$nice_num))
cap_NC <- which(positive_capra_score$nice_num == positive_capra_score$capra_score_num)
cap_CD <- which(positive_capra_score$capra_score_num == positive_capra_score$damico_num)
cap_ND <- which(positive_capra_score$damico_num == positive_capra_score$nice_num)
cap_aF <- positive_capra_score[cap_ND,]
cap_NDC <- which(cap_aF$damico_num == cap_aF$capra_score_num)

cap_input <- c(
  damico = NROW(cap_DAM),
  capra = NROW(cap_CAP),
  nice = NROW(cap_NIC),
  "nice&damico" = NROW(cap_ND),
  "nice&capra" = NROW(cap_NC),
  "capra&damico" = NROW(cap_CD),
  "nice&damico&capra" = NROW(cap_NDC)
)
upset(fromExpression(cap_input), 
      nintersects = NA, 
      nsets = 3, 
      keep.order = T,
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 0,
      #order.by = "freq", 
      #decreasing = T, 
      text.scale = 1.5, 
      point.size = 3, 
      line.size = 1
)

list_global <- list(DAM_low = c(unique(positive_capra_score[positive_capra_score$damico_num == 1,]$PUBCSNUM)),
                    NIC_low = c(unique(positive_capra_score[positive_capra_score$nice_num == 1,]$PUBCSNUM)),
                    CAP_low = c(unique(positive_capra_score[positive_capra_score$capra_score_num == 1,]$PUBCSNUM)),
                    DAM_int = c(unique(positive_capra_score[positive_capra_score$damico_num == 2,]$PUBCSNUM)),
                    NIC_int = c(unique(positive_capra_score[positive_capra_score$nice_num == 2,]$PUBCSNUM)),
                    CAP_int = c(unique(positive_capra_score[positive_capra_score$capra_score_num == 2,]$PUBCSNUM)),
                    DAM_high = c(unique(positive_capra_score[positive_capra_score$damico_num == 3,]$PUBCSNUM)),
                    NIC_high = c(unique(positive_capra_score[positive_capra_score$nice_num == 3,]$PUBCSNUM)),
                    CAP_high = c(unique(positive_capra_score[positive_capra_score$capra_score_num == 3,]$PUBCSNUM)))
upset(fromList(list_global),
      nsets = 9,
      order.by = "freq",
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 0, 
      text.scale = 1.3 
      #sets.bar.color = c("purple","sienna1","deepskyblue","yellowgreen","violet"),
)

list_low <- list(DAM_low = c(unique(positive_capra_score[positive_capra_score$damico_num == 1,]$PUBCSNUM)),
                 NIC_low = c(unique(positive_capra_score[positive_capra_score$nice_num == 1,]$PUBCSNUM)),
                 CAP_low = c(unique(positive_capra_score[positive_capra_score$capra_score_num == 1,]$PUBCSNUM)))
upset(fromList(list_low),
      nintersects = NA,
      nsets = 3,
      order.by = "freq",
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 0, 
      text.scale = 1.3 
      #sets.bar.color = c("purple","sienna1","deepskyblue","yellowgreen","violet"),
)
list_intermediate <- list(DAM_int = c(unique(positive_capra_score[positive_capra_score$damico_num == 2,]$PUBCSNUM)),
                          NIC_int = c(unique(positive_capra_score[positive_capra_score$nice_num == 2,]$PUBCSNUM)),
                          CAP_int = c(unique(positive_capra_score[positive_capra_score$capra_score_num == 2,]$PUBCSNUM)))
upset(fromList(list_intermediate),
      nintersects = NA,
      nsets = 3,
      keep.order = T,
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 0 , 
      text.scale = 1.3
      #sets.bar.color = c("purple","sienna1","deepskyblue","yellowgreen","violet"),
)

list_high <- list(DAM_high = c(unique(positive_capra_score[positive_capra_score$damico_num == 3,]$PUBCSNUM)),
                  NIC_high = c(unique(positive_capra_score[positive_capra_score$nice_num == 3,]$PUBCSNUM)),
                  CAP_high = c(unique(positive_capra_score[positive_capra_score$capra_score_num == 3,]$PUBCSNUM)))
upset(fromList(list_high),
      nsets = 3,
      keep.order = T,
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 0, 
      text.scale = 1.3
      #sets.bar.color = c("purple","sienna1","deepskyblue","yellowgreen","violet"),
)

rm(cap_NC, cap_CD, cap_ND, cap_aF, cap_NDC)
####################################### Bubble ####################################### 

# Libraries
library(ggplot2)
library(plotly)
library(viridis)
library(hrbrthemes)
##### Prep data
a <- c("D'amico", "1", NROW(seer_risk[which(seer_risk$damico_num == 1),]), NROW(which(seer_risk$damico_num == 1)) / NROW(DAM) *100)
b <- c("D'amico", "2", NROW(seer_risk[which(seer_risk$damico_num == 2),]), NROW(which(seer_risk$damico_num == 2)) / NROW(DAM) *100)
c <- c("D'amico", "3", NROW(seer_risk[which(seer_risk$damico_num == 3),]), NROW(which(seer_risk$damico_num == 3)) / NROW(DAM) *100)
d <- c("Capra", "1", NROW(seer_risk[which(seer_risk$capra_score_num == 1),]), NROW(which(seer_risk$capra_score_num == 1)) / NROW(CAP) *100)
e <- c("Capra", "2", NROW(seer_risk[which(seer_risk$capra_score_num == 2),]), NROW(which(seer_risk$capra_score_num == 2)) / NROW(CAP) *100)
f <- c("Capra", "3", NROW(seer_risk[which(seer_risk$capra_score_num == 3),]), NROW(which(seer_risk$capra_score_num == 3)) / NROW(CAP) *100)
g <- c("NICE", "1", NROW(seer_risk[which(seer_risk$nice_num == 1),]), NROW(which(seer_risk$nice_num == 1)) / NROW(NIC) *100)
h <- c("NICE", "2", NROW(seer_risk[which(seer_risk$nice_num == 2),]), NROW(which(seer_risk$nice_num == 2)) / NROW(NIC) *100)
i <- c("NICE", "3", NROW(seer_risk[which(seer_risk$nice_num == 3),]), NROW(which(seer_risk$nice_num == 3)) / NROW(NIC) *100)
data <- data.frame(t(data.frame(a, b, c, d, e, f, g, h, i))) %>% 
  `colnames<-`(., c("risk_stratification", "risk", "number", "proportion"))
data$risk <- as.numeric(data$risk)
data$number <- as.numeric(as.character(data$number))
##### Plot
data <- data %>%
  # prepare text for tooltip
  mutate(text = paste("Risk Stratification: ", risk_stratification, "\nRisk: ", risk, "\nNbr: ",
                      number, "\nprop: ", proportion, sep="")) %>%
  
  # ggplot
  ggplot( aes(x=risk, y=number, size = number, color = risk_stratification, text=text)) +
  ggtitle("Patients per risk low intermediate high for each risk stratifictaion") +
  scale_y_continuous(limit = c(0, 330000)) +
  scale_x_continuous(limits = c(0.5,3.3)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1, 25), name="Risk stratification") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="top")
#plot.margin = margin(2, 2, 2, 2, "cm")
# ggplot interactive with plotly
data <- ggplotly(data, tooltip="text")
data

rm(a,b,c,d,e,f,g,h,i)
################################ Only on capra positive population
##### Prep data
a <- c("D'amico", "1", NROW(positive_capra_score[which(positive_capra_score$damico_num == 1),]), NROW(which(positive_capra_score$damico_num == 1)) / NROW(cap_DAM) *100)
b <- c("D'amico", "2", NROW(positive_capra_score[which(positive_capra_score$damico_num == 2),]), NROW(which(positive_capra_score$damico_num == 2)) / NROW(cap_DAM) *100)
c <- c("D'amico", "3", NROW(positive_capra_score[which(positive_capra_score$damico_num == 3),]), NROW(which(positive_capra_score$damico_num == 3)) / NROW(cap_DAM) *100)
d <- c("Capra", "1", NROW(positive_capra_score[which(positive_capra_score$capra_score_num == 1),]), NROW(which(positive_capra_score$capra_score_num == 1)) / NROW(cap_CAP) *100)
e <- c("Capra", "2", NROW(positive_capra_score[which(positive_capra_score$capra_score_num == 2),]), NROW(which(positive_capra_score$capra_score_num == 2)) / NROW(cap_CAP) *100)
f <- c("Capra", "3", NROW(positive_capra_score[which(positive_capra_score$capra_score_num == 3),]), NROW(which(positive_capra_score$capra_score_num == 3)) / NROW(cap_CAP) *100)
g <- c("NICE", "1", NROW(positive_capra_score[which(positive_capra_score$nice_num == 1),]), NROW(which(positive_capra_score$nice_num == 1)) / NROW(cap_NIC) *100)
h <- c("NICE", "2", NROW(positive_capra_score[which(positive_capra_score$nice_num == 2),]), NROW(which(positive_capra_score$nice_num == 2)) / NROW(cap_NIC) *100)
i <- c("NICE", "3", NROW(positive_capra_score[which(positive_capra_score$nice_num == 3),]), NROW(which(positive_capra_score$nice_num == 3)) / NROW(cap_NIC) *100)
data_cap_pos <- data.frame(t(data.frame(a, b, c, d, e, f, g, h, i))) %>% 
  `colnames<-`(., c("risk_stratification", "risk", "number", "proportion"))
data_cap_pos$risk <- as.numeric(data_cap_pos$risk)
data_cap_pos$number <- as.numeric(as.character(data_cap_pos$number))
##### Plot
data_cap_pos <- data_cap_pos %>%
  # prepare text for tooltip
  mutate(text = paste("Risk Stratification: ", risk_stratification, "\nRisk: ", risk, "\nNbr: ",
                      number, "\nprop: ", proportion, sep="")) %>%
  
  # ggplot
  ggplot( aes(x=risk, y=number, size = number, color = risk_stratification, text=text)) +
  ggtitle("Patients per risk low intermediate high for each risk stratifictaion") +
  scale_y_continuous(limit = c(0, 75000)) +
  scale_x_continuous(limits = c(0.5,3.3)) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1, 25), name="Risk stratification") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="top")
#plot.margin = margin(2, 2, 2, 2, "cm")
# ggplot interactive with plotly
data_cap_pos <- ggplotly(data_cap_pos, tooltip="text")
data_cap_pos



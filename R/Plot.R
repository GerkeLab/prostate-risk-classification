####################################### UpSet ####################################### 
library(UpSetR)
################################################################################# I ### risk_seer
#count the difference
DAM <- which(!is.na(risk_seer$damico))
CAP <- which(!is.na(risk_seer$capra_score))
NIC <- which(!is.na(risk_seer$nice))
NC <- which(risk_seer$nice == risk_seer$capra_score)
CD <- which(risk_seer$capra_score == risk_seer$damico)
ND <- which(risk_seer$damico == risk_seer$nice)
aF <- risk_seer[ND,]
NDC <- which(aF$damico == aF$capra_score)

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
list_global <- list(DAM_low = c(unique(risk_seer[risk_seer$damico == "Low",]$PUBCSNUM)),
                    NIC_low = c(unique(risk_seer[risk_seer$nice == "Low",]$PUBCSNUM)),
                    CAP_low = c(unique(risk_seer[risk_seer$capra_score == "Low",]$PUBCSNUM)),
                    DAM_int = c(unique(risk_seer[risk_seer$damico == "Intermediate",]$PUBCSNUM)),
                    NIC_int = c(unique(risk_seer[risk_seer$nice == "Intermediate",]$PUBCSNUM)),
                    CAP_int = c(unique(risk_seer[risk_seer$capra_score == "Intermediate",]$PUBCSNUM)),
                    DAM_high = c(unique(risk_seer[risk_seer$damico == "High",]$PUBCSNUM)),
                    NIC_high = c(unique(risk_seer[risk_seer$nice == "High",]$PUBCSNUM)),
                    CAP_high = c(unique(risk_seer[risk_seer$capra_score == "High",]$PUBCSNUM)))
upset(fromList(list_global),
      nsets = 9,
      order.by = "freq",
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 0, 
      text.scale = 1.3 
      #sets.bar.color = c("purple","sienna1","deepskyblue","yellowgreen","violet"),
)

list_low <- list(DAM_low = c(unique(risk_seer[risk_seer$damico == "Low",]$PUBCSNUM)),
                 NIC_low = c(unique(risk_seer[risk_seer$nice == "Low",]$PUBCSNUM)),
                 CAP_low = c(unique(risk_seer[risk_seer$capra_score == "Low",]$PUBCSNUM)))
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
list_intermediate <- list(DAM_int = c(unique(risk_seer[risk_seer$damico == "Intermediate",]$PUBCSNUM)),
                          NIC_int = c(unique(risk_seer[risk_seer$nice == "Intermediate",]$PUBCSNUM)),
                          CAP_int = c(unique(risk_seer[risk_seer$capra_score == "Intermediate",]$PUBCSNUM)))
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

list_high <- list(DAM_high = c(unique(risk_seer[risk_seer$damico == "High",]$PUBCSNUM)),
                  NIC_high = c(unique(risk_seer[risk_seer$nice == "High",]$PUBCSNUM)),
                  CAP_high = c(unique(risk_seer[risk_seer$capra_score == "High",]$PUBCSNUM)))
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
# DAM <- which(!is.na(ncdb$damico))
# CAP <- which(!is.na(ncdb$capra_score))
# NIC <- which(!is.na(ncdb$nice))
# 
# NC <- which(ncdb$nice == ncdb$capra_score)
# CD <- which(ncdb$capra_score == ncdb$damico)
# 
# ND <- which(ncdb$damico == ncdb$nice)
# aF <- ncdb[ND,]
# #aF[159:161]
# 
# NDC <- which(aF$damico == aF$capra_score)
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
############################################################################### III ### risk_seer only capra+ DF

positive_capra_score <- risk_seer[which(!is.na(risk_seer$capra_score)),]
cap_DAM <- which(!is.na(positive_capra_score$damico))
cap_CAP <- which(!is.na(positive_capra_score$capra_score))
cap_NIC <- which(!is.na(positive_capra_score$nice))
cap_NC <- which(positive_capra_score$nice == positive_capra_score$capra_score)
cap_CD <- which(positive_capra_score$capra_score == positive_capra_score$damico)
cap_ND <- which(positive_capra_score$damico == positive_capra_score$nice)
cap_aF <- positive_capra_score[cap_ND,]
cap_NDC <- which(cap_aF$damico == cap_aF$capra_score)

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

list_global <- list(DAM_low = c(unique(positive_capra_score[positive_capra_score$damico == "Low",]$PUBCSNUM)),
                    NIC_low = c(unique(positive_capra_score[positive_capra_score$nice == "Low",]$PUBCSNUM)),
                    CAP_low = c(unique(positive_capra_score[positive_capra_score$capra_score == "Low",]$PUBCSNUM)),
                    DAM_int = c(unique(positive_capra_score[positive_capra_score$damico == "Intermediate",]$PUBCSNUM)),
                    NIC_int = c(unique(positive_capra_score[positive_capra_score$nice == "Intermediate",]$PUBCSNUM)),
                    CAP_int = c(unique(positive_capra_score[positive_capra_score$capra_score == "Intermediate",]$PUBCSNUM)),
                    DAM_high = c(unique(positive_capra_score[positive_capra_score$damico == "High",]$PUBCSNUM)),
                    NIC_high = c(unique(positive_capra_score[positive_capra_score$nice == "High",]$PUBCSNUM)),
                    CAP_high = c(unique(positive_capra_score[positive_capra_score$capra_score == "High",]$PUBCSNUM)))
upset(fromList(list_global),
      nsets = 9,
      order.by = "freq",
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 0, 
      text.scale = 1.3 
      #sets.bar.color = c("purple","sienna1","deepskyblue","yellowgreen","violet"),
)

list_low <- list(DAM_low = c(unique(positive_capra_score[positive_capra_score$damico == "Low",]$PUBCSNUM)),
                 NIC_low = c(unique(positive_capra_score[positive_capra_score$nice == "Low",]$PUBCSNUM)),
                 CAP_low = c(unique(positive_capra_score[positive_capra_score$capra_score == "Low",]$PUBCSNUM)))
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
list_intermediate <- list(DAM_int = c(unique(positive_capra_score[positive_capra_score$damico == "Intermediate",]$PUBCSNUM)),
                          NIC_int = c(unique(positive_capra_score[positive_capra_score$nice == "Intermediate",]$PUBCSNUM)),
                          CAP_int = c(unique(positive_capra_score[positive_capra_score$capra_score == "Intermediate",]$PUBCSNUM)))
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

list_high <- list(DAM_high = c(unique(positive_capra_score[positive_capra_score$damico == "High",]$PUBCSNUM)),
                  NIC_high = c(unique(positive_capra_score[positive_capra_score$nice == "High",]$PUBCSNUM)),
                  CAP_high = c(unique(positive_capra_score[positive_capra_score$capra_score == "High",]$PUBCSNUM)))
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
a <- c("D'amico", "Low", NROW(risk_seer[which(risk_seer$damico == "Low"),]), NROW(which(risk_seer$damico == "Low")) / NROW(DAM) *100)
b <- c("D'amico", "Intermediate", NROW(risk_seer[which(risk_seer$damico == "Intermediate"),]), NROW(which(risk_seer$damico == "Intermediate")) / NROW(DAM) *100)
c <- c("D'amico", "High", NROW(risk_seer[which(risk_seer$damico == "High"),]), NROW(which(risk_seer$damico == "High")) / NROW(DAM) *100)
d <- c("Capra", "Low", NROW(risk_seer[which(risk_seer$capra_score == "Low"),]), NROW(which(risk_seer$capra_score == "Low")) / NROW(CAP) *100)
e <- c("Capra", "Intermediate", NROW(risk_seer[which(risk_seer$capra_score == "Intermediate"),]), NROW(which(risk_seer$capra_score == "Intermediate")) / NROW(CAP) *100)
f <- c("Capra", "High", NROW(risk_seer[which(risk_seer$capra_score == "High"),]), NROW(which(risk_seer$capra_score == "High")) / NROW(CAP) *100)
g <- c("NICE", "Low", NROW(risk_seer[which(risk_seer$nice == "Low"),]), NROW(which(risk_seer$nice == "Low")) / NROW(NIC) *100)
h <- c("NICE", "Intermediate", NROW(risk_seer[which(risk_seer$nice == "Intermediate"),]), NROW(which(risk_seer$nice == "Intermediate")) / NROW(NIC) *100)
i <- c("NICE", "High", NROW(risk_seer[which(risk_seer$nice == "High"),]), NROW(which(risk_seer$nice == "High")) / NROW(NIC) *100)
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
  #theme_ipsum() +
  theme(legend.position="top")
#plot.margin = margin(2, 2, 2, 2, "cm")
# ggplot interactive with plotly
data <- ggplotly(data, tooltip="text")
data

rm(a,b,c,d,e,f,g,h,i)
################################ Only on capra positive population
##### Prep data
a <- c("D'amico", "Low", NROW(positive_capra_score[which(positive_capra_score$damico == "Low"),]), NROW(which(positive_capra_score$damico == "Low")) / NROW(cap_DAM) *100)
b <- c("D'amico", "Intermediate", NROW(positive_capra_score[which(positive_capra_score$damico == "Intermediate"),]), NROW(which(positive_capra_score$damico == "Intermediate")) / NROW(cap_DAM) *100)
c <- c("D'amico", "High", NROW(positive_capra_score[which(positive_capra_score$damico == "High"),]), NROW(which(positive_capra_score$damico == "High")) / NROW(cap_DAM) *100)
d <- c("Capra", "Low", NROW(positive_capra_score[which(positive_capra_score$capra_score == "Low"),]), NROW(which(positive_capra_score$capra_score == "Low")) / NROW(cap_CAP) *100)
e <- c("Capra", "Intermediate", NROW(positive_capra_score[which(positive_capra_score$capra_score == "Intermediate"),]), NROW(which(positive_capra_score$capra_score == "Intermediate")) / NROW(cap_CAP) *100)
f <- c("Capra", "High", NROW(positive_capra_score[which(positive_capra_score$capra_score == "High"),]), NROW(which(positive_capra_score$capra_score == "High")) / NROW(cap_CAP) *100)
g <- c("NICE", "Low", NROW(positive_capra_score[which(positive_capra_score$nice == "Low"),]), NROW(which(positive_capra_score$nice == "Low")) / NROW(cap_NIC) *100)
h <- c("NICE", "Intermediate", NROW(positive_capra_score[which(positive_capra_score$nice == "Intermediate"),]), NROW(which(positive_capra_score$nice == "Intermediate")) / NROW(cap_NIC) *100)
i <- c("NICE", "High", NROW(positive_capra_score[which(positive_capra_score$nice == "High"),]), NROW(which(positive_capra_score$nice == "High")) / NROW(cap_NIC) *100)
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
  #theme_ipsum() +
  theme(legend.position="top")
#plot.margin = margin(2, 2, 2, 2, "cm")
# ggplot interactive with plotly
data_cap_pos <- ggplotly(data_cap_pos, tooltip="text")
data_cap_pos


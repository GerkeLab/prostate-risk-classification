library(UpSetR)

####################################################### SEER
#count the difference
DAM <- which(!is.na(seer$damico_num))
CAP <- which(!is.na(seer$capra_score_num))
NIC <- which(!is.na(seer$nice_num))

NC <- which(seer$nice_num == seer$capra_score_num)
CD <- which(seer$capra_score_num == seer$damico_num)

ND <- which(seer$damico_num == seer$nice_num)
aF <- seer[ND,]
#aF[159:161]

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


# Jordan's code
list_pop <- list(oneD = c(unique(seer[seer$damico_num == 1,]$PUBCSNUM)),
                 oneN = c(unique(seer[seer$nice_num == 1,]$PUBCSNUM)),
                 oneC = c(unique(seer[seer$capra_score_num == 1,]$PUBCSNUM)),
                 twoD = c(unique(seer[seer$damico_num == 2,]$PUBCSNUM)),
                 twoN = c(unique(seer[seer$nice_num == 2,]$PUBCSNUM)),
                 twoC = c(unique(seer[seer$capra_score_num == 2,]$PUBCSNUM)),
                 threeD = c(unique(seer[seer$damico_num == 3,]$PUBCSNUM)),
                 threeN = c(unique(seer[seer$nice_num == 3,]$PUBCSNUM)),
                 threeC = c(unique(seer[seer$capra_score_num == 3,]$PUBCSNUM)))

# list_pop <- list(EUR = c(unique(admixture_calls[admixture_calls$POP=="EUR",]$ID)),
#                  EAS = c(unique(admixture_calls[admixture_calls$POP=="EAS",]$ID)),
#                  AMR = c(unique(admixture_calls[admixture_calls$POP=="AMR",]$ID)),
#                  SAS = c(unique(admixture_calls[admixture_calls$POP=="SAS",]$ID)),
#                  AFR = c(unique(admixture_calls[admixture_calls$POP=="AFR",]$ID)))
# pdf("figures/pop_upset.pdf", height = 6, width = 9)
upset(fromList(list_pop),
      nsets = 9,
      order.by = "freq",
      mainbar.y.label = "Risk Intersections",
      sets.x.label = "Patients Per Risk",
      number.angles = 30 
      #sets.bar.color = c("purple","sienna1","deepskyblue","yellowgreen","violet"),
)

####################################################### NCDB
#count the difference
DAM <- which(!is.na(ncdb$damico_num))
CAP <- which(!is.na(ncdb$capra_score_num))
NIC <- which(!is.na(ncdb$nice_num))

NC <- which(ncdb$nice_num == ncdb$capra_score_num)
CD <- which(ncdb$capra_score_num == ncdb$damico_num)

ND <- which(ncdb$damico_num == ncdb$nice_num)
aF <- ncdb[ND,]
#aF[159:161]

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


# Jordan's code
list_pop <- list(oneD = c(unique(ncdb[ncdb$damico_num == 1,]$PUBCSNUM)),
                 oneN = c(unique(ncdb[ncdb$nice_num == 1,]$PUBCSNUM)),
                 oneC = c(unique(ncdb[ncdb$capra_score_num == 1,]$PUBCSNUM)),
                 twoD = c(unique(ncdb[ncdb$damico_num == 2,]$PUBCSNUM)),
                 twoN = c(unique(ncdb[ncdb$nice_num == 2,]$PUBCSNUM)),
                 twoC = c(unique(ncdb[ncdb$capra_score_num == 2,]$PUBCSNUM)),
                 threeD = c(unique(ncdb[ncdb$damico_num == 3,]$PUBCSNUM)),
                 threeN = c(unique(ncdb[ncdb$nice_num == 3,]$PUBCSNUM)),
                 threeC = c(unique(ncdb[ncdb$capra_score_num == 3,]$PUBCSNUM)))


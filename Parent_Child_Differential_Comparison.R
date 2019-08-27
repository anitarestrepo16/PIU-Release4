#set working directory
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/Project")
#turn on library
library(base)
#################### Import all data ##############
temp = list.files(pattern = "*csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

############ Make NAs into zeros #############
IAT[is.na(IAT)] <- 0
PCIAT[is.na(PCIAT)] <- 0

APQ_P[is.na(APQ_P)] <- 0
APQ_SR[is.na(APQ_SR)] <- 0

MFQ_P[is.na(MFQ_P)] <- 0
MFQ_SR[is.na(MFQ_SR)] <- 0

SCARED_P[is.na(SCARED_P)] <- 0
SCARED_SR[is.na(SCARED_SR)] <- 0

############### ensure total scores are correctly computed for PCIAT and IAT #############

IAT$IAT_Total <- rowSums(IAT[2:21])
PCIAT$PCIAT_Total <- rowSums(PCIAT[2:21])

########################### pull out values of interest
IAT_of_interest <- data.frame(
  ID = IAT$URSI,
  IAT_SR = IAT$IAT_Total)

PCIAT_of_interest <- data.frame(
  ID = PCIAT$URSI,
  IAT_Parent = PCIAT$PCIAT_Total)

Demos_of_interest <- data.frame(
  ID = Basic_Demos$URSI,
  Sex = Basic_Demos$Sex,
  Age = floor(Basic_Demos$Age),
  Site = Basic_Demos$Study_Site)

Barratt_of_interest <- data.frame(
  ID = Barratt$URSI,
  SES = Barratt$Barratt_Total)

SCARED_SR_of_interest <- data.frame(
  ID = SCARED_SR$URSI,
  Social_Anxiety_SR = SCARED_SR$SCARED_SR_SC,
  Total_Anxiety_SR = SCARED_SR$SCARED_SR_Total)

SCARED_P_of_interest <- data.frame(
  ID = SCARED_P$URSI,
  Social_Anxiety_P = SCARED_P$SCARED_P_SC,
  Total_Anxiety_P = SCARED_P$SCARED_P_Total)

MFQ_SR_of_interest <- data.frame(
  ID = MFQ_SR$URSI,
  Depression_SR = MFQ_SR$MFQ_SR_Total)

MFQ_P_of_interest <- data.frame(
  ID = MFQ_P$URSI,
  Depression_P = MFQ_P$MFQ_P_Total)

APQ_P_of_interest <- data.frame(
  ID = APQ_P$URSI,
  Involvement_P = APQ_P$APQ_P_INV,
  Positive_Parenting_P = APQ_P$APQ_P_PP,
  Poor_Monitoring_P = APQ_P$APQ_P_PM,
  Inconsistent_Discipline_P = APQ_P$APQ_P_ID,
  Corporal_Punishment_P = APQ_P$APQ_P_CP)

APQ_SR_of_interest <- data.frame(
  ID = APQ_SR$URSI,
  Involvement_Mother_SR = APQ_SR$APQ_SR_INV_M,
  Involvement_Father_SR = APQ_SR$APQ_SR_INV_D,
  Positive_Parenting_SR = APQ_SR$APQ_SR_PP,
  Poor_Monitoring_SR = APQ_SR$APQ_SR_PM,
  Inconsistent_Discipline_SR = APQ_SR$APQ_SR_ID,
  Corporal_Punishment_SR = APQ_SR$APQ_SR_CP)

############### merge into one data frame #########
# create merge function
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

Parent_Child_Comparisons <- Reduce(Merge, list(IAT_of_interest,
                                               PCIAT_of_interest, SCARED_SR_of_interest, SCARED_P_of_interest,
                                               MFQ_SR_of_interest, MFQ_P_of_interest, APQ_P_of_interest, APQ_SR_of_interest,
                                               Demos_of_interest, Barratt_of_interest))

################### compute differential scores (parent minus child for all)
Parent_Child_Comparisons$IAT_Delta <- Parent_Child_Comparisons$IAT_Parent - Parent_Child_Comparisons$IAT_SR

Parent_Child_Comparisons$Social_Anxiety_Delta <- Parent_Child_Comparisons$Social_Anxiety_P - 
  Parent_Child_Comparisons$Social_Anxiety_SR

Parent_Child_Comparisons$Total_Anxiety_Delta <- Parent_Child_Comparisons$Total_Anxiety_P - 
  Parent_Child_Comparisons$Total_Anxiety_SR

Parent_Child_Comparisons$Depression_Delta <- Parent_Child_Comparisons$Depression_P - 
  Parent_Child_Comparisons$Depression_SR

Parent_Child_Comparisons$Positive_Parenting_Delta <- Parent_Child_Comparisons$Positive_Parenting_P - 
  Parent_Child_Comparisons$Positive_Parenting_SR

Parent_Child_Comparisons$Poor_Monitoring_Delta <- Parent_Child_Comparisons$Poor_Monitoring_P - 
  Parent_Child_Comparisons$Poor_Monitoring_SR

Parent_Child_Comparisons$Inconsistent_Discipline_Delta <- Parent_Child_Comparisons$Inconsistent_Discipline_P - 
  Parent_Child_Comparisons$Inconsistent_Discipline_SR

Parent_Child_Comparisons$Corporal_Punishment_Delta <- Parent_Child_Comparisons$Corporal_Punishment_P - 
  Parent_Child_Comparisons$Corporal_Punishment_SR

##################### correlation matrix of differential scores

library(psych)

###### Select columns to be included and correlate variables
correlationvars2 <- Parent_Child_Comparisons[,21:32]
rvalues2 <- cor(correlationvars2, use="pairwise.complete.obs")
#get P and n values for correlations
significance2 <- corr.test(correlationvars2, adjust="fdr")
pvalues2 <- significance2$p
nvalues2 <- array(significance2$n)
# Set plot parameters, color, and labels
par(mfrow=c(1,1))
col4 <- colorRampPalette(c("red","#FF7F00","yellow", "white",
                           "cyan", "#007FFF","#00007F")) 
#colnames(rvalues2) <- c("Sex", "Barratt", "CBCL: Total", "CBCL: Int", "CBCL: Ext","SDQ: Total","SDQ: Int","SDQ: Ext",
#                        "SDQ: Peer Prob", "SDQ: Prosocial", "MFQ(P)","MFQ(SR)","SCARED(P)",
#                       "SCARED(SR)","ARI(P)","ARI(SR)","SWAN: Total","SWAN: Inatt", "SWAN: Hyper",
#                        "ICU(P)","ASSQ","SCQ","SRS", "FSIQ","PIQ","VIQ","CELF",
#                        "BMI","Food Addiction: Symp Count","Food Addiction: DX",
#                        "Internet Addiction")
#rownames(rvalues2) <- c("Sex", "Barratt", "CBCL: Total", "CBCL: Int", "CBCL: Ext","SDQ: Total","SDQ: Int","SDQ: Ext",
#                        "SDQ: Peer Prob", "SDQ: Prosocial", "MFQ(P)","MFQ(SR)","SCARED(P)",
#                        "SCARED(SR)","ARI(P)","ARI(SR)","SWAN: Total","SWAN: Inatt", "SWAN: Hyper",
#                        "ICU(P)","ASSQ","SCQ","SRS", "FSIQ","PIQ","VIQ","CELF",
#                        "BMI","Food Addiction: Symp Count","Food Addiction: DX",
#                        "Internet Addiction")
###PLOT CORRELATION MATRIX
library(corrplot)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")

#save correlation matrix
png(file="Parent_Child_Comparisons_Corr.png", width = 1200, height = 1000)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")
dev.off()


################# boxplot of differential scores


png(file="Parent_Child_Comparisons_Boxplot.png", width = 1400, height = 800)
boxplot(Parent_Child_Comparisons$IAT_Delta, Parent_Child_Comparisons$Social_Anxiety_Delta, 
        Parent_Child_Comparisons$Total_Anxiety_Delta, Parent_Child_Comparisons$Depression_Delta, 
        Parent_Child_Comparisons$Positive_Parenting_Delta, Parent_Child_Comparisons$Poor_Monitoring_Delta,
        Parent_Child_Comparisons$Inconsistent_Discipline_Delta, Parent_Child_Comparisons$Corporal_Punishment_Delta, 
        main = "Parent-Child Discrepancies by Measure", xlab = "Measure", ylab = "Discrepancy Score (Parent - Child)",
        names = c("IAT", "Social Anxiety", "Total Anxiety", "Depression", "Positive Parenting", "Poor Monitoring", 
                  "Inconsistent Discipline", "Corporal Punishment"))
abline(h=0)
dev.off()

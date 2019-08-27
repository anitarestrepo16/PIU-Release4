#set working directory
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/Project")
#turn on library
library(base)

#################### Import all data ##############
temp = list.files(pattern = "*csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

temp = list.files(pattern = "*txt")
list2env(
  lapply(setNames(temp, make.names(gsub("*.txt$", "", temp))), 
         read.delim), envir = .GlobalEnv)

# pull out variables of interest

# create z-scores for each APQ subscale and average out relevant subscales to form positive and negative parenting composites
APQ_P_of_interest <- data.frame(
  ID = APQ_P$URSI,
  Involvement_P = APQ_P$APQ_P_INV,
  Involvement_P_z = ((APQ_P$APQ_P_INV - mean(APQ_P$APQ_P_INV))/sd(APQ_P$APQ_P_INV)),
  Positive_Parenting_P = APQ_P$APQ_P_PP,
  Positive_Parenting_P_z = ((APQ_P$APQ_P_PP - mean(APQ_P$APQ_P_PP))/sd(APQ_P$APQ_P_PP)),
  Poor_Monitoring_P = APQ_P$APQ_P_PM,
  Poor_Monitoring_P_z = ((APQ_P$APQ_P_PM - mean(APQ_P$APQ_P_PM))/sd(APQ_P$APQ_P_PM)),
  Inconsistent_Discipline_P = APQ_P$APQ_P_ID,
  Inconsistent_Discipline_P_z = ((APQ_P$APQ_P_ID - mean(APQ_P$APQ_P_ID))/sd(APQ_P$APQ_P_ID)),
  Corporal_Punishment_P = APQ_P$APQ_P_CP,
  Corporal_Punishment_P_z = ((APQ_P$APQ_P_CP - mean(APQ_P$APQ_P_CP))/sd(APQ_P$APQ_P_CP)))

APQ_SR_of_interest <- data.frame(
  ID = APQ_SR$URSI,
  Involvement_Mother_SR = APQ_SR$APQ_SR_INV_M,
  Involvement_Mother_SR_z = ((APQ_SR$APQ_SR_INV_M - mean(APQ_SR$APQ_SR_INV_M))/sd(APQ_SR$APQ_SR_INV_M)),
  Involvement_Father_SR = APQ_SR$APQ_SR_INV_D,
  Involvement_Father_SR_z = ((APQ_SR$APQ_SR_INV_D - mean(APQ_SR$APQ_SR_INV_D))/sd(APQ_SR$APQ_SR_INV_D)),
  Positive_Parenting_SR = APQ_SR$APQ_SR_PP,
  Positive_Parenting_SR_z = ((APQ_SR$APQ_SR_PP - mean(APQ_SR$APQ_SR_PP))/sd(APQ_SR$APQ_SR_PP)),
  Poor_Monitoring_SR = APQ_SR$APQ_SR_PM,
  Poor_Monitoring_SR_z = ((APQ_SR$APQ_SR_PM - mean(APQ_SR$APQ_SR_PM))/sd(APQ_SR$APQ_SR_PM)),
  Inconsistent_Discipline_SR = APQ_SR$APQ_SR_ID,
  Inconsistent_Discipline_SR_z = ((APQ_SR$APQ_SR_ID - mean(APQ_SR$APQ_SR_ID))/sd(APQ_SR$APQ_SR_ID)),
  Corporal_Punishment_SR = APQ_SR$APQ_SR_CP,
  Corporal_Punishment_SR_z = ((APQ_SR$APQ_SR_CP - mean(APQ_SR$APQ_SR_CP))/sd(APQ_SR$APQ_SR_CP)))

# calculate positive and negative parenting composites
#parent-report
APQ_P_of_interest$Positive_Parenting_Composite_P <- (APQ_P_of_interest$Involvement_P_z + APQ_P_of_interest$Positive_Parenting_P_z)/2
APQ_P_of_interest$Negative_Parenting_Composite_P <- (APQ_P_of_interest$Poor_Monitoring_P_z + APQ_P_of_interest$Inconsistent_Discipline_P_z +
                                                       APQ_P_of_interest$Corporal_Punishment_P_z)/3

#self-report
APQ_SR_of_interest$Positive_Parenting_Composite_SR <- (APQ_SR_of_interest$Involvement_Mother_SR_z + APQ_SR_of_interest$Involvement_Father_SR_z +
                                                         APQ_SR_of_interest$Positive_Parenting_SR_z)/2
APQ_SR_of_interest$Negative_Parenting_Composite_SR <- (APQ_SR_of_interest$Poor_Monitoring_SR_z + APQ_SR_of_interest$Inconsistent_Discipline_SR_z +
                                                         APQ_SR_of_interest$Corporal_Punishment_SR_z)/3

##################### Correlation for APQ SR ###########

library(psych)

###### Select columns to be included and correlate variables
correlationvars2 <- APQ_SR_of_interest[,-c(1, 3, 5, 7, 9, 11, 13)]
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
png(file="Corr_APQ_SR.png", width = 1200, height = 1000)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")
dev.off()

##################### Correlation for Parent APQ ###########

library(psych)

###### Select columns to be included and correlate variables
correlationvars2 <- APQ_P_of_interest[,-c(1, 3, 5, 7, 9, 11)]
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
png(file="Corr_APQ_P.png", width = 1200, height = 1000)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")
dev.off()

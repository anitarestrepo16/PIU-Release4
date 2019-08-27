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


#create symptom count score for YFAS
YFAS_C$Score_1 <- ifelse(YFAS_C$YFAS_C_Score_01 >= 1, 1, 0)
YFAS_C$Score_2 <- ifelse(YFAS_C$YFAS_C_Score_02 >= 1, 1, 0)
YFAS_C$Score_3 <- ifelse(YFAS_C$YFAS_C_Score_03 >= 1, 1, 0)
YFAS_C$Score_4 <- ifelse(YFAS_C$YFAS_C_Score_04 >= 1, 1, 0)
YFAS_C$Score_5 <- ifelse(YFAS_C$YFAS_C_Score_05 >= 1, 1, 0)
YFAS_C$Score_6 <- ifelse(YFAS_C$YFAS_C_Score_06 >= 1, 1, 0)
YFAS_C$Score_7 <- ifelse(YFAS_C$YFAS_C_Score_07 >= 1, 1, 0)

YFAS_C$Symptom_Count <- YFAS_C$Score_1 + YFAS_C$Score_2 + YFAS_C$Score_3 +
  YFAS_C$Score_4 + YFAS_C$Score_5 + YFAS_C$Score_6 + YFAS_C$Score_7

YFAS$Score_1 <- ifelse(YFAS$YFAS_Score_01 >= 1, 1, 0)
YFAS$Score_2 <- ifelse(YFAS$YFAS_Score_02 >= 1, 1, 0)
YFAS$Score_3 <- ifelse(YFAS$YFAS_Score_03 >= 1, 1, 0)
YFAS$Score_4 <- ifelse(YFAS$YFAS_Score_04 >= 1, 1, 0)
YFAS$Score_5 <- ifelse(YFAS$YFAS_Score_05 >= 1, 1, 0)
YFAS$Score_6 <- ifelse(YFAS$YFAS_Score_06 >= 1, 1, 0)
YFAS$Score_7 <- ifelse(YFAS$YFAS_Score_07 >= 1, 1, 0)

YFAS$Symptom_Count <- YFAS$Score_1 + YFAS$Score_2 + YFAS$Score_3 +
  YFAS$Score_4 + YFAS$Score_5 + YFAS$Score_6 + YFAS$Score_7

# pull out variables of interest
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

AUDIT_of_interest <- data.frame(
  ID = AUDIT$URSI, 
  AUDIT_Total = AUDIT$AUDIT_Total_Score)

ESPAD_of_interest <- data.frame(
  ID = ESPAD$URSI,
  Cannabis = ESPAD$ESPAD_01d)

FTQA_of_interest <- data.frame(
  ID = FTQA$URSI,
  Nicotine = FTQA$FTQA_Total)

FTND_of_interest <- data.frame(
  ID = FTND$URSI,
  Nicotine = FTND$FTND_Total)

YFAS_A_of_interest <- data.frame(
  ID = YFAS$URSI,
  YFAS_Impairment = YFAS$YFAS_Score_08,
  YFAS_Symptom_Count = YFAS$Symptom_Count)

YFAS_C_of_interest <- data.frame(
  ID = YFAS_C$URSI,
  YFAS_Impairment = YFAS_C$YFAS_C_Score_08,
  YFAS_Symptom_Count = YFAS_C$Symptom_Count)

APQ_P_of_interest <- data.frame(
  ID = APQ_P$URSI,
  APQ_Parent = APQ_P$APQ_P_Total)
  #APQ_P[,-(1:43)])

APQ_SR_of_interest <- data.frame(
  ID = APQ_SR$URSI,
  APQ_Child = APQ_SR$APQ_SR_Total)
  #APQ_SR[,-(1:52)])

DTS_of_interest <- data.frame(
  ID = DTS$URSI,
  DTS$DTS_Total)

PSI_of_interest <- data.frame(
  ID = PSI$URSI,
  PSI_T_Score = PSI$PSI_Total_T)

CCSC_of_interest <- data.frame(
  ID = CCSC$URSI,
  Problem_Focused_Coping = CCSC$CCSC_PFC,
  Cognitive_Decision_Making = CCSC$CCSC_CDM,
  Direct_Problem_Solving = CCSC$CCSC_DPS,
  Seeking_Understanding = CCSC$CCSC_SU,
  Avoidance_Coping = CCSC$CCSC_AC,
  Avoidant_Actions = CCSC$CCSC_AA,
  Repression = CCSC$CCSC_REP,
  Wishful_Thinking = CCSC$CCSC_WT,
  Positive_Cognitive_Restructuring = CCSC$CCSC_PCR,
  Control = CCSC$CCSC_CON,
  Optimism = CCSC$CCSC_OPT,
  Positivity = CCSC$CCSC_POS,
  Religion = CCSC$CCSC_REL,
  Support_Seeking = CCSC$CCSC_SS,
  Support_Sought_from_Guardian = CCSC$CCSC_SUPMF,
  Support_Sought_from_Other_Adults = CCSC$CCSC_SUPOA,
  Support_Sought_from_Peers = CCSC$CCSC_SUPEER,
  Support_Sought_from_Siblings = CCSC$CCSC_SUPSIB)
  
  #CCSC[, -(1:57)])

# merge dataframes for nicotine and YFAs
Nicotine_of_interest = rbind(FTQA_of_interest, FTND_of_interest)

YFAS_of_interest = rbind(YFAS_A_of_interest, YFAS_C_of_interest)

# create merge function
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

# create correlation data frame
Corr_substance_coping_parenting <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, Demos_of_interest, 
                                                      Barratt_of_interest, AUDIT_of_interest, ESPAD_of_interest, 
                                                      Nicotine_of_interest, YFAS_of_interest, APQ_P_of_interest,
                                                      APQ_SR_of_interest, DTS_of_interest, PSI_of_interest, CCSC_of_interest))
write.csv(Corr_substance_coping_parenting, "Corr_substance_coping_parenting.csv", row.names = FALSE)

# new data frame for only substance use measures
Corr_substance <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, Demos_of_interest, 
                                                      Barratt_of_interest, AUDIT_of_interest, ESPAD_of_interest, 
                                                      Nicotine_of_interest, YFAS_of_interest))
write.csv(Corr_substance, "Corr_substance.csv", row.names = FALSE)

# new data frame for APQ subscales + composites

  
  
#################### new code from Lindsay ###########

library(psych)

###### Select columns to be included and correlate variables
correlationvars2 <- Corr_substance[,-1]
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
png(file="Corr_substance.png", width = 1200, height = 1000)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")
dev.off()

# create correlation matrix w/ R values
png(file="Corr_substance_coping_parenting_values.png", width = 1400, height = 1200)
corrplot(rvalues2, p.mat = pvalues2, method="number", insig = "p-value",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")
dev.off()

# make data frame with r, p and degrees of freedom
Corr_substance_coping_parenting_values <- as.matrix(significance2)
pvalues <- data.frame(pvalues2)
rvalues <- data.frame(rvalues2)
df <- data.frame(significance2$n - 2)

#set working directory
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/Project")
#turn on library
library(base)

#################### Import all data ##############
temp = list.files(pattern = "*csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

#################### Create data frames ################
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

Demos_of_interest <- data.frame(
  ID = Basic_Demos$URSI,
  Sex = Basic_Demos$Sex,
  Age = floor(Basic_Demos$Age),
  Site = Basic_Demos$Study_Site)

Barratt_of_interest <- data.frame(
  ID = Barratt$URSI,
  Maternal_Education = Barratt$Barratt_P1_Edu,
  SES = Barratt$Barratt_Total)

ASSQ_of_interest <- data.frame(
  ID = ASSQ$URSI,
  ASSQ_Total = ASSQ$ASSQ_Total)

AUDIT_of_interest <- data.frame(
  ID = AUDIT$URSI, 
  AUDIT_Total = AUDIT$AUDIT_Total_Score)

Conners_of_interest <- data.frame(
  ID = Conners$URSI,
  Conners_Hyperactivity_Impulsivity = Conners$C3SR_HY_T,
  Conners_Inattention = Conners$C3SR_IN_T,
  Conners_Learning_Problems = Conners$C3SR_LP_T)

SWAN_of_interest <- data.frame(
  ID = SWAN$F,
  SWAN_Inattentive = SWAN$SWAN_IN,
  SWAN_Hyperactive = SWAN$SWAN_HY)

CBCL_post_of_interest <- data.frame(
  ID = CBCL$URSI,
  CBCL_Internalizing = CBCL$CBCL_Int_T,
  CBCL_Externalizing = CBCL$CBCL_Ext_T,
  CBCL_Total = CBCL$CBCL_Total_T)

CBCL_Pre_of_interest <- data.frame(
  ID = CBCL_Pre$URSI,
  CBCL_Internalizing = CBCL_Pre$CBCLPre_Int_T,
  CBCL_Externalizing = CBCL_Pre$CBCLPre_Ext_T,
  CBCL_Total = CBCL_Pre$CBCLPre_Total_T)

YSR_of_interest <- data.frame(
  ID = YSR$URSI,
  YSR_Internalizing = YSR$YSR_Int_T,
  YSR_Externalixing = YSR$YSR_Ext_T,
  YSR_Total = YSR$YSR_Total_T)

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

#merge CBCL and Nicotine dfs
CBCL_of_interest = rbind(
  CBCL_post_of_interest, CBCL_Pre_of_interest)

Nicotine_of_interest = rbind(FTQA_of_interest, FTND_of_interest)

YFAS_of_interest = rbind(YFAS_A_of_interest, YFAS_C_of_interest)

# create merge function
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

# create dataframe for final correlation
Final_Correlation <- Reduce(Merge, list(IAT_no_missing[,c(1, 22)], PCIAT_no_missing[,c(1, 22)], Demos_of_interest,
                                        Barratt_of_interest, ASSQ_of_interest, AUDIT_of_interest,
                                        Conners_of_interest, SWAN_of_interest, CBCL_of_interest ,
                                        YSR_of_interest, ESPAD_of_interest, Nicotine_of_interest, YFAS_of_interest, 
                                        SCARED_SR_of_interest, SCARED_P_of_interest, MFQ_SR_of_interest,
                                        MFQ_P_of_interest))


#################### Plot Correlation Matrix ###########

library(psych)

###### Select columns to be included and correlate variables
Final_Correlation <- Final_Correlation[,-1]
rvalues2 <- cor(Final_Correlation, use="pairwise.complete.obs")
#get P and N values for correlations
significance2 <- corr.test(Final_Correlation, adjust="fdr")
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
png(file="Final_Correlation.png", width = 1200, height = 1000)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")
dev.off()

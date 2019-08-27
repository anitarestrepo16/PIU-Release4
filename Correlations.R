#set working directory
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/Project")
#turn on library
library(base)

#################### Import all data ##############
temp = list.files(pattern = "*csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

###################### correlation matrix ############

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

Demos_of_interest <- data.frame(
  ID = Basic_Demos$URSI,
  Sex = Basic_Demos$Sex,
  Age = floor(Basic_Demos$Age),
  Site = Basic_Demos$Study_Site)

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


# create dataframe for correlation - Correlation #1
Correlation_data1 <- Reduce(Merge, list(SDS_of_interest, APQ_P_of_interest, APQ_SR_of_interest, CELF_of_interest, CCSC_of_interest,
                                       Demos_of_interest, Barratt_of_interest, Physical_of_interest, IAT_of_interest, PCIAT_of_interest))
write.csv(Correlation_data1, "Correlation_data1.csv", row.names = FALSE)

# create dataframe for correlation - Correlation #2 (Dimensional + Substance Use)
Correlation_data2 <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, ASSQ_of_interest, AUDIT_of_interest,
                                        Conners_of_interest, SWAN_of_interest, CBCL_of_interest ,
                                        YSR_of_interest, Demos_of_interest, ESPAD_of_interest, Nicotine_of_interest, YFAS_of_interest, 
                                        SCARED_SR_of_interest, SCARED_P_of_interest, MFQ_SR_of_interest,
                                        MFQ_P_of_interest))


write.csv(Correlation_data2, "Correlation_data2.csv", row.names = FALSE)
#################### new code from Lindsay ###########

library(psych)

###### Select columns to be included and correlate variables
correlationvars2 <- Correlation_data2[,-1]
rvalues2 <- cor(correlationvars2, use="pairwise.complete.obs")
#get P and N values for correlations
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
png(file="Correlation_2.png", width = 1200, height = 1000)
corrplot(rvalues2, p.mat = pvalues2, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper")
dev.off()

####################### end of new code #########################

# create CELF % differences in scores where positive scores are at or above criterion and negative are below
CELF$Percent_Difference <- ifelse(CELF$CELF_ExceedCutoff == 0, -(abs(CELF$CELF_CriterionScore - CELF$CELF_Total)/((CELF$CELF_CriterionScore + CELF$CELF_Total)/2)*100), abs(CELF$CELF_CriterionScore - CELF$CELF_Total)/((CELF$CELF_CriterionScore + CELF$CELF_Total)/2)*100)

# pull out variables of interest
CELF_of_interest <- data.frame(
  ID = CELF$ï..URSI,
  Percent_Difference = CELF$Percent_Difference)

APQ_P_of_interest <- data.frame(
  ID = APQ_P$ï..URSI, 
  APQ_P[,-(1:43)])

APQ_SR_of_interest <- data.frame(
  ID = APQ_SR$ï..URSI,
  APQ_SR[,-(1:52)])

CCSC_of_interest <- data.frame(
  ID = CCSC$ï..URSI,
  CCSC[, -(1:57)])

Demos_of_interest <- data.frame(
  ID = Basic_Demos$ï..URSI,
  Sex = Basic_Demos$Sex,
  Age = floor(Basic_Demos$Age))

Barratt_of_interest <- data.frame(
  ID = Barratt$ï..URSI,
  Maternal_Education = Barratt$Barratt_P1_Edu,
  SES = Barratt$Barratt_Total)

SDS_of_interest <- data.frame(
  ID = SDS$ï..URSI,
  SDS[,c(29, 31, 33, 35, 37, 39, 41)])

Physical_of_interest <- data.frame(
  ID = Physical$ï..URSI,
  BMI = Physical$BMI)

IAT_of_interest <- data.frame(
  ID = IAT$ï..URSI,
  IAT$IAT_Total)

PCIAT_of_interest <- data.frame(
  ID = PCIAT$ï..URSI,
  PCIAT$PCIAT_Total)

# create merge function
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

# create dataframe for correlation
Correlation_data <- Reduce(Merge, list(SDS_of_interest, APQ_P_of_interest, APQ_SR_of_interest, CELF_of_interest, CCSC_of_interest,
                                       Demos_of_interest, Barratt_of_interest, Physical_of_interest, IAT_of_interest, PCIAT_of_interest))
write.csv(Correlation_data, "Correlation_data.csv", row.names = FALSE)

#create correlation matrix
cor(na.omit(Correlation_data[,-1]))
library(corrplot)
forcorrplot <- cor(na.omit(Correlation_data[,-1]))
corrplot(forcorrplot)
corrplot(forcorrplot, type = "upper", method = "color", order = "hclust")

#save correlation matrix
png(file="Correlation_1.png", width = 1000, height = 800)
corrplot(forcorrplot, type = "upper", method = "color", order = "hclust")
dev.off()


###### Select columns to be included and correlate variables
library(psych)
correlationvars <- Correlation_data[,-1]
#correlationvars$Age<-as.numeric(correlationvars$Age)
rvalues <- cor(correlationvars, use="pairwise.complete.obs")
#get P and N values for correlations
significance <- corr.test(correlationvars, adjust="fdr")
pvalues <- significance$p
nvalues <- significance$n
# Set plot parameters, color, and labels
par(mfrow=c(1,1))
col4 <- colorRampPalette(c("red","#FF7F00","yellow", "white",
                           "cyan", "#007FFF","#00007F"))  
rownames(rvalues) <- c("Disorders of initiating and maintaining sleep T-Score", "Sleep Breathing Disorders T-Score", "Disorders of arousal T-Score",
                       "Sleep-Wake Transition Disorders T-Score", "Disorders of excessive somnolence T-Score", "Sleep hyperhydrosis T-Score",
                       "SDS Total T-Score", "Involvement Score: Parent", "Positive Parenting Score: Parent", "Poor Monitoring/Supervision Score: Parent", 
                       "Other Parental Discipline: Parent",	"Inconsistent Discipline Score: Parent", "Corporal Punishment Score: Parent",
                       "APQ Total Score: Score", "Mother Involvement Score: Self", "Father Involvement Score: Self", "Positive Parenting Score: Self",
                       "Poor Monitoring/Supervision Score: Self", "Inconsistent Discipline Score: Self", "Corporal Punishment Score: Self",
                       "Other Parental Discipline: Self", "APQ Total Score: Self", "CELF	Percent Difference", "Problem Focused Coping Score",
                       "Cognitive Decision Making Score", "Direct Problem Solving Score", "Seeking Understanding Score",	"Avoidance Coping Score",
                       "Avoidant Actions Score",	"Repression Score",	"Wishful Thinking Score",	"Positive Cognitive Restructuring Score",	
                       "Control Score", "Optimism Score", "Positivity Score", "Religion Score", "Support Seeking Score", "Support Sought from Mother/Father/Guardian Score",
                       "Support Sought from Other Adults Score", "Support Sought from Peers Score", "Support Sought from Siblings Score",
                       "Sex", "Age", "Maternal Education", "SES", "BMI", "Internet Addiction: Self-Report",	"Internet Addiction: Parent-Report")

#rownames(rvalues) <- c("Disorders of initiating and maintaining sleep T-Score", "Sleep Breathing Disorders T-Score", "Disorders of arousal T-Score",
#"Sleep-Wake Transition Disorders T-Score", "Disorders of excessive somnolence T-Score", "Sleep hyperhydrosis T-Score",
#                      "SDS Total T-Score", "Involvement Score: Parent", "Positive Parenting Score: Parent", "Poor Monitoring/Supervision Score: Parent", 
#                      "Other Parental Discipline: Parent",	"Inconsistent Discipline Score: Parent", "Corporal Punishment Score: Parent",
#                      "APQ Total Score: Score", "Mother Involvement Score: Self", "Father Involvement Score: Self", "Positive Parenting Score: Self",
#                      "Poor Monitoring/Supervision Score: Self", "Inconsistent Discipline Score: Self", "Corporal Punishment Score: Self",
#                     "Other Parental Discipline: Self", "APQ Total Score: Self", "CELF	Percent Difference", "Problem Focused Coping Score",
#                     "Cognitive Decision Making Score", "Direct Problem Solving Score", "Seeking Understanding Score",	"Avoidance Coping Score",
#                     "Avoidant Actions Score",	"Repression Score",	"Wishful Thinking Score",	"Positive Cognitive Restructuring Score",	
#                     "Control Score", "Optimism Score", "Positivity Score", "Religion Score", "Support Seeking Score", "Support Sought from Mother/Father/Guardian Score",
#                     "Support Sought from Other Adults Score", "Support Sought from Peers Score", "Support Sought from Siblings Score",
#                     "Sex", "Age", "Maternal Education", "SES", "BMI", "Internet Addiction: Self-Report",	"Internet Addiction: Parent-Report")?

##PLOT CORRELATION MATRIX
#colnames(rvalues) <- c(1:48)
library(corrplot)
corrplot(rvalues, p.mat = pvalues, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper", tl.cex =2, tl.pos = "d")

png(file="Correlation_1.png", width = 1200, height = 1000)
corrplot(rvalues, p.mat = pvalues, method="color", insig = "blank",
         addrect=2, col=rev(col4(100)), mar=c(1, 1, 1, 1) + 0.1,
         tl.col="black", tl.srt=60, type="upper", tl.cex =1, diag = FALSE)
dev.off()

################################ Scatterplot of total scores vs. factor scores from CFA #####################
IAT_no_missing$IAT_Total <- rowSums(IAT_no_missing[2:21])

library(psych)

# for IAT
png(file="IAT_Scores_scatterplot", width = 1200, height = 1000)
plot(IAT_no_missing$IAT_Total, IAT_no_missing$IAT_fscores, main="Self-Report IAT total score vs. factor score", 
     xlab="IAT Total Sum Score ", ylab="IAT Factor Score")
abline(lm(IAT_no_missing$IAT_fscores ~ IAT_no_missing$IAT_Total), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_no_missing$IAT_fscores ~ IAT_no_missing$IAT_Total))$adj.r.squared, digits=4)))
dev.off()

# for PCIAT
png(file="PCIAT_Scores_scatterplot", width = 1200, height = 1000)
plot(PCIAT_no_missing$PCIAT_Total, PCIAT_no_missing$PCIAT_fscores, main="Parent-Report IAT total score vs. factor score", 
     xlab="PCIAT Total Sum Score ", ylab="PCIAT Factor Score")
abline(lm(PCIAT_no_missing$PCIAT_fscores ~ PCIAT_no_missing$PCIAT_Total), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(PCIAT_no_missing$PCIAT_fscores ~ PCIAT_no_missing$PCIAT_Total))$adj.r.squared, digits=4)))
dev.off()
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

#turn NAs into zeros within IAT and PCIAT dataframes (based on the scoring from original csv file from Lindsay)
IAT[is.na(IAT)] <- 0
PCIAT[is.na(PCIAT)] <- 0

# recompute total sum scores due to errors in original csv file
IAT$IAT_Total <- rowSums(IAT[,2:21])
PCIAT$PCIAT_Total <- rowSums(PCIAT[,2:21])

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

CCSC_of_interest <- data.frame(
  ID = CCSC$URSI,
  Problem_Focused_Coping = CCSC$CCSC_PFC,
  Avoidance_Coping = CCSC$CCSC_AC,
  Positive_Cognitive_Restructuring = CCSC$CCSC_PCR,
  Religion = CCSC$CCSC_REL,
  Support_Seeking = CCSC$CCSC_SS)

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

# combine into dataframe for regression

Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

Reg_APQ_CCSC <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, Demos_of_interest, Barratt_of_interest, APQ_P_of_interest, APQ_SR_of_interest,
                                   CCSC_of_interest))
Reg_APQ_CCSC <- na.omit(Reg_APQ_CCSC)

# new dataframe with only APQ Parent stuff
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

Reg_APQ_P <- Reduce(Merge, list(PCIAT_of_interest, Demos_of_interest, Barratt_of_interest, APQ_P_of_interest))
Reg_APQ_P <- na.omit(Reg_APQ_P)

# New data frame with only APQ SR stuff
Reg_APQ_SR <- Reduce(Merge, list(IAT_of_interest, Demos_of_interest, Barratt_of_interest, APQ_SR_of_interest))
Reg_APQ_SR <- na.omit(Reg_APQ_SR)

# new dataframe with only CCSC stuff
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

Reg_CCSC <- Reduce(Merge, list(IAT_of_interest, PCIAT_of_interest, Demos_of_interest, Barratt_of_interest, CCSC_of_interest))

# Regression with subscales of APQ_P predicting PCIAT
lm1<-lm(IAT_Parent ~ Involvement_P + Positive_Parenting_P + Poor_Monitoring_P + Inconsistent_Discipline_P +
          Corporal_Punishment_P + Sex + Age + SES + Site, data = Reg_APQ_CCSC)
summary(lm1)
# Regression with P positive and negative composites predicting PCIAT
lm1<-lm(IAT_Parent ~ Positive_Parenting_Composite_P +
          Negative_Parenting_Composite_P + Sex + Age + SES + Site, data = Reg_APQ_CCSC)
summary(lm1)

# Regression with subscales of APQ_SR predicting IAT
lm1<-lm(IAT_SR ~ Involvement_Mother_SR + Involvement_Father_SR + Positive_Parenting_SR + Poor_Monitoring_SR + Inconsistent_Discipline_SR +
          Corporal_Punishment_SR + Sex + Age + SES + Site, data = Reg_APQ_CCSC)
summary(lm1)
# Regression with SR positive and negative composites predicting IAT
lm1<-lm(IAT_SR ~ Positive_Parenting_Composite_SR +
          Negative_Parenting_Composite_SR + Sex + Age + SES + Site, data = Reg_APQ_CCSC)
summary(lm1)


# Regression with overarching subscales of CCSC predicting IAT_SR
lm1<-lm(IAT_SR ~ Problem_Focused_Coping + Avoidance_Coping + Positive_Cognitive_Restructuring + Religion + Support_Seeking +
          Sex + Age + SES + Site, data = Reg_APQ_CCSC)
summary(lm1)

# Regression with overarching subscales of CCSC predicting PCIAT
lm1<-lm(IAT_Parent ~ Problem_Focused_Coping + Avoidance_Coping + Positive_Cognitive_Restructuring + Religion + Support_Seeking +
          Sex + Age + SES + Site, data = Reg_APQ_CCSC)
summary(lm1)

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

################### Create dataframes ##################

#turn NAs into zeros within IAT and PCIAT dataframes (based on the scoring from original csv file from Lindsay)
IAT[is.na(IAT)] <- 0
PCIAT[is.na(PCIAT)] <- 0

# recompute total sum scores due to errors in original csv file
IAT$IAT_Total <- rowSums(IAT[,2:21])
PCIAT$PCIAT_Total <- rowSums(PCIAT[,2:21])

# pull out values of interest 
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

ASSQ_of_interest <- data.frame(
  ID = ASSQ$URSI,
  ASSQ_Total = ASSQ$ASSQ_Total)

Conners_of_interest <- data.frame(
  ID = Conners$URSI,
  Conners_Hyperactivity_Impulsivity = Conners$C3SR_HY_T,
  Conners_Inattention = Conners$C3SR_IN_T,
  Conners_Learning_Problems = Conners$C3SR_LP_T)

SWAN_of_interest <- data.frame(
  ID = SWAN$F,
  SWAN_Inattentive = SWAN$SWAN_IN,
  SWAN_Hyperactive = SWAN$SWAN_HY,
  SWAN_Total = SWAN$SWAN_Total)

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

# merge data frames of interest
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

# data frame for consensus dxs (binary)
Logistic_Regression <- Reduce(Merge, list(Dx_of_interest, IAT_of_interest,
                                          PCIAT_of_interest, Demos_of_interest, Barratt_of_interest))
#remove missing data
Logistic_Regression <- na.omit(Logistic_Regression)

# data frames for questionnaires (dimensional)
linear_regression <- Reduce(Merge, list(IAT_of_interest,
                                        PCIAT_of_interest, ASSQ_of_interest, 
                                        Conners_of_interest, SWAN_of_interest, SCARED_SR_of_interest, SCARED_P_of_interest,
                                        MFQ_SR_of_interest, MFQ_P_of_interest, Demos_of_interest, Barratt_of_interest))


################## Linear regressions using consensus dxs to predict total sum scores ##################
# run regressions for total sum score of IAT and PCIAT
Reg_Dx_SR<-lm(IAT_SR ~ ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
          Social_Anxiety + Sex + Age + SES + Site, data = Logistic_Regression)
summary(Reg_Dx_SR)

Reg_Dx_P<-lm(IAT_Parent ~ ASD + Learning_Disorder + Anxiety + Depression + ADHD_Combined + ADHD_Inattentive + ADHD_Hyperactive +
          Social_Anxiety + Sex + Age + SES + Site, data = Logistic_Regression)
summary(Reg_Dx_P)

##################### Multiple Linear Regression with questionnaires Predicting IAt scores #####################
# IAT with only self-report q's as predcictors
lm1<-lm(IAT_SR ~ Conners_Hyperactivity_Impulsivity + Conners_Inattention + Conners_Learning_Problems + 
          Social_Anxiety_SR + Total_Anxiety_SR +
          Depression_SR + Sex + Age + SES + Site, data = linear_regression)
summary(lm1)

#PCIAT with only parent-report q's as predictors
lm1<-lm(IAT_Parent ~ ASSQ_Total +
          SWAN_Inattentive + SWAN_Hyperactive + SWAN_Total + Social_Anxiety_P + Total_Anxiety_P +
          Depression_P + Sex + Age + SES + Site, data = linear_regression)
summary(lm1)

################## Multiple linear regressions with IAT predicting Questionnaire scores spearated by SR and parent-report #######
#Self-Report Rgression
lm1<-lm(Conners_Hyperactivity_Impulsivity + Conners_Inattention + Conners_Learning_Problems + 
          Social_Anxiety_SR + Total_Anxiety_SR + Depression_SR ~ IAT_Total + Sex + Age + SES + Site, data = linear_regression)
summary(lm1)

lm1<-lm(ASSQ_Total + SWAN_Inattentive + SWAN_Hyperactive + Social_Anxiety_P + Total_Anxiety_P +
         Depression_P ~ PCIAT_Total + Sex + Age + SES + Site, data = linear_regression)
summary(lm1)

########################## logistic regression with all dxs as outcome variables ##########
#cast all outcome variables (dxs) as factors to try and make glmer function work -->> it did not -.-
Dx_of_interest$ASD <- factor(Dx_of_interest$ASD)
Dx_of_interest$Learning_Disorder  <- factor(Dx_of_interest$Learning_Disorder)
Dx_of_interest$Anxiety <- factor(Dx_of_interest$Anxiety)
Dx_of_interest$Depression <- factor(Dx_of_interest$Depression)
Dx_of_interest$ADHD_Combined <- factor(Dx_of_interest$ADHD_Combined)
Dx_of_interest$ADHD_Inattentive <- factor(Dx_of_interest$ADHD_Inattentive)
Dx_of_interest$ADHD_Hyperactive <- factor(Dx_of_interest$ADHD_Hyperactive)
Dx_of_interest$Social_Anxiety <- factor(Dx_of_interest$Social_Anxiety)

library(lme4)
# IAT score as predictor of all dxs
lm1 <- glmer(cbind(ASD, Learning_Disorder, Anxiety, Depression, ADHD_Combined, ADHD_Inattentive,
                 ADHD_Hyperactive, Social_Anxiety) ~ (1|IAT_Total) + (1| Sex) + (1 | Age) + (1 | Site), family= "binomial",data = Logistic_Regression)

# PCIAT score as predictor of all dxs
lm1 <- glm(cbind(ASD, Learning_Disorder, Anxiety, Depression, ADHD_Combined, ADHD_Inattentive,
                 ADHD_Hyperactive, Social_Anxiety) ~ PCIAT_Total + Sex + Age + Site, family= "binomial",data = Logistic_Regression)


#try running indidividual logistic regressions -->> does NOT take into account common variance -.-
regressors <- names(Logistic_Regression[,2:9])
lapply(regressors,function(name){glm(Logistic_Regression[,name] ~ Logistic_Regression$IAT_Total + Logistic_Regression$Age +
                                       Logistic_Regression$Sex + Logistic_Regression$Site, na.action=na.exclude,
                                     family=binomial(link= "logit"))})
lapply(regressors,function(name){summary(glm(Logistic_Regression[,name] ~ Logistic_Regression$IAT_Total + Logistic_Regression$Age +
                                               Logistic_Regression$Sex + Logistic_Regression$Site, na.action=na.exclude,
                                             family=binomial(link= "logit")))})
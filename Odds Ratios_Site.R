#set working directory
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/Project")
#turn on library
library(base)

#################### Import all data ##############
temp = list.files(pattern = "*csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

############################################# Odds Ratios #######################

# scores greater than or equal to 40 on IAT and PCIAT are considered PIU (as per Kim et al. paper)

#making new dataframe by coding each participant as either severe (1) or non-severe (0) for IAT and PCIAT using normal total scores
Categorical_IAT <- data.frame(ID = IAT$URSI, Problematic = ifelse (IAT$IAT_Total >=50, 1, 0))
Categorical_PCIAT <- data.frame(ID = PCIAT$URSI, Problematic = ifelse (PCIAT$PCIAT_Total >=50, 1, 0))

#make new dataframes with each participant as severe (1) or no-severe(0) based on CFA total scores
IAT_no_missing <- na.omit(IAT)
PCIAT_no_missing <- na.omit(PCIAT)

IAT_no_missing$IAT_fscores <- predict(fit_IAT)
PCIAT_no_missing$PCIAT_fscores <- predict(fit_PCIAT)

Categorical_IAT <- data.frame(
  ID = IAT_no_missing$ID,
  Problematic = ifelse (IAT_no_missing$IAT_fscores >= ((40-26)/17.49), 1, 0))

Categorical_PCIAT <- data.frame(
  ID = PCIAT_no_missing$ID, 
  Problematic = ifelse (PCIAT_no_missing$PCIAT_fscores >= ((40-25.75)/20.36), 1, 0))

# stupid R
names(Categorical_IAT)[names(Categorical_IAT) == 'F1'] <- 'Problematic'
names(Categorical_PCIAT)[names(Categorical_PCIAT) == 'F1'] <- 'Problematic'

# adding columns for each cluster (0 = not in cluster, 1= yes in cluster)
IAT_Regression$ASD_Cluster <- ifelse(Sub_Group = "x", 1, 0)

#remove participants who dropped the study
ConsensusDx <- ConsensusDx[!(ConsensusDx$NoDX == 3),]


# making new dataframe by coding each diagnosis as either 0 (no) or 1 (yes) ------>>> if get error message, it is because URSI is screwed up
Dx_of_interest <- data.frame(
  ID = ConsensusDx$URSI,
  ASD = ifelse ((ConsensusDx$DX_01_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_02_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_03_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_04_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_05_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_06_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_07_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_08_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_09_Sub == "Autism Spectrum Disorder" | ConsensusDx$DX_10_Sub == "Autism Spectrum Disorder"), 1, 0),
  Learning_Disorder = ifelse (ConsensusDx$DX_01_Sub == "Specific Learning Disorder" | ConsensusDx$DX_02_Sub == "Specific Learning Disorder" | ConsensusDx$DX_03_Sub == "Specific Learning Disorder" | ConsensusDx$DX_04_Sub == "Specific Learning Disorder" | ConsensusDx$DX_05_Sub == "Specific Learning Disorder" | ConsensusDx$DX_06_Sub == "Specific Learning Disorder" | ConsensusDx$DX_07_Sub == "Specific Learning Disorder" | ConsensusDx$DX_08_Sub == "Specific Learning Disorder" | ConsensusDx$DX_09_Sub == "Specific Learning Disorder" | ConsensusDx$DX_10_Sub == "Specific Learning Disorder", 1, 0),
  Anxiety = ifelse (ConsensusDx$DX_01_Cat == "Anxiety Disorders" | ConsensusDx$DX_02_Cat == "Anxiety Disorders" | ConsensusDx$DX_03_Cat == "Anxiety Disorders" | ConsensusDx$DX_04_Cat == "Anxiety Disorders" | ConsensusDx$DX_05_Cat == "Anxiety Disorders" | ConsensusDx$DX_06_Cat == "Anxiety Disorders" | ConsensusDx$DX_07_Cat == "Anxiety Disorders" | ConsensusDx$DX_08_Cat == "Anxiety Disorders" | ConsensusDx$DX_09_Cat == "Anxiety Disorders" | ConsensusDx$DX_10_Cat == "Anxiety Disorders", 1, 0),
  Depression = ifelse (ConsensusDx$DX_01_Cat == "Depressive Disorders" | ConsensusDx$DX_02_Cat == "Depressive Disorders" | ConsensusDx$DX_03_Cat == "Depressive Disorders" | ConsensusDx$DX_04_Cat == "Depressive Disorders" | ConsensusDx$DX_05_Cat == "Depressive Disorders" | ConsensusDx$DX_06_Cat == "Depressive Disorders" | ConsensusDx$DX_07_Cat == "Depressive Disorders" | ConsensusDx$DX_08_Cat == "Depressive Disorders" | ConsensusDx$DX_09_Cat == "Depressive Disorders" | ConsensusDx$DX_10_Cat == "Depressive Disorders", 1, 0),
  ADHD_Combined = ifelse (ConsensusDx$DX_01 == "ADHD-Combined Type" | ConsensusDx$DX_02 == "ADHD-Combined Type" | ConsensusDx$DX_03 == "ADHD-Combined Type" | ConsensusDx$DX_04 == "ADHD-Combined Type" | ConsensusDx$DX_05 == "ADHD-Combined Type" | ConsensusDx$DX_06 == "ADHD-Combined Type" | ConsensusDx$DX_07 == "ADHD-Combined Type" | ConsensusDx$DX_08 == "ADHD-Combined Type" | ConsensusDx$DX_09 == "ADHD-Combined Type" | ConsensusDx$DX_10 == "ADHD-Combined Type", 1, 0),
  ADHD_Inattentive = ifelse (ConsensusDx$DX_01 == "ADHD-Inattentive Type" | ConsensusDx$DX_02 == "ADHD-Inattentive Type" | ConsensusDx$DX_03 == "ADHD-Inattentive Type" | ConsensusDx$DX_04 == "ADHD-Inattentive Type" | ConsensusDx$DX_05 == "ADHD-Inattentive Type" | ConsensusDx$DX_06 == "ADHD-Inattentive Type" | ConsensusDx$DX_07 == "ADHD-Inattentive Type" | ConsensusDx$DX_08 == "ADHD-Inattentive Type" | ConsensusDx$DX_09 == "ADHD-Inattentive Type" | ConsensusDx$DX_10 == "ADHD-Inattentive Type", 1, 0),
  ADHD_Hyperactive = ifelse (ConsensusDx$DX_01 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_02 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_03 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_04 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_05 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_06 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_07 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_08 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_09 == "ADHD-Hyperactive/Impulsive Type" | ConsensusDx$DX_10 == "ADHD-Hyperactive/Impulsive Type", 1, 0),
  Social_Anxiety = ifelse (ConsensusDx$DX_01 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_02 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_03 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_04 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_05 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_06 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_07 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_08 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_09 == "Social Anxiety (Social Phobia)" | ConsensusDx$DX_10 == "Social Anxiety (Social Phobia)", 1, 0)
)

#add new column for comorbidity count
ConsensusDx$Dx1 <- ifelse (ConsensusDx$DX_01_Cat !="" , 1, 0)
ConsensusDx$Dx2 <- ifelse (ConsensusDx$DX_02_Cat !="" , 1, 0)
ConsensusDx$Dx3 <- ifelse (ConsensusDx$DX_03_Cat !="" , 1, 0)
ConsensusDx$Dx4 <- ifelse (ConsensusDx$DX_04_Cat !="" , 1, 0)
ConsensusDx$Dx5 <- ifelse (ConsensusDx$DX_05_Cat !="" , 1, 0)
ConsensusDx$Dx6 <- ifelse (ConsensusDx$DX_06_Cat !="" , 1, 0)
ConsensusDx$Dx7 <- ifelse (ConsensusDx$DX_07_Cat !="" , 1, 0)
ConsensusDx$Dx8 <- ifelse (ConsensusDx$DX_08_Cat !="" , 1, 0)
ConsensusDx$Dx9 <- ifelse (ConsensusDx$DX_09_Cat !="" , 1, 0)
ConsensusDx$Dx10 <- ifelse (ConsensusDx$DX_10_Cat !="" , 1, 0)

Dx_of_interest$Comorbidites <- rowSums(ConsensusDx[,113:120, 122])


# save file and turn all na into 0
write.csv(Dx_of_interest, "Dx_of_interest.csv", row.names = FALSE, na="0")

#read in new Dx_of_interest
Dx_of_interest <- read.csv("Dx_of_interest.csv")

#create site info dataframe
Site_Info <- data.frame(
  ID = Basic_Demos$URSI,
  Site = Basic_Demos$Study_Site)

# create merge function
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

# merge dataframes with DX of interest for PCIAT and IAT respectively
IAT_Dx <- Reduce(Merge, list(Categorical_IAT, Dx_of_interest, Site_Info))
PCIAT_Dx <- Reduce(Merge, list(Categorical_PCIAT, Dx_of_interest, Site_Info))

# split dataframes into Midtown and Staten Island
x <- split.data.frame(IAT_Dx, IAT_Dx$Site)
str(x)

names(x) <- c("IAT_Dx_SI", "IAT_Dx_MRV", "IAT_Dx_MT")
list2env(x, envir = .GlobalEnv)

y <- split.data.frame(PCIAT_Dx, PCIAT_Dx$Site)
names(y) <- c("PCIAT_Dx_SI", "PCIAT_Dx_MRV", "PCIAT_Dx_MT")
list2env(y, envir = .GlobalEnv)

#calculate odds ratios for each Dx for IAT for different sites
#ASD
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#Learning Disorder
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#anxiety
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#depression
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#ADHD-C
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#ADHD-H -- too few (don't use)
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#ADHD-I
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$ADHD_Inattentive)
data_table
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$ADHD_Inattentive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#Social Anxiety
data_table <- table(IAT_Dx_SI$Problematic, IAT_Dx_SI$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(IAT_Dx_MT$Problematic, IAT_Dx_MT$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#calculate odds ratios for each Dx for PCIAT for diff. sites

#ASD
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#Learning Disorder
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#anxiety
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#depression
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#ADHD-C
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#ADHD-I
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$ADHD_Inattentive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$ADHD_Inattentive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#ADHD-H
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

#Social Anxiety
data_table <- table(PCIAT_Dx_SI$Problematic, PCIAT_Dx_SI$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

data_table <- table(PCIAT_Dx_MT$Problematic, PCIAT_Dx_MT$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

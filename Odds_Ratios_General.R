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
Categorical_IAT <- data.frame(ID = IAT$URSI, Problematic = ifelse (IAT$IAT_Total >=80, 1, 0))
Categorical_PCIAT <- data.frame(ID = PCIAT$URSI, Problematic = ifelse (PCIAT$PCIAT_Total >=90, 1, 0))
# create merge function
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

# merge dataframes with DX of interest for PCIAT and IAT respectively
IAT_Dx <- Reduce(Merge, list(Categorical_IAT, Dx_of_interest))
PCIAT_Dx <- Reduce(Merge, list(Categorical_PCIAT, Dx_of_interest))

#calculate odds ratios for each Dx for IAT
#ASD
data_table <- table(IAT_Dx$Problematic, IAT_Dx$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#Learning Disorder
data_table <- table(IAT_Dx$Problematic, IAT_Dx$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#anxiety
data_table <- table(IAT_Dx$Problematic, IAT_Dx$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#depression
data_table <- table(IAT_Dx$Problematic, IAT_Dx$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#ADHD-C
data_table <- table(IAT_Dx$Problematic, IAT_Dx$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#ADHD-H
data_table <- table(IAT_Dx$Problematic, IAT_Dx$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#ADHD-I
data_table <- table(IAT_Dx$Problematic, IAT_Dx$ADHD_Inattentive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)



#Social Anxiety
data_table <- table(IAT_Dx$Problematic, IAT_Dx$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)



#calculate odds ratios for each Dx for PCIAT

#ASD
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)



#Learning Disorder
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#anxiety
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#depression
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#ADHD-C
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)



#ADHD-I
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$ADHD_Inattentive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)



#ADHD-H
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)



#Social Anxiety
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)



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


############### ensure total scores are correctly computed for PCIAT and IAT #############

IAT$IAT_Total <- rowSums(IAT[2:21])
PCIAT$PCIAT_Total <- rowSums(PCIAT[2:21])

#add ID column
IAT$ID <- IAT$URSI
PCIAT$ID <- PCIAT$URSI

############################################# Odds Ratios where parent and child agree on problematic #######################

# scores greater than or equal to 40 on IAT and PCIAT are considered PIU (as per Kim et al. paper)

#making new columns by coding each participant as either problematic (1) or 
#non-problematic (0) for IAT and PCIAT using normal total scores
IAT$IAT_Problematic <- ifelse (IAT$IAT_Total >= 40, 1, 0)
PCIAT$PCIAT_Problematic <- ifelse (PCIAT$PCIAT_Total >= 40, 1, 0)
# create merge function
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
# merge dataframes with DX of interest for PCIAT and IAT respectively
IAT_PCIAT_Dx <- Reduce(Merge, list(IAT[, c(22, 23, 24)], PCIAT[, c(22, 23, 24)], Dx_of_interest))

# save file
write.csv(IAT_PCIAT_Dx, "IAT_PCIAT_Dx.csv", row.names = FALSE)

# multiply vectors of PCIAT_Problematic and IAt_Problematic to get total_problematic for when child and parent agree
IAT_PCIAT_Dx$Agreed_Problematic <- IAT_PCIAT_Dx$IAT_Problematic*IAT_PCIAT_Dx$PCIAT_Problematic

#calculate odds ratios for each Dx for parent and child agreed upon problematic
#ASD
data_table <- table(IAT_PCIAT_Dx$Agreed_Problematic, IAT_PCIAT_Dx$ASD)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#Learning Disorder
data_table <- table(IAT_PCIAT_Dx$Agreed_Problematic, IAT_PCIAT_Dx$Learning_Disorder)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#anxiety
data_table <- table(IAT_PCIAT_Dx$Agreed_Problematic, IAT_PCIAT_Dx$Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#depression
data_table <- table(IAT_PCIAT_Dx$Agreed_Problematic, IAT_PCIAT_Dx$Depression)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#ADHD-C
data_table <- table(IAT_PCIAT_Dx$Agreed_Problematic, IAT_PCIAT_Dx$ADHD_Combined)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#ADHD-H
data_table <- table(IAT_PCIAT_Dx$Agreed_Problematic, IAT_PCIAT_Dx$ADHD_Hyperactive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)


#ADHD-I
data_table <- table(IAT_PCIAT_Dx$Agreed_Problematic, IAT_PCIAT_Dx$ADHD_Inattentive)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)



#Social Anxiety
data_table <- table(IAT_PCIAT_Dx$Agreed_Problematic, IAT_PCIAT_Dx$Social_Anxiety)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)




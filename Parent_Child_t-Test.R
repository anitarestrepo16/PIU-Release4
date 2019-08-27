#set working directory
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/Project")
#turn on library
library(base)
#################### Import all data ##############
temp = list.files(pattern = "*csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
################## Clean data #########################

#remove participants with no data
PCIAT <- PCIAT[-c(75), ]
MFQ_SR <- MFQ_SR[-c(30), ]

#turn NAs into zeros only for IAT and PCIAT because of scoring errors.
IAT[is.na(IAT)] <- 0
PCIAT[is.na(PCIAT)] <- 0

#rescore PCIAT and IAT
IAT$IAT_Total <- rowSums(IAT[2:21])
PCIAT$PCIAT_Total <- rowSums(PCIAT[2:21])

####### Boxplot for comparisons
#pull out data of interest
IAT_of_interest <- data.frame(
  ID = IAT$URSI,
  IAT_SR = IAT$IAT_Total)

PCIAT_of_interest <- data.frame(
  ID = PCIAT$URSI,
  IAT_Parent = PCIAT$PCIAT_Total)

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

#dataframe for comparison of differentials
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

Parent_Child_Comparisons <- Reduce(Merge, list(IAT_of_interest,
                                               PCIAT_of_interest, SCARED_SR_of_interest, SCARED_P_of_interest,
                                               MFQ_SR_of_interest, MFQ_P_of_interest, APQ_P_of_interest, APQ_SR_of_interest))

#compute differential scores (parent minus child for all)
Parent_Child_Comparisons$IAT_Delta <- Parent_Child_Comparisons$IAT_Parent - Parent_Child_Comparisons$IAT_SR

Parent_Child_Comparisons$Total_Anxiety_Delta <- Parent_Child_Comparisons$Total_Anxiety_P - 
  Parent_Child_Comparisons$Total_Anxiety_SR

Parent_Child_Comparisons$Depression_Delta <- Parent_Child_Comparisons$Depression_P - 
  Parent_Child_Comparisons$Depression_SR

Parent_Child_Comparisons$Positive_Parenting_Composite_Delta <- Parent_Child_Comparisons$Positive_Parenting_Composite_P - 
  Parent_Child_Comparisons$Positive_Parenting_Composite_SR

Parent_Child_Comparisons$Negative_Parenting_Composite_Delta <- Parent_Child_Comparisons$Negative_Parenting_Composite_P - 
  Parent_Child_Comparisons$Negative_Parenting_Composite_SR

#Parent_Child_Comparisons$Positive_Parenting_Delta <- Parent_Child_Comparisons$Positive_Parenting_P - 
#  Parent_Child_Comparisons$Positive_Parenting_SR
#
#Parent_Child_Comparisons$Poor_Monitoring_Delta <- Parent_Child_Comparisons$Poor_Monitoring_P - 
#  Parent_Child_Comparisons$Poor_Monitoring_SR
#
#Parent_Child_Comparisons$Inconsistent_Discipline_Delta <- Parent_Child_Comparisons$Inconsistent_Discipline_P - 
#  Parent_Child_Comparisons$Inconsistent_Discipline_SR
#
#Parent_Child_Comparisons$Corporal_Punishment_Delta <- Parent_Child_Comparisons$Corporal_Punishment_P - 
#  Parent_Child_Comparisons$Corporal_Punishment_SR


#############t-tests
#IAT
t.test(Parent_Child_Comparisons$IAT_SR, Parent_Child_Comparisons$IAT_Parent, paired = TRUE, alternative = "two.sided")
#MFQ (depression)
t.test(Parent_Child_Comparisons$Depression_SR, Parent_Child_Comparisons$Depression_P, paired = TRUE, alternative = "two.sided")
#SCARED (Anxiety)
t.test(Parent_Child_Comparisons$Total_Anxiety_SR, Parent_Child_Comparisons$Total_Anxiety_P, paired = TRUE, alternative = "two.sided")
#APQ (Positive)
t.test(Parent_Child_Comparisons$Positive_Parenting_Composite_SR, Parent_Child_Comparisons$Positive_Parenting_Composite_P, paired = TRUE, alternative = "two.sided")
#APQ (Negative)
t.test(Parent_Child_Comparisons$Negative_Parenting_Composite_SR, Parent_Child_Comparisons$Negative_Parenting_Composite_P, paired = TRUE, alternative = "two.sided")

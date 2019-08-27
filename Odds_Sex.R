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
Categorical_IAT <- data.frame(ID = IAT$URSI, Problematic = ifelse (IAT$IAT_Total >=40, 1, 0))
Categorical_PCIAT <- data.frame(ID = PCIAT$URSI, Problematic = ifelse (PCIAT$PCIAT_Total >=40, 1, 0))

#add sex
Demos_of_interest <- data.frame(
  ID = Basic_Demos$URSI,
  Sex = Basic_Demos$Sex)

# create merge function
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

# merge dataframes with DX of interest for PCIAT and IAT respectively
IAT_Dx <- Reduce(Merge, list(Categorical_IAT, Demos_of_interest, Dx_of_interest))
PCIAT_Dx <- Reduce(Merge, list(Categorical_PCIAT, Demos_of_interest, Dx_of_interest))

# Odds ratios for sex with SR IAT
data_table <- table(IAT_Dx$Problematic, IAT_Dx$Sex)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

# Odds ratios for sex with PCIAT
data_table <- table(PCIAT_Dx$Problematic, PCIAT_Dx$Sex)
data_table
library(epiR)
epi.2by2(data_table, method="cohort.count", conf.level=0.95)

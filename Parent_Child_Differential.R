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

#################################### calculate differential based on problematic vs. non-problematic #############

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

# Make new column with direction of parent-child discrepancy:
# -1 = child reported problematic/ parent reported non-problematic
# 0 = child and parent agree
# 1 = child reported non-problematic/ parent reported problematic

IAT_PCIAT_Dx$Discrepancy <- ifelse (IAT_PCIAT_Dx$IAT_Problematic > IAT_PCIAT_Dx$PCIAT_Problematic, -1,
                                    ifelse (IAT_PCIAT_Dx$IAT_Problematic < IAT_PCIAT_Dx$PCIAT_Problematic, 1,  0))

#create and save histogram of Number of -1, 0 and 1
png(file="Parent_Child_Discrepancy.png", width = 1000, height = 800)
Parent_Child_Discrepancy <- hist(IAT_PCIAT_Dx$Discrepancy,
                    main = "Histogram of Direction of Parent-Child Discrepancy (Problematic vs. Non-Problematic)",
                    xlab = "Direction of Discrepancy",
                    ylab = "Number of Participants",
                    xlim = c(-1, 1),
                    ylim = c(0, 500),
                    border = "dark blue",
                    col = "light blue",
                    labels = TRUE)
dev.off()


############################ Calculate Differential Based on Total Sum Score ####################

# Make new column with direction of parent-child discrepancy:
# -1 = IAT > PCIAT
# 0 = IAT = PCIAT
# 1 = IAT < PCIAT

IAT_PCIAT_Dx$Discrepancy_Score <- ifelse (IAT_PCIAT_Dx$IAT_Total > IAT_PCIAT_Dx$PCIAT_Total, -1,
                                    ifelse (IAT_PCIAT_Dx$IAT_Total < IAT_PCIAT_Dx$PCIAT_Total, 1,  0))

#create and save histogram of Number of -1, 0 and 1
png(file="Parent_Child_Discrepancy_Score.png", width = 1000, height = 800)
Parent_Child_Discrepancy_Score <- hist(IAT_PCIAT_Dx$Discrepancy_Score,
                                 main = "Histogram of Direction of Parent-Child Discrepancy (Total Score)",
                                 xlab = "Direction of Discrepancy",
                                 ylab = "Number of Participants",
                                 xlim = c(-1, 1),
                                 ylim = c(0, 500),
                                 border = "dark blue",
                                 col = "light blue",
                                 labels = TRUE)
dev.off()

#set working directory
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/Project")
#turn on library
library(base)

############### ensure total scores are correctly computed for PCIAT and IAT #############

IAT_no_missing$IAT_Total <- rowSums(IAT_no_missing[2:21])
PCIAT_no_missing$PCIAT_Total <- rowSums(PCIAT_no_missing[2:21])

#create data frame with only participants who have both PCIAT and IAT totals scores
# create merge function
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}


# create dataframe for correlation - Correlation #1
IAT_and_PCIAT <- Reduce(Merge, list(IAT_no_missing, PCIAT_no_missing))
IAT_and_PCIAT <- na.omit(IAT_and_PCIAT)
write.csv(IAT_and_PCIAT, "IAT_and_PCIAT.csv", row.names = FALSE)

################# Create Scatterplot ###################

library(psych)

png(file="IAT_PCIAT_Scatterplot.png", width = 1200, height = 1000)
plot(IAT_and_PCIAT$IAT_Total, IAT_and_PCIAT$PCIAT_Total, main="Self-Report Vs. Parent-Report IAT Total Scores", 
     xlab="Self-Report IAT Total Score ", ylab="Parent-Report IAT Total Score")
abline(lm(IAT_and_PCIAT$PCIAT_Total ~ IAT_and_PCIAT$IAT_Total), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_and_PCIAT$PCIAT_Total ~ IAT_and_PCIAT$IAT_Total))$adj.r.squared, digits=4)))
dev.off()

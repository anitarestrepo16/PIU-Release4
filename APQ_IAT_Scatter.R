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

############### ensure total scores are correctly computed for PCIAT and IAT #############

IAT_no_missing$IAT_Total <- rowSums(IAT_no_missing[2:21])
PCIAT_no_missing$PCIAT_Total <- rowSums(PCIAT_no_missing[2:21])

#create data frame with only participants who have both PCIAT and IAT totals scores
# create merge function
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}

# pull out variables of interest
APQ_P_of_interest <- data.frame(
  ID = APQ_P$URSI,
  P_Involvement = APQ_P$APQ_P_INV,
  P_Positive_Parenting = APQ_P$APQ_P_PP,
  P_Poor_Monitoring = APQ_P$APQ_P_PM,
  P_Inconsistent_Discipline = APQ_P$APQ_P_ID,
  P_Corporal_Punishment = APQ_P$APQ_P_CP)


APQ_SR_of_interest <- data.frame(
  ID = APQ_SR$URSI,
  SR_Involvement_Mother = APQ_SR$APQ_SR_INV_M,
  SR_Involvement_Father = APQ_SR$APQ_SR_INV_D,
  SR_Positive_Parenting = APQ_SR$APQ_SR_PP,
  SR_Poor_Monitoring = APQ_SR$APQ_SR_PM,
  SR_Inconsistent_Discipline = APQ_SR$APQ_SR_ID,
  SR_Corporal_Punishment = APQ_SR$APQ_SR_CP)

# create dataframe for correlation
IAT_APQ <- Reduce(Merge, list(IAT_no_missing, PCIAT_no_missing, APQ_P_of_interest, APQ_SR_of_interest))
IAT_APQ <- na.omit(IAT_APQ)
write.csv(IAT_APQ, "IAT_APQ.csv", row.names = FALSE)

################# Create Scatterplots ###################

library(psych)

#Inconsistent Discipline

#self-report
png(file="SR_IAT_Inconsistent_Discipline_Scatterplot.png", width = 1200, height = 1000)
plot(IAT_APQ$IAT_Total, IAT_APQ$SR_Inconsistent_Discipline, main="SR: IAT Vs. Inconsistent Discipline", 
     xlab="Self-Report IAT Total Score ", ylab="Self-Report Inconsistent Discipline Score")
abline(lm(IAT_APQ$IAT_Total ~ IAT_APQ$SR_Inconsistent_Discipline), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_APQ$IAT_Total ~ IAT_APQ$SR_Inconsistent_Discipline))$adj.r.squared, digits=4)))
dev.off()

#parent report
png(file="P_IAT_Inconsistent_Discipline_Scatterplot.png", width = 1200, height = 1000)
plot(IAT_APQ$PCIAT_Total, IAT_APQ$P_Inconsistent_Discipline, main="Parent: IAT Vs. Inconsistent Discipline", 
     xlab="Parent-Report IAT Total Score ", ylab="Parent-Report Inconsistent Discipline Score")
abline(lm(IAT_APQ$PCIAT_Total ~ IAT_APQ$P_Inconsistent_Discipline), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_APQ$PCIAT_Total ~ IAT_APQ$P_Inconsistent_Discipline))$adj.r.squared, digits=4)))
dev.off()

#Involvement

#self-report - father
png(file="SR_IAT_Father_Involvement_Scatterplot.png", width = 1200, height = 1000)
plot(IAT_APQ$IAT_Total, IAT_APQ$SR_Involvement_Father, main="SR: IAT Vs. Involvement (Father)", 
     xlab="Self-Report IAT Total Score ", ylab="Self-Report Father Involvement Score")
abline(lm(IAT_APQ$IAT_Total ~ IAT_APQ$SR_Involvement_Father), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_APQ$IAT_Total ~ IAT_APQ$SR_Involvement_Father))$adj.r.squared, digits=4)))
dev.off()

#self-report - mother
png(file="SR_IAT_Mother_Involvement_Scatterplot.png", width = 1200, height = 1000)
plot(IAT_APQ$IAT_Total, IAT_APQ$SR_Involvement_Mother, main="SR: IAT Vs. Involvement (Mother)", 
     xlab="Self-Report IAT Total Score ", ylab="Self-Report Mother Involvement Score")
abline(lm(IAT_APQ$IAT_Total ~ IAT_APQ$SR_Involvement_Mother), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_APQ$IAT_Total ~ IAT_APQ$SR_Involvement_Mother))$adj.r.squared, digits=4)))
dev.off()

#parent report
png(file="P_IAT_Involvement_Scatterplot.png", width = 1200, height = 1000)
plot(IAT_APQ$PCIAT_Total, IAT_APQ$P_Involvement, main="Parent: IAT Vs. Involvement", 
     xlab="Parent-Report IAT Total Score ", ylab="Parent-Report Involvement Score")
abline(lm(IAT_APQ$PCIAT_Total ~ IAT_APQ$P_Involvement), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_APQ$PCIAT_Total ~ IAT_APQ$P_Involvement))$adj.r.squared, digits=4)))
dev.off()

# Positive Parenting

#self-report
png(file="SR_IAT_Positive_Parenting_Scatterplot.png", width = 1200, height = 1000)
plot(IAT_APQ$IAT_Total, IAT_APQ$SR_Positive_Parenting, main="SR: IAT Vs. Positive Parenting", 
     xlab="Self-Report IAT Total Score ", ylab="Self-Report Positive Parenting Score")
abline(lm(IAT_APQ$IAT_Total ~ IAT_APQ$SR_Positive_Parenting), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_APQ$IAT_Total ~ IAT_APQ$SR_Positive_Parenting))$adj.r.squared, digits=4)))
dev.off()

#parent report
png(file="P_IAT_Positive_Parenting_Scatterplot.png", width = 1200, height = 1000)
plot(IAT_APQ$PCIAT_Total, IAT_APQ$P_Positive_Parenting, main="Parent: IAT Vs. Positive Parenting", 
     xlab="Parent-Report IAT Total Score ", ylab="Parent-Report Positive Parenting Score")
abline(lm(IAT_APQ$PCIAT_Total ~ IAT_APQ$P_Positive_Parenting), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_APQ$PCIAT_Total ~ IAT_APQ$P_Positive_Parenting))$adj.r.squared, digits=4)))
dev.off()

# Poor Monitoring

#self-report
png(file="SR_IAT_Poor_Monitoring_Scatterplot.png", width = 1200, height = 1000)
plot(IAT_APQ$IAT_Total, IAT_APQ$SR_Poor_Monitoring, main="SR: IAT Vs. Poor_Monitoring", 
     xlab="Self-Report IAT Total Score ", ylab="Self-Report Poor_Monitoring Score")
abline(lm(IAT_APQ$IAT_Total ~ IAT_APQ$SR_Poor_Monitoring), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_APQ$IAT_Total ~ IAT_APQ$SR_Poor_Monitoring))$adj.r.squared, digits=4)))
dev.off()

#parent report
png(file="P_IAT_Poor_Monitoring_Scatterplot.png", width = 1200, height = 1000)
plot(IAT_APQ$PCIAT_Total, IAT_APQ$P_Poor_Monitoring, main="Parent: IAT Vs. Poor_Monitoring", 
     xlab="Parent-Report IAT Total Score ", ylab="Parent-Report Poor_Monitoring Score")
abline(lm(IAT_APQ$PCIAT_Total ~ IAT_APQ$P_Poor_Monitoring), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_APQ$PCIAT_Total ~ IAT_APQ$P_Poor_Monitoring))$adj.r.squared, digits=4)))
dev.off()

# Corporal Punishment

#self-report
png(file="SR_IAT_Corporal_Punishment_Scatterplot.png", width = 1200, height = 1000)
plot(IAT_APQ$IAT_Total, IAT_APQ$SR_Corporal_Punishment, main="SR: IAT Vs. Corporal_Punishment", 
     xlab="Self-Report IAT Total Score ", ylab="Self-Report Corporal_Punishment Score")
abline(lm(IAT_APQ$IAT_Total ~ IAT_APQ$SR_Corporal_Punishment), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_APQ$IAT_Total ~ IAT_APQ$SR_Corporal_Punishment))$adj.r.squared, digits=4)))
dev.off()

#parent report
png(file="P_IAT_Corporal_Punishment_Scatterplot.png", width = 1200, height = 1000)
plot(IAT_APQ$PCIAT_Total, IAT_APQ$P_Corporal_Punishment, main="Parent: IAT Vs. Corporal_Punishment", 
     xlab="Parent-Report IAT Total Score ", ylab="Parent-Report Corporal_Punishment Score")
abline(lm(IAT_APQ$PCIAT_Total ~ IAT_APQ$P_Corporal_Punishment), col="red") # regression line (y~x)
legend("topright", bty="n", legend=paste("R2 =", format(summary(lm(IAT_APQ$PCIAT_Total ~ IAT_APQ$P_Corporal_Punishment))$adj.r.squared, digits=4)))
dev.off()

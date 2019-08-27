#set working directory
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/R6 Project")
#turn on library
library(base)
#################### Import all data ##############
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/R6 Project/Data")
temp = list.files(pattern = "*csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)
################## Clean data #########################

#remove participants with no data
Physical <- Physical[complete.cases(Physical$BMI), ]
PCIAT <- PCIAT[-c(958, 1132, 1491), ]
C3SR <- C3SR[-c(653, 1163), ]
FTND <- FTND[-c(58), ]
MFQ_P <- MFQ_P[-c(918, 1296), ]
MFQ_SR <- MFQ_SR[-c(918), ]
SCARED_SR <- SCARED_SR[-c(727), ]
YFAS_C <- YFAS_C[-c(425,450, 1346), ]

#turn NAs into zeros only for IAT and PCIAT because of scoring errors.
IAT[is.na(IAT)] <- 0
PCIAT[is.na(PCIAT)] <- 0

#rescore PCIAT and IAT
IAT$IAT_Total <- rowSums(IAT[2:21])
PCIAT$PCIAT_Total <- rowSums(PCIAT[2:21])

#create dummy variable for 1 caregiver only based on Barratt responses
Barratt$No_C2 <- ifelse(is.na(Barratt$Barratt_P2_Edu) & 
                          is.na(Barratt$Barratt_P2_Occ), 1, 0)

########### make df with intersection of all samples -> final sample (NOT including dimensional measures of psychoapthology) #########
#pull out data of interest (Dx_of_interest already created above)
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

Race <- data.frame(
  ID = PreInt_Demos_Fam$URSI,
  Race = PreInt_Demos_Fam$Child_Race)

Barratt_of_interest <- data.frame(
  ID = Barratt$URSI,
  SES = Barratt$Barratt_Total,
  Single_Caregiver = Barratt$No_C2)

Physical_of_interest <- data.frame(
  ID = Physical$URSI,
  BMI = Physical$BMI)

BIA_of_interest <- data.frame(
  ID = BIA$URSI,
  FMI = BIA$FMI)

PAQ_A_of_interest <- data.frame(
  ID = PAQ_A$URSI,
  PAQ_Total = PAQ_A$PAQ_A_Total)

PAQ_C_of_interest <- data.frame(
  ID = PAQ_C$URSI,
  PAQ_Total = PAQ_C$PAQ_C_Total)

SDS_of_interest <- data.frame(
  ID = SDS$URSI,
  SDS_Total = SDS$SDS_Total_T)

CIS_P_of_interest <- data.frame(
  ID = CIS_P$URSI,
  CIS_P_Score = CIS_P$CIS_P_Score)

CIS_SR_of_interest <- data.frame(
  ID = CIS_SR$URSI,
  CIS_SR_Score = CIS_SR$CIS_SR_Total)

#merge PAQ dataframes
PAQ_of_interest <- rbind(PAQ_A_of_interest, PAQ_C_of_interest)

#construct final sample df for both Parent and SR
Merge <- function(x, y){
  df <- merge(x, y, by= "ID", all.x=TRUE, all.y=TRUE)
  return(df)
}
Standard_Sample_SR <- Reduce(Merge, list(IAT_of_interest,
                                         Demos_of_interest, Race, Barratt_of_interest, 
                                         Physical_of_interest, 
                                         BIA_of_interest, PAQ_of_interest, SDS_of_interest, 
                                         CIS_SR_of_interest, Dx_of_interest))
Standard_Sample_P <- Reduce(Merge, list(PCIAT_of_interest,
                                        Demos_of_interest, Race, Barratt_of_interest,
                                        Physical_of_interest, BIA_of_interest, PAQ_of_interest, 
                                        SDS_of_interest, CIS_P_of_interest, Dx_of_interest))
#remove all Na's
Standard_Sample_SR <- Standard_Sample_SR[complete.cases(Standard_Sample_SR), ]
Standard_Sample_P <- Standard_Sample_P[complete.cases(Standard_Sample_P), ]

#add IAT and PCIAT individual items 
Standard_Sample_SR_Full <- merge(Standard_Sample_SR, IAT_Full, by.y = "ID", all.y = FALSE, all.x = FALSE)
Standard_Sample_P_Full <- merge(Standard_Sample_P, PCIAT_Full, by.y = "ID", all.y = FALSE, all.x = FALSE)

############# Histograms for each questionnaire -- x = # of missing questions ###########
######### Full sample
#### IAT
#Count Number of NAs per row
IAT$IAT_Missing <- rowSums(is.na(IAT))
#create and save histogram of IAT Missing
png(file="IAT_Missing_Hist.png", width = 1000, height = 800)
par(cex = 1.7)
hist(IAT$IAT_Missing,
                      main = "Histogram of IAT Missing",
                      xlab = "Number Of Items Missing",
                      ylab = "Number of Participants",
                      xlim = c(0, 5),
                      ylim = c(0, 1000),
                      border = "dark blue",
                      col = "light blue",
                    labels = TRUE)
dev.off()
### PCIAT
#Count Number of NAs per row
PCIAT$PCIAT_Missing <- rowSums(is.na(PCIAT))
#create and save histogram of PCIAT Missing
png(file="PCIAT_Missing_Hist.png", width = 1000, height = 800)
par(cex = 1.7)
hist(PCIAT$PCIAT_Missing,
                    main = "Histogram of PCIAT Missing",
                    xlab = "Number Of Participants",
                    ylab = "Number of Items Missing",
                    xlim = c(0, 5),
                    ylim = c(0, 1000),
                    border = "dark blue",
                    col = "light blue",
                    labels = TRUE)
dev.off()

########## Final Standard Sample
#set WD
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/R6 Project/Figures")
#### IAT
#change URSI to ID
colnames(IAT)[1] <- "ID"
#add IAT items back in
IAT_Standard <- merge(Standard_Sample_SR[,1:2], IAT[, 1:21], by.y = "ID", all = FALSE)
#Count Number of NAs per row
IAT_Standard$IAT_Missing <- rowSums(is.na(IAT_Standard[, 3:22]))
#create and save histogram of IAT Missing
png(file="IAT_Missing_Hist.png", width = 1000, height = 800)
par(cex = 1.7)
hist(Standard_Sample_SR_Full$IAT_Missing,
     main = "Histogram of IAT Missing",
     xlab = "Number Of Items Missing",
     ylab = "Number of Participants",
     xlim = c(0, 5),
     ylim = c(0, 1000),
     border = "dark blue",
     col = "light blue",
     labels = TRUE)
dev.off()

### PCIAT
#change URSI to ID
colnames(PCIAT)[1] <- "ID"
#add IAT items back in
PCIAT_Standard <- merge(Standard_Sample_P[,1:2], PCIAT[, 1:21], by.y = "ID", all = FALSE)
#Count Number of NAs per row
PCIAT_Standard$PCIAT_Missing <- rowSums(is.na(PCIAT_Standard[, 3:22]))

#create and save histogram of PCIAT Missing
png(file="PCIAT_Missing_Hist.png", width = 1000, height = 800)
par(cex = 1.7)
hist(Standard_Sample_P_Full$PCIAT_Missing,
     main = "Histogram of PCIAT Missing",
     xlab = "Number Of Participants",
     ylab = "Number of Items Missing",
     xlim = c(0, 5),
     ylim = c(0, 1000),
     border = "dark blue",
     col = "light blue",
     labels = TRUE)
dev.off()

#### SDS
#add SDS individual items
colnames(SDS)[1] <- "ID"
Standard_SDS <- merge(Standard_Sample_SR[, 1:2], SDS[, 1:27] , by.y = "ID", all.y = FALSE, all.x = FALSE)
#Count Number of NAs per row
Standard_SDS$SDS_Missing <- rowSums(is.na(Standard_SDS[, 3:28]))
#create and save histogram of IAT Missing
png(file="SDS_Missing_Hist.png", width = 1000, height = 800)
par(cex = 2)
hist(Standard_Sample_P_Full$SDS_Missing,
     main = "Histogram of SDS Missing",
     xlab = "Number Of Items Missing",
     ylab = "Number of Participants",
     xlim = c(0, 5),
     ylim = c(0, 10),
     border = "dark blue",
     col = "light blue",
     labels = TRUE)
dev.off()

#### CIS
#add CIS individual items
colnames(CIS_P)[1] <- "ID"
colnames(CIS_SR)[1] <- "ID"
Standard_CIS_P <- merge(Standard_Sample_P[, 1:2], CIS_P[, 1:14], by.y = "ID", all = FALSE)
Standard_CIS_SR <- merge(Standard_Sample_SR[, 1:2], CIS_SR[, 1:14], by.y = "ID", all.y = FALSE, all.x = FALSE)
#Count Number of NAs per row
Standard_CIS_SR$CIS_Missing <- rowSums(is.na(Standard_CIS_SR[, 3:15]))
Standard_CIS_P$CIS_Missing <- rowSums(is.na(Standard_CIS_P[, 3:15]))
#create and save histogram of CIS Missing
png(file="CIS_SR_Missing_Hist.png", width = 1000, height = 800)
par(cex = 2)
hist(Standard_Sample_SR_Full$CIS_Missing,
     main = "Histogram of CIS SR Missing",
     xlab = "Number Of Items Missing",
     ylab = "Number of Participants",
     xlim = c(0, 5),
     ylim = c(0, 10),
     border = "dark blue",
     col = "light blue",
     labels = TRUE)
dev.off()
png(file="CIS_P_Missing_Hist.png", width = 1000, height = 800)
par(cex = 2)
hist(Standard_Sample_P_Full$CIS_Missing,
     main = "Histogram of CIS P Missing",
     xlab = "Number Of Items Missing",
     ylab = "Number of Participants",
     xlim = c(0, 5),
     ylim = c(0, 10),
     border = "dark blue",
     col = "light blue",
     labels = TRUE)
dev.off()

#### PAQ
#change URSI to ID
colnames(PAQ_A)[1] <- "ID"
colnames(PAQ_C)[1] <- "ID"
#Count Number of NAs per row
PAQ_C$PAQ_Missing <- rowSums(is.na(PAQ_C[, 29:44]))
PAQ_A$PAQ_Missing <- rowSums(is.na(PAQ_A[, 29:42]))
#create df with missing for both PAQs
Merge <- function(x, y){
  df <- merge(x, y, by.y = "ID", all.x=TRUE, all.y=FALSE)
  return(df)
}

Standard_PAQ_A <- Reduce(Merge, list(Standard_Sample_SR[, 1:2], PAQ_A[1:309, c(1, 44)]))
Standard_PAQ_C <- Reduce(Merge, list(Standard_Sample_SR[, 1:2], PAQ_C[1:918, c(1, 46)]))
Standard_PAQ <- rbind(Standard_PAQ_A, Standard_PAQ_C)
Standard_PAQ <- Standard_PAQ[complete.cases(Standard_PAQ),]

#create and save histogram of PAQ Missing
png(file="PAQ_Missing_Hist.png", width = 1000, height = 800)
par(cex = 2)
hist(Standard_Sample_SR_Full$PAQ_Missing,
     main = "Histogram of PAQ Missing",
     xlab = "Number Of Items Missing",
     ylab = "Number of Participants",
     xlim = c(0, 10),
     ylim = c(0, 20),
     border = "dark blue",
     col = "light blue",
     labels = TRUE)
dev.off()

#### Barratt
#Add Barratt items
colnames(Barratt)[1] <- "ID"
Standard_Sample_P_Full <- merge(Standard_Sample_P, Barratt[, -c(2, 5, 8, 9)], by.y = "ID", all = FALSE)

#Count Number of NAs per row
Standard_Sample_P_Full$Barratt_Missing <- rowSums(is.na(Standard_Sample_P_Full[, 21:24]))
#create and save histogram of Barratt Missing
png(file="Barratt_Missing_Hist.png", width = 1000, height = 800)
par(cex = 2)
hist(Standard_Sample_P_Full$Barratt_Missing,
     main = "Histogram of Barratt Missing",
     xlab = "Number Of Items Missing",
     ylab = "Number of Participants",
     xlim = c(0, 5),
     ylim = c(0, 80),
     border = "dark blue",
     col = "light blue",
     labels = TRUE)
dev.off()

############## Scatterplots for responses by missing
####SR
png(file = "IAT_Missing_Scatter.png", width = 1000, height = 800)
par(cex = 1.7)
plot(Standard_Sample_SR_Full$IAT_Missing, Standard_Sample_SR_Full$IAT_SR,
     main = "IAT Scores by Number Missing",
     xlab = "Number Of Missing Questions",
     ylab = "IAT Total Score")
dev.off()

####P
png(file = "PCIAT_Missing_Scatter.png", width = 1000, height = 800)
par(cex = 1.7)
plot(Standard_Sample_P_Full$PCIAT_Missing, Standard_Sample_P_Full$IAT_Parent,
     main = "PCIAT Scores by Number Missing",
     xlab = "Number Of Missing Questions",
     ylab = "PCIAT Total Score")
dev.off()

############# Distribution of Scores for people with missing items
#Index rows with missing data
Only_Missing_SR <- Standard_Sample_SR_Full[rowSums(is.na(Standard_Sample_SR_Full))!=0,]
Only_Missing_P <- Standard_Sample_P_Full[rowSums(is.na(Standard_Sample_P_Full))!=0,]
#SR
png(file="IAT_Missing_Hist.png", width = 1000, height = 800)
par(cex = 1.7)
hist(Only_Missing_SR$IAT_SR,
     main = "Distribution of IAT Scores with Missing Values",
     xlab = "IAT Scores",
     ylab = "Number of Participants",
     xlim = c(0, 100),
     ylim = c(0, 5),
     border = "dark blue",
     col = "light blue",
     labels = TRUE)
dev.off()
#P
png(file="PCIAT_Missing_Hist.png", width = 1000, height = 800)
par(cex = 1.7)
hist(Only_Missing_P$IAT_Parent,
     main = "Distribution of PCIAT Scores with Missing Values",
     xlab = "PCIAT Scores",
     ylab = "Number of Participants",
     xlim = c(0, 100),
     ylim = c(0, 5),
     border = "dark blue",
     col = "light blue",
     labels = TRUE)
dev.off()

### APQ SR

#Count Number of NAs per row
APQ_SR$APQ_SR_Missing <- rowSums(is.na(APQ_SR))
#create and save histogram of PCIAT Missing
png(file="APQ_SR_Missing.png", width = 1000, height = 800)
APQ_SR_Missing <- hist(APQ_SR$APQ_SR_Missing,
                      main = "Histogram of APQ SR Missing",
                      xlab = "Number Of Participants",
                      ylab = "Number of Items Missing",
                      xlim = c(0, 20),
                      ylim = c(0, 1300),
                      border = "dark blue",
                      col = "light blue",
                      labels = TRUE)
dev.off()

### APQ P

#Count Number of NAs per row
APQ_P$APQ_P_Missing <- rowSums(is.na(APQ_P))
#create and save histogram of PCIAT Missing
png(file="APQ_P_Missing.png", width = 1000, height = 800)
APQ_P_Missing <- hist(APQ_P$APQ_P_Missing,
                       main = "Histogram of APQ P Missing",
                       xlab = "Number Of Participants",
                       ylab = "Number of Items Missing",
                       xlim = c(0, 5),
                       ylim = c(0, 1350),
                       border = "dark blue",
                       col = "light blue",
                       labels = TRUE)
dev.off()

### CCSC

#Count Number of NAs per row
CCSC$CCSC_Missing <- rowSums(is.na(CCSC))
#create and save histogram of PCIAT Missing
png(file="CCSC_Missing.png", width = 1000, height = 800)
CCSC_Missing <- hist(CCSC$CCSC_Missing,
                      main = "Histogram of CCSC Missing",
                      xlab = "Number Of Participants",
                      ylab = "Number of Items Missing",
                      xlim = c(0, 14),
                      ylim = c(0, 900),
                      border = "dark blue",
                      col = "light blue",
                      labels = TRUE)
dev.off()

### SWAN

#Count Number of NAs per row
SWAN$SWAN_Missing <- rowSums(is.na(SWAN))
#create and save histogram of PCIAT Missing
png(file="SWAN_Missing.png", width = 1000, height = 800)
SWAN_Missing <- hist(SWAN$SWAN_Missing,
                     main = "Histogram of SWAN Missing",
                     xlab = "Number Of Participants",
                     ylab = "Number of Items Missing",
                     xlim = c(0, 10),
                     ylim = c(0, 1350),
                     border = "dark blue",
                     col = "light blue",
                     labels = TRUE)
dev.off()

### Conners

#Count Number of NAs per row
C3SR$CONNERS_Missing <- rowSums(is.na(C3SR))
#create and save histogram of PCIAT Missing
png(file="CONNERS_Missing.png", width = 1000, height = 800)
CONNERS_Missing <- hist(C3SR$CONNERS_Missing,
                     main = "Histogram of CONNERS Missing",
                     xlab = "Number Of Participants",
                     ylab = "Number of Items Missing",
                     xlim = c(0, 15),
                     ylim = c(0, 1000),
                     border = "dark blue",
                     col = "light blue",
                     labels = TRUE)
dev.off()

### MFQ SR

MFQ_SR <- MFQ_SR[-c(30), ]
#Count Number of NAs per row
MFQ_SR$MFQ_SR_Missing <- rowSums(is.na(MFQ_SR))
#create and save histogram of PCIAT Missing
png(file="MFQ_SR_Missing.png", width = 1000, height = 800)
MFQ_SR_Missing <- hist(MFQ_SR$MFQ_SR_Missing,
                        main = "Histogram of MFQ_SR Missing",
                        xlab = "Number Of Participants",
                        ylab = "Number of Items Missing",
                        xlim = c(0, 3),
                        ylim = c(0, 900),
                        border = "dark blue",
                        col = "light blue",
                        labels = TRUE)
dev.off()

### MFQ P

#Count Number of NAs per row
MFQ_P$MFQ_P_Missing <- rowSums(is.na(MFQ_P))
#create and save histogram of PCIAT Missing
png(file="MFQ_P_Missing.png", width = 1000, height = 800)
MFQ_P_Missing <- hist(MFQ_P$MFQ_P_Missing,
                       main = "Histogram of MFQ_P Missing",
                       xlab = "Number Of Participants",
                       ylab = "Number of Items Missing",
                       xlim = c(0, 10),
                       ylim = c(0, 950),
                       border = "dark blue",
                       col = "light blue",
                       labels = TRUE)
dev.off()

### SCARED_SR

#Count Number of NAs per row
SCARED_SR$SCARED_SR_Missing <- rowSums(is.na(SCARED_SR))
#create and save histogram of PCIAT Missing
png(file="SCARED_SR_Missing.png", width = 1000, height = 800)
SCARED_SR_Missing <- hist(SCARED_SR$SCARED_SR_Missing,
                      main = "Histogram of SCARED_SR Missing",
                      xlab = "Number Of Participants",
                      ylab = "Number of Items Missing",
                      xlim = c(0, 3),
                      ylim = c(0, 950),
                      border = "dark blue",
                      col = "light blue",
                      labels = TRUE)
dev.off()

### SCARED_p

#Count Number of NAs per row
SCARED_P$SCARED_P_Missing <- rowSums(is.na(SCARED_P))
#create and save histogram of PCIAT Missing
png(file="SCARED_P_Missing.png", width = 1000, height = 800)
SCARED_P_Missing <- hist(SCARED_P$SCARED_P_Missing,
                          main = "Histogram of SCARED_P Missing",
                          xlab = "Number Of Participants",
                          ylab = "Number of Items Missing",
                          xlim = c(0, 11),
                          ylim = c(0, 1150),
                          border = "dark blue",
                          col = "light blue",
                          labels = TRUE)
dev.off()

### ASSQ

#Count Number of NAs per row
ASSQ$ASSQ_Missing <- rowSums(is.na(ASSQ))
#create and save histogram of PCIAT Missing
png(file="ASSQ_Missing.png", width = 1000, height = 800)
ASSQ_Missing <- hist(ASSQ$ASSQ_Missing,
                         main = "Histogram of ASSQ Missing",
                         xlab = "Number Of Participants",
                         ylab = "Number of Items Missing",
                         xlim = c(0, 4),
                         ylim = c(0, 1400),
                         border = "dark blue",
                         col = "light blue",
                         labels = TRUE)
dev.off()

### BARRATT

#Count Number of NAs per row
Barratt$Barratt_Missing <- rowSums(is.na(Barratt))
#create and save histogram of PCIAT Missing
png(file="Barratt_Missing.png", width = 1000, height = 800)
Barratt_Missing <- hist(Barratt$Barratt_Missing,
                     main = "Histogram of Barratt Missing",
                     xlab = "Number Of Participants",
                     ylab = "Number of Items Missing",
                     xlim = c(0, 3),
                     ylim = c(0, 1400),
                     border = "dark blue",
                     col = "light blue",
                     labels = TRUE)
dev.off()

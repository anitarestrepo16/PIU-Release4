#set working directory
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/Project")
#turn on library
library(base)

#################### Import all data ##############
temp = list.files(pattern = "*csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

################################### Clustering ###############################
### Processing Speed Biclustering
### Bonhwang Koo
### 7/19/2018

install.packages("viridis")
install.packages("fmsb")
install.packages("ggplot2")
install.packages("reshape")
install.packages("klaR")

library(psych)
library(viridis)
library(fmsb)
library(cluster)
library(ggplot2)
library(reshape)
library(klaR)



dimensional <- c("CBCL_Int_T", "CBCL_Ext_T", "MFQ_P_Total_AgeSex_Ctrl", 
                 "MFQ_SR_Total_AgeSex_Ctrl", "SCARED_P_Total_AgeSex_Ctrl", "SCARED_SR_Total_AgeSex_Ctrl", 
                 "ARI_P_Total_Score_AgeSex_Ctrl", "ARI_S_Total_Score_AgeSex_Ctrl", "SWAN_IN_AgeSex_Ctrl", 
                 "SWAN_HY_AgeSex_Ctrl", "ASD_latent_factor", "PC1")
categorical <- c("ADHD_I", "ADHD_H", "ADHD_C", "ASD", "DMDD", 
                 "GAD", "Learning", "Anx_Soc", "Anx_All", "Depression", 
                 "ASD_ADHD_I", "ASD_ADHD_H", "ASD_ADHD_C", "ASD_ADHD_All", "ASD_noADHD")
categorical_short <- c("ADHD_I", "ADHD_C", "ASD", "Learning", "Anx_All", "Depression")

df_clust <- read.csv("Data Cleaned/Biclustering_Data.csv", stringsAsFactors = FALSE)
df_clust_all <- df_clust[, c("EID", dimensional, categorical_short)]
df_clust_asmt <- df_clust[, c("EID", dimensional)]

dfs <- list(df_clust_all, df_clust_asmt)
names(dfs) <- c("all", "asmt")

## znorm
## Apply z-score normalization to a numeric vector
znorm <- function(x) {
  (x-min(x))/(max(x)-min(x))
}

## znorm
## Apply z-score normalization to a numeric vector
ranknorm <- function(x) {
  rank(x)/length(x)
}

for (i in 1:length(dfs)) {
  df <- dfs[[i]]
  # Remove EID column and append as row name
  EID <- df$EID
  df <- df[,-1]
  # Centers and scales data.
  #df <- scale(df)
  df <- apply(df, 2, znorm)
  #df <- apply(df, 2, ranknorm)
  rownames(df) <- EID
  # Cluster rows by 
  r_dist <- dist(df, method = "euclidean")
  hr <- hclust(r_dist, method="ward.D2")
  # Cluster columns 
  c_dist <- dist(t(df), method = "euclidean")
  hc <- hclust(c_dist, method="ward.D2")
  
  png(paste0("Plots/Bicluster-znorm/Trees/row_clusters_", names(dfs)[i], ".png"))
  plot(hr, labels = FALSE)
  dev.off()
  
  png(paste0("Plots/Bicluster-znorm/Trees/col_clusters_", names(dfs)[i], ".png"))
  plot(hc)
  dev.off()
} 

# cluster_k_row_values <- list(all = c(3, 4),
#                          asmt = c(3, 5, 8),
#                          dx = c(3, 4))
# 
# cluster_k_col_values <- list(all = c(3, 4, 7),
#                              asmt = c(3, 4, 5),
#                              dx = c(3, 5, 6))

cluster_k_row_values <- list(all = c(3, 4, 5),
                             asmt = c(3, 4, 5))

cluster_k_col_values <- list(all = c(3, 4, 6),
                             asmt = c(3, 4, 5))


# cluster_k_row_values <- list(all = c(3, 5, 7),
#                              asmt = c(3, 5, 6))
# 
# cluster_k_col_values <- list(all = c(3, 4, 7),
#                              asmt = c(3, 4, 5))

silhouette_values <- data.frame(variables = character(), axis = character(), k = numeric(), avg_sil_width = numeric(),
                                med_sil_width = numeric(), min_sil_width = numeric(), max_sil_width = numeric(), 
                                mean_clust_size = numeric(), stringsAsFactors = FALSE)

for (i in 1:length(dfs)) {
  df <- dfs[[i]]
  # Remove EID column and append as row name
  EID <- df$EID
  df <- df[,-1]
  # Centers and scales data.
  #df <- scale(df)
  df <- apply(df, 2, znorm)
  #df <- apply(df, 2, ranknorm)
  rownames(df) <- EID
  # Cluster rows by 
  r_dist <- dist(df, method = "euclidean")
  hr <- hclust(r_dist, method="ward.D2")
  # Cluster columns 
  c_dist <- dist(t(df), method = "euclidean")
  hc <- hclust(c_dist, method="ward.D2")
  for (j in 1:length(cluster_k_row_values[[i]])) {
    k_row <- cluster_k_row_values[[i]][j]
    mycl <- cutree(hr, k = k_row)
    
    png(paste0("Plots/Bicluster-znorm/Silhouette/silhouette_", names(dfs)[i], "_row_k", k_row, ".png"))
    sil_r <- silhouette(mycl, r_dist)
    plot(sil_r)
    dev.off()
    
    sil_r_summary <- summary(sil_r)
    
    silhouette_values <- rbind(silhouette_values, 
                               data.frame(variables = names(dfs)[i], 
                                          axis = "row", 
                                          k = k_row, 
                                          avg_sil_width = mean(sil_r[, "sil_width"]),
                                          med_sil_width = median(sil_r[, "sil_width"]),
                                          min_sil_width = min(sil_r[, "sil_width"]),
                                          max_sil_width = max(sil_r[, "sil_width"]),
                                          mean_clust_size = mean(sil_r_summary$clus.sizes)))
  }
  
  for (j in 1:length(cluster_k_col_values[[i]])) {
    k_col <- cluster_k_col_values[[i]][j]
    mycl2 <- cutree(hc, k = k_col)
    
    png(paste0("Plots/Bicluster-znorm/Silhouette/silhouette_", names(dfs)[i], "_col_k", k_col, ".png"))
    sil_c <- silhouette(mycl2, c_dist)
    plot(sil_c)
    dev.off()
    
    sil_c_summary <- summary(sil_c)
    
    silhouette_values <- rbind(silhouette_values, 
                               data.frame(variables = names(dfs)[i], 
                                          axis = "col", 
                                          k = k_col, 
                                          avg_sil_width = mean(sil_c[, "sil_width"]),
                                          med_sil_width = median(sil_c[, "sil_width"]),
                                          min_sil_width = min(sil_c[, "sil_width"]),
                                          max_sil_width = max(sil_c[, "sil_width"]),
                                          mean_clust_size = mean(sil_c_summary$clus.sizes)))
  }
  
}

silhouette_values <- silhouette_values[with(silhouette_values, order(variables, axis)), ]


##FUNCTION## Apply clustering to data matrix. Output clustering variables and visualization.
TwoWayclust <- function(Data1, group_k = 3, variable_k = 3) {
  #takes in a csv dataset, cleans it up, standardizes it, and calculates the twowayclust on the data at the threshold defined by the user
  #saves the heatmap, and outputs the mycl for the group differences function
  
  #change so that if variable is already loaded into workspace the can call it
  #if variable is a path then do the following, else just set Clusterset equal to the dataset_path variable
  
  # 3.1 Euclidean + Ward Clustering of Subjects
  r_dist <- dist(Data1, method = "euclidean")
  hr <- hclust(r_dist, method = "ward.D2");
  #r_dist <- dist(Data1, method = "binary")
  #hr <- hclust(r_dist, method = "complete");
  
  # 3.2 Spearman + Complete Clustering of Variables
  c_dist <- dist(t(Data1), method = "euclidean")
  hc <- hclust(c_dist, method = "ward.D2")
  #c_dist <- dist(t(Data1), method = "binary")
  #hc <- hclust(c_dist, method = "complete")
  
  # 4 Subject Group Assignment
  Sub_Group <- cutree(hr, k = group_k)
  mycolhr <- rainbow(length(unique(Sub_Group)), start = 0.1, end = 0.9); 
  mycolhr <- mycolhr[as.vector(Sub_Group)]
  
  # 5 Variable Group Assignment
  Var_Group <- cutree(hc, k = variable_k)
  mycolhc <- rainbow(length(unique(Var_Group)), start = 0.1, end = 0.9); 
  mycolhc <- mycolhc[as.vector(Var_Group)]
  
  # 6 Visualization
  h<-heatmap(as.matrix(Data1), Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = inferno(256), scale = "none", RowSideColors = mycolhr, ColSideColors = mycolhc)
  h
  #Action- Figure out how to export the visualization to disk-
  #Action- Figure out how to save all relevant variables- Var-Group, Sub_group, mycolhr, hr, hc
}

TwoWayclust_means <- function(Data1, group_k = 3, variable_k = 3) {
  # 3.1 Euclidean + Ward Clustering of Subjects
  r_dist <- dist(Data1, method = "euclidean")
  hr <- hclust(r_dist, method = "ward.D2");
  #r_dist <- dist(Data1, method = "binary")
  #hr <- hclust(r_dist, method = "complete");
  
  # 3.2 Spearman + Complete Clustering of Variables
  c_dist <- dist(t(Data1), method = "euclidean")
  hc <- hclust(c_dist, method = "ward.D2")
  #c_dist <- dist(t(Data1), method = "binary")
  #hc <- hclust(c_dist, method = "complete")
  
  # 4 Subject Group Assignment
  Sub_Group <- cutree(hr, k = group_k)
  mycolhr <- rainbow(length(unique(Sub_Group)), start = 0.1, end = 0.9); 
  mycolhr <- mycolhr[as.vector(Sub_Group)]
  
  # 5 Variable Group Assignment
  Var_Group <- cutree(hc, k = variable_k)
  mycolhc <- rainbow(length(unique(Var_Group)), start = 0.1, end = 0.9); 
  mycolhc <- mycolhc[as.vector(Var_Group)]
  
  # plot as line plot
  df_clust_ordered <- Data1[hr$order, hc$order]
  hr_ordered <- Sub_Group[hr$order]
  hc_ordered <- Var_Group[hc$order]
  df_clust_ordered <- cbind(df_clust_ordered, unname(hr_ordered))
  colnames(df_clust_ordered)[which(colnames(df_clust_ordered) == "")] <- "group"
  
  df_clust_scaled_means <- aggregate(df_clust_ordered, list(group = unname(hr_ordered)), mean)
  df_clust_scaled_means <- melt(df_clust_scaled_means, id.vars = "group")
  names(df_clust_scaled_means) <- c("group", "variable", "mean")
  
  df_clust_scaled_se <- aggregate(df_clust_ordered, list(group = unname(hr_ordered)), function(x) sd(x)/sqrt(length(x)))
  df_clust_scaled_se <- melt(df_clust_scaled_se, id.vars = "group")
  names(df_clust_scaled_se) <- c("group", "variable", "se")
  
  df_clust_scaled_all <- merge(df_clust_scaled_means, df_clust_scaled_se, by = c("group", "variable"))
  
  ggplot(df_clust_scaled_all, aes(x = variable, y = mean, color = as.factor(group), group = as.factor(group))) + 
    geom_point() + 
    geom_line() +
    geom_ribbon(aes(ymin = mean - se,
                    ymax = mean + se,
                    fill = as.factor(group)),
                alpha = 0.2) +
    #geom_vline(xintercept = vlines, colour = "#919d9d") +
    labs(title = "Comparing Means of Clustered Groups", x = "Measure", y = "Mean Scaled Score") +
    scale_color_manual(name = "Cluster", values = rainbow(length(unique(Sub_Group)), start=0.1, end=0.9)) + 
    scale_fill_discrete(guide = FALSE) +
    #scale_x_discrete(labels = c("GFTA Sounds-\nin-Words", "CTOPP Rapid\n Symbolic Naming",
    #                            "CTOPP Elision", "WIAT Pseudo-word\n Decoding",
    #                            "WIAT Spelling", "WIAT Word Reading",
    #                            "CTOPP Non-word\n Repetition", "CTOPP Blending\n Words",
    #                            "Verbal IQ")) +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 60, hjust = 1))
}

#heights <- data.frame(fields = c("all", "asmt", "dx"), rows = c(5, 3, 7), cols = c(7, 3, 3), stringsAsFactors = FALSE)
heights <- data.frame(fields = names(dfs), rows = c(4, 3), cols = c(6, 4), stringsAsFactors = FALSE)

for (i in 1:length(dfs)) {
  df <- dfs[[i]]
  # Remove EID column and append as row name
  EID <- df$EID
  df <- df[,-1]
  # Centers and scales data.
  #df <- scale(df)
  #df <- apply(df, 2, znorm)
  df <- apply(df, 2, ranknorm)
  rownames(df) <- EID
  
  png(paste0("Plots/Bicluster-znorm/Heatmaps/heatmap_", heights[i, "fields"], ".png"))
  TwoWayclust(df, group_k = heights[i, "rows"], variable_k = heights[i, "cols"])
  dev.off()
  
  TwoWayclust_means(df, group_k = heights[i, "rows"], variable_k = heights[i, "cols"])
  ggsave(paste0("Plots/Bicluster-znorm/Means/bicluster_means_", heights[i, "fields"], ".png"))
}

#### K-Mode Clustering
library(klaR)
#categorical <- c("ADHD_I", "ADHD_H", "ADHD_C", "ASD", "DMDD", "GAD", "Learning", "Anx_Soc", "Depression")
#df_clust <- read.csv("Data Cleaned/Biclustering_Data.csv", stringsAsFactors = FALSE)
K_Modes_Dx_of_interest <- Dx_of_interest[, 2:9]
set.seed(123)
K_Modes_Dx_of_interest <- Clustering_Dx_of_interest[, c(sample(1:8))]

clust_df <- data.frame(nclust = numeric(), weighted = logical(),
                                        mean_dist = numeric(), stringsAsFactors = FALSE)
for (nclust in 4:9) {
  for (bool in c(TRUE, FALSE)) {
    clust <- kmodes(K_Modes_Dx_of_interest[, 2:ncol(K_Modes_Dx_of_interest)], nclust, weighted = bool)
    K_Modes_Dx_of_interest_2 <- data.frame(Clustering_Dx_of_interest)
    K_Modes_Dx_of_interest_2[, "cluster"] <- clust$cluster
    #Clustering_Dx_of_interest_2 <- Clustering_Dx_of_interest_2[order(Clustering_Dx_of_interest_2$cluster), ]
    K_Modes_Dx_of_interest_2 <- K_Modes_Dx_of_interest_2[with(K_Modes_Dx_of_interest_2, order(cluster, ADHD_Combined, ADHD_Inattentive, ADHD_Hyperactive, ASD, Social_Anxiety, Anxiety, Learning_Disorder, Depression)), ]
    mycolhr <- rainbow(length(unique(K_Modes_Dx_of_interest_2$cluster)), start = 0.1, end = 0.9); 
    mycolhr <- mycolhr[K_Modes_Dx_of_interest_2$cluster]
    heatmap(as.matrix(K_Modes_Dx_of_interest_2[, names(K_Modes_Dx_of_interest_2) != "cluster"]), Rowv = NA, Colv = NA, col = inferno(256), scale = "none", RowSideColors = mycolhr)}}



#### Cluster categorical variables using binary distance matrix clustering
install.packages("ade4")
library('ade4')
library(cluster)

Cluster_Dx_of_interest <- Dx_of_interest[,1:9]
write.csv(Cluster_Dx_of_interest, "Cluster_Dx_of_interest.csv", row.names = FALSE)

# Remove ID column and append as row name
rownames(Cluster_Dx_of_interest) <- Cluster_Dx_of_interest$ID
Cluster_Dx_of_interest <- Cluster_Dx_of_interest[,-1]
Cluster_Dx_of_interest <- as.matrix(Cluster_Dx_of_interest)

# 3.1 Euclidean + Ward Clustering of Subjects
r_dist <- dist(Cluster_Dx_of_interest, method = "binary")
hr <- hclust(r_dist, method = "ward.D2");
plot(hr)
#r_dist <- dist(Data1, method = "binary")
#hr <- hclust(r_dist, method = "complete");

# 3.2 Spearman + Complete Clustering of Variables (Dxs)
c_dist <- dist(t(Cluster_Dx_of_interest), method = "binary")
hc <- hclust(c_dist, method = "ward.D2")
plot(hc)
#c_dist <- dist(t(Data1), method = "binary")
#hc <- hclust(c_dist, method = "complete")

# 4 Subject Group Assignment
Sub_Group <- cutree(hr, k = 7)
mycolhr <- rainbow(length(unique(Sub_Group)), start = 0.1, end = 0.9); 
mycolhr <- mycolhr[as.vector(Sub_Group)]

# 5 Variable Group Assignment
Var_Group <- cutree(hc, k = 4)
mycolhc <- rainbow(length(unique(Var_Group)), start = 0.1, end = 0.9); 
mycolhc <- mycolhc[as.vector(Var_Group)]

# 6 Visualization
#heatmap

h<-heatmap(as.matrix(Cluster_Dx_of_interest), Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = inferno(256), scale = "none", RowSideColors = mycolhr, ColSideColors = mycolhc)
h


#silhouette for subjects
sil_r <- silhouette(Sub_Group, r_dist)
plot(sil_r)

sil_r_summary <- summary(sil_r)
summary(sil_r)

#silhouette for variables
sil_c <- silhouette(Var_Group, c_dist)
plot(sil_c)

sil_c_summary <- summary(sil_c)
summary(sil_c)


silhouette_values <- rbind(silhouette_values, 
                           data.frame(variables = names(dfs)[i], 
                                      axis = "col", 
                                      k = k_col, 
                                      avg_sil_width = mean(sil_c[, "sil_width"]),
                                      med_sil_width = median(sil_c[, "sil_width"]),
                                      min_sil_width = min(sil_c[, "sil_width"]),
                                      max_sil_width = max(sil_c[, "sil_width"]),
                                      mean_clust_size = mean(sil_c_summary$clus.sizes)))

###### Gower Daisy Clusters ######################
library(cluster)

# Gower Distance + Ward Clustering of Subjects
r_dist <- daisy(Cluster_Dx_of_interest, metric = c("gower"))
hr <- hclust(r_dist, method = "ward.D2");
plot(hr)

# 3.2 Spearman + Complete Clustering of Variables (Dxs)
c_dist <- daisy(t(Cluster_Dx_of_interest), metric = "gower")
hc <- hclust(c_dist, method = "ward.D2")
plot(hc)

# 4 Subject Group Assignment
Sub_Group <- cutree(hr, k = 7)
mycolhr <- rainbow(length(unique(Sub_Group)), start = 0.1, end = 0.9); 
mycolhr <- mycolhr[as.vector(Sub_Group)]

# 5 Variable Group Assignment
Var_Group <- cutree(hc, k = 4)
mycolhc <- rainbow(length(unique(Var_Group)), start = 0.1, end = 0.9); 
mycolhc <- mycolhc[as.vector(Var_Group)]

# 6 Visualization
#heatmap

h<-heatmap(as.matrix(Cluster_Dx_of_interest), Rowv = as.dendrogram(hr), Colv = as.dendrogram(hc), col = inferno(256), scale = "none", RowSideColors = mycolhr, ColSideColors = mycolhc)
h

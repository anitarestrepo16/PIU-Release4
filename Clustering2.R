### Processing Speed Biclustering
### Bonhwang Koo
### 7/19/2018

library(psych)
library(viridis)
library(fmsb)
library(cluster)
library(ggplot2)
library(reshape)
library(klaR)

##FUNCTION## Apply clustering to data matrix. Output clustering variables and visualization.
TwoWayclust <- function(Data1, group_k = 3, variable_k = 3, method = "euclidean", gower = FALSE) {
  #takes in a csv dataset, cleans it up, standardizes it, and calculates the twowayclust on the data at the threshold defined by the user
  #saves the heatmap, and outputs the mycl for the group differences function
  
  #change so that if variable is already loaded into workspace the can call it
  #if variable is a path then do the following, else just set Clusterset equal to the dataset_path variable
  if (!gower) {
    r_dist <- dist(Data1, method = method)
    c_dist <- dist(t(Data1), method = method)
  } else {
    r_dist <- daisy(Data1, metric = "gower")
    c_dist <- daisy(t(Data1), metric = "gower")
  }
  hr <- hclust(r_dist, method = "ward.D2");
  hc <- hclust(c_dist, method = "ward.D2")
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

TwoWayclust_means <- function(Data1, group_k = 3, variable_k = 3, method = "euclidean", gower = FALSE) {
  if (!gower) {
    r_dist <- dist(Data1, method = method)
    c_dist <- dist(t(Data1), method = method)
  } else {
    r_dist <- daisy(Data1, metric = "gower")
    c_dist <- daisy(t(Data1), metric = "gower")
  }
  hr <- hclust(r_dist, method = "ward.D2");
  hc <- hclust(c_dist, method = "ward.D2")
  
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

############## Binary distance hierarchical clustering

Cluster_Dx_of_interest <- Dx_of_interest[,1:9]
write.csv(Cluster_Dx_of_interest, "Cluster_Dx_of_interest.csv", row.names = FALSE)

# Remove ID column and append as row name
rownames(Cluster_Dx_of_interest) <- Cluster_Dx_of_interest$ID
Cluster_Dx_of_interest <- Cluster_Dx_of_interest[,-1]
Cluster_Dx_of_interest <- as.matrix(Cluster_Dx_of_interest)

# Cluster rows
r_dist <- dist(Cluster_Dx_of_interest, method = "binary")
hr <- hclust(r_dist, method="ward.D2")

# Cluster columns 
c_dist <- dist(t(Cluster_Dx_of_interest), method = "binary")
hc <- hclust(c_dist, method="ward.D2")

png(file="Subject_dendrogram", width = 1200, height = 1000)
plot(hr, labels = FALSE)
dev.off()

png(file="Variable_dendrogram", width = 1200, height = 1000)
plot(hc)
dev.off()

## Silhouette Values
silhouette_values <- data.frame(variables = character(), axis = character(), k = numeric(), avg_sil_width = numeric(),
                                med_sil_width = numeric(), min_sil_width = numeric(), max_sil_width = numeric(), 
                                mean_clust_size = numeric(), stringsAsFactors = FALSE)


for (k_row in c(4:9)) {
  
  r_dist <- dist(Cluster_Dx_of_interest, method = "binary")
  hr <- hclust(r_dist, method="ward.D2")
  Sub_Group <- cutree(hr, k = k_row)
  
  png(paste0("silhouette_row_k", k_row, ".png"))
  sil_r <- silhouette(Sub_Group, r_dist)
  plot(sil_r)
  dev.off()
  
  sil_r_summary <- summary(sil_r)
  
  silhouette_values <- rbind(silhouette_values, 
                             data.frame(variables = "dx", 
                                        axis = "row", 
                                        k = k_row, 
                                        avg_sil_width = mean(sil_r[, "sil_width"]),
                                        med_sil_width = median(sil_r[, "sil_width"]),
                                        min_sil_width = min(sil_r[, "sil_width"]),
                                        max_sil_width = max(sil_r[, "sil_width"]),
                                        mean_clust_size = mean(sil_r_summary$clus.sizes)))
}

for (k_col in c(2:7)) {
  
  c_dist <- dist(t(Cluster_Dx_of_interest), method = "binary")
  hc <- hclust(c_dist, method="ward.D2")
  Var_Group <- cutree(hc, k = k_col)
  
  png(paste0("silhouette_col_k", k_col, ".png"))
  sil_c <- silhouette(Var_Group, c_dist)
  plot(sil_c)
  dev.off()
  
  sil_c_summary <- summary(sil_c)
  
  silhouette_values <- rbind(silhouette_values, 
                             data.frame(variables = "dx", 
                                        axis = "col", 
                                        k = k_col, 
                                        avg_sil_width = mean(sil_c[, "sil_width"]),
                                        med_sil_width = median(sil_c[, "sil_width"]),
                                        min_sil_width = min(sil_c[, "sil_width"]),
                                        max_sil_width = max(sil_c[, "sil_width"]),
                                        mean_clust_size = mean(sil_c_summary$clus.sizes)))
}


silhouette_values <- silhouette_values[with(silhouette_values, order(variables, axis)), ]
library(ggplot2)
ggplot(silhouette_values[,], aes(x = k, y = avg_sil_width)) + geom_line() + geom_point() + facet_grid(axis ~ ., scales = "free_y")
ggsave("silhouette-widths.png")

# Heatmap: Group k = 4:9, variable k = 4
for (i in c(4:9)) {
  png(paste0("Heatmap_rowk", i, ".png"))
  TwoWayclust(Cluster_Dx_of_interest, group_k = i, variable_k = 4, method = "binary")
  dev.off()
}


#### K-Mode Clustering
#categorical <- c("ADHD_I", "ADHD_H", "ADHD_C", "ASD", "DMDD", 
#                 "GAD", "Learning", "Anx_Soc", "Depression")
#df_clust <- read.csv("Data Cleaned/Biclustering_Data.csv", stringsAsFactors = FALSE)
#df_clust_dx <- df_clust[, c(categorical)]
library(klaR)
set.seed(1234)
K_Modes_Dx_of_interest <- Cluster_Dx_of_interest[, c(sample(1:8))]

clust_df <- data.frame(nclust = numeric(), weighted = logical(), mean_dist = numeric(), stringsAsFactors = FALSE)
for (nclust in 4:9) {
  for (bool in c(TRUE, FALSE)) {
    clust <- kmodes(K_Modes_Dx_of_interest[, 2:ncol(K_Modes_Dx_of_interest)], nclust, weighted = bool)
    K_Modes_Dx_of_interest_2 <- data.frame(K_Modes_Dx_of_interest)
    K_Modes_Dx_of_interest_2[, "cluster"] <- clust$cluster
    #df_clust_dx_2 <- df_clust_dx_2[order(df_clust_dx_2$cluster), ]
    K_Modes_Dx_of_interest_2 <- K_Modes_Dx_of_interest_2[with(K_Modes_Dx_of_interest_2, order(cluster, ADHD_Combined, ADHD_Inattentive, ADHD_Hyperactive, ASD, Social_Anxiety, Anxiety, Learning_Disorder, Depression)), ]
    mycolhr <- rainbow(length(unique(K_Modes_Dx_of_interest_2$cluster)), start = 0.1, end = 0.9); 
    mycolhr <- mycolhr[K_Modes_Dx_of_interest_2$cluster]
    png(paste0("kmode_Heatmap-kmode_nclust", nclust, "_", ifelse(bool, "weighted", "unweighted"),".png"))
    heatmap(as.matrix(K_Modes_Dx_of_interest_2[, names(K_Modes_Dx_of_interest_2) != "cluster"]), Rowv = NA, Colv = NA, col = inferno(256), scale = "none", RowSideColors = mycolhr)
    dev.off()
  }
}

#### Hierarchical Clustering using Gower Distance
library(cluster)
# Cluster rows by 
r_dist <- daisy(Cluster_Dx_of_interest, metric = "gower")
hr <- hclust(r_dist, method="ward.D2")
# Cluster columns 
c_dist <- daisy(t(Cluster_Dx_of_interest), metric = "gower")
hc <- hclust(c_dist, method="ward.D2")

png("gower_row_clusters_dx.png")
plot(hr, labels = FALSE)
dev.off()

png("gower_col_clusters_dx.png")
plot(hc)
dev.off()

## Silhouette Values
silhouette_values_gower <- data.frame(variables = character(), axis = character(), k = numeric(), avg_sil_width = numeric(),
                                      med_sil_width = numeric(), min_sil_width = numeric(), max_sil_width = numeric(), 
                                      mean_clust_size = numeric(), stringsAsFactors = FALSE)


for (k_row in c(4:9)) {
  
  r_dist <- daisy(Cluster_Dx_of_interest, metric = "gower")
  hr <- hclust(r_dist, method="ward.D2")
  Sub_Group <- cutree(hr, k = k_row)
  
  # png(paste0("Plots/Bicluster-dx/Silhouette/silhouette_row_k", k_row, ".png"))
  sil_r <- silhouette(Sub_Group, r_dist)
  # plot(sil_r)
  # dev.off()
  
  sil_r_summary <- summary(sil_r)
  
  silhouette_values_gower <- rbind(silhouette_values_gower, 
                                   data.frame(variables = "dx", 
                                              axis = "row", 
                                              k = k_row, 
                                              avg_sil_width = mean(sil_r[, "sil_width"]),
                                              med_sil_width = median(sil_r[, "sil_width"]),
                                              min_sil_width = min(sil_r[, "sil_width"]),
                                              max_sil_width = max(sil_r[, "sil_width"]),
                                              mean_clust_size = mean(sil_r_summary$clus.sizes)))
}

for (k_col in c(2:7)) {
  
  c_dist <- daisy(t(Cluster_Dx_of_interest), metric = "gower")
  hc <- hclust(c_dist, method="ward.D2")
  Var_Group <- cutree(hc, k = k_col)
  
  # png(paste0("Plots/Bicluster-dx/Silhouette/silhouette_col_k", k_col, ".png"))
  sil_c <- silhouette(Var_Group, c_dist)
  # plot(sil_c)
  # dev.off()
  
  sil_c_summary <- summary(sil_c)
  
  silhouette_values_gower <- rbind(silhouette_values_gower, 
                                   data.frame(variables = "dx", 
                                              axis = "col", 
                                              k = k_col, 
                                              avg_sil_width = mean(sil_c[, "sil_width"]),
                                              med_sil_width = median(sil_c[, "sil_width"]),
                                              min_sil_width = min(sil_c[, "sil_width"]),
                                              max_sil_width = max(sil_c[, "sil_width"]),
                                              mean_clust_size = mean(sil_c_summary$clus.sizes)))
}


silhouette_values_gower <- silhouette_values_gower[with(silhouette_values_gower, order(variables, axis)), ]
library(ggplot2)
ggplot(silhouette_values_gower[,], aes(x = k, y = avg_sil_width)) + geom_line() + facet_grid(axis ~ ., scales = "free_y")
ggsave("gower_silhouette-widths.png")

# Heatmap: Group k = 4:9, variable k = 4
for (i in c(4:9)) {
  png(paste0("gower_Heatmap_rowk", i, ".png"))
  TwoWayclust(Cluster_Dx_of_interest, group_k = i, variable_k = 4, gower = TRUE)
  dev.off()
}

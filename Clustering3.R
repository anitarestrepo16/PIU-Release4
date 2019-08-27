############## Binary distance hierarchical clustering #################
setwd("J:/Healthy Brain Network/Mentorship/Research Projects/2018/Internalizing/Project")


Cluster_Dx_of_interest <- Dx_of_interest[,1:9]
write.csv(Cluster_Dx_of_interest, "Cluster_Dx_of_interest.csv", row.names = FALSE)

# Remove kids w/ less than 2 dxs
Cluster_Dx_of_interest <- subset (Dx_of_interest, rowSums(Dx_of_interest[,2:9]) >= 2, select= ID:Social_Anxiety)

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
library(cluster)
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
library(viridis)
for (i in c(4:9)) {
  png(paste0("Heatmap_rowk", i, ".png"))
  par(oma = c(7, 1, 1, 1))
  TwoWayclust(Cluster_Dx_of_interest, group_k = i, variable_k = 4, method = "binary")
  dev.off()
}


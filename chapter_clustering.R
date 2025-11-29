############ Stat 666 Term Project  ##############
############ Code for Clustering    ##############

### SET UP
library(cluster)
library(ggplot2)
library(tidyr)
library(dplyr)

# setwd("termproj")
chaps <- read.csv("tfidf_features_100.csv", header=TRUE)
chaps <-chaps[,-1]
dim(chaps)
# no need to standardize b/c TF–IDF, TFIDF features on same numerical scale

# get distance
feat_dist <- dist(data.matrix(chaps))

##### CLUSTER COMPUTATIONS

par(mfrow=c(1,3))

## COMPLETE LINKAGE
complink <- hclust(feat_dist,method='complete')
plot(complink,labels=labels(chaps)[[1]]) 

## AVERAGE LINKAGE
avglink <- hclust(feat_dist,method='average')
plot(avglink,labels=labels(chaps)[[1]]) 

## WARD'S LINKAGE
wardlink <- hclust(feat_dist,method='ward.D2')
plot(wardlink,labels=labels(chaps)[[1]])

par(mfrow=c(1,1))

# Plot dendrogram with colored cluster borders
k <- 7  # choose some number
groups <- cutree(wardlink, k = k)
plot(wardlink, labels = labels(chaps)[[1]])
rect.hclust(wardlink, k = k, border = 2:8)

tab <- table(groups)
tab
tab / sum(tab) # percentage of chapters in each cluster
sd(as.numeric(tab)) # standard deviation of cluster sizes

## K MEANS
k <- 11  # choose number of clusters
k_means <- kmeans(chaps, centers = k, nstart = 25)  # nstart = 25 tries different random starts
groups <- k_means$cluster
table(groups) # inspect cluster sizes
sd(as.numeric(table(groups))) # SD of cluster sizes

### trying to find the best cluster size --------------------

# clustering methods
methods <- list(
  complete = complink,
  average  = avglink,
  ward     = wardlink#,
  # kmean    = k_means
)
# k clusters to try
ks <- 5:12

### compute SD of cluster sizes ####
# this is code to print them in a matrix
# # storage matrix
# sd_mat <- matrix(NA_real_,
#                  nrow = length(methods),
#                  ncol = length(ks),
#                  dimnames = list(method = names(methods),
#                                  k = paste0("k", ks)))
# 
# for (m in names(methods)) {
#   for (k in ks) {
#     groups <- cutree(methods[[m]], k = k)
#     tab <- table(groups)
#     sd_mat[m, paste0("k", k)] <- sd(as.numeric(tab))
#   }
# }
# sd_mat

### visualize results

# Compute SD of cluster sizes
sd_list <- data.frame()

for(m in names(methods)){
  for(k in ks){
    groups <- cutree(methods[[m]], k = k)
    tab <- table(groups)
    sd_val <- sd(as.numeric(tab))
    sd_list <- rbind(sd_list, data.frame(Method = m, k = k, SD = sd_val))
  }
}

# K-means clustering
km_sil_list <- data.frame()
for(k in ks){
  km_res <- kmeans(chaps, centers = k, nstart = 25)
  tab <- table(km_res$cluster)
  sd_val <- sd(as.numeric(tab))
  # pause and calculate silhouette scores for k-means while we're here:
  sil <- silhouette(km_res$cluster, feat_dist)
  avg_sil <- mean(sil[, 3])
  km_sil_list <- rbind(km_sil_list, data.frame(Method = "kmeans", k = k, Silhouette = avg_sil))
  # okay back to SD for balance
  sd_list <- rbind(sd_list, data.frame(Method = "kmeans", k = k, SD = sd_val))
}

# Ensure k is treated as ordered factor so they print in order
sd_list$k <- factor(sd_list$k, levels = ks)

# Plot heatmap
ggplot(sd_list, aes(x = k, y = Method, fill = SD)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", SD)), size = 3) +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = "Number of clusters (k)", y = "Linkage Method",
       title = "Cluster Balance (Std. Dev.)") +
  theme_minimal(base_size = 12)

### calculate silhouette scores for the hierarchical methods ####
for (m in names(methods)) {
  cat("\n=== Method:", m, "===\n")
  for (k in ks) {
    groups_k <- cutree(methods[[m]], k = k)
    sil <- silhouette(groups_k, dist(chaps))
    cat("k =", k, "Avg silhouette =", round(mean(sil[, 3]),4), "\n")
  }
}
km_sil_list


##################################

# I'm kinda leaning towards k-means as the best method, by balance
# and by silhouette score? so a cluster size of perhaps 8????
# 12 feels like two many, 5 wouldn't be terrible, I feel like it might
# depend on PCA/ discriminant analysis

# My next steps are to see if I can print out what chapters go in what 
# clusters, to see if I can ascribe any meaning to this

# then do the write up and be done woohoo 

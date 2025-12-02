############ Stat 666 Term Project  ##############
############ Code for Clustering    ##############

##### SET UP
library(cluster)
library(ggplot2)
library(tidyr)
library(dplyr)

# setwd("termproj")
chaps <- read.csv("tfidf_features_100.csv", header=TRUE)
text_names <- chaps[, 1]  # first column = text names (chapter names)
chaps <-chaps[,-1] # remove the chapter names (so "1 Nephi", etc., isn't being used in clustering)
dim(chaps)
# no need to standardize b/c TFIDF features on same numerical scale

# get distance
feat_dist <- dist(data.matrix(chaps))

##### AGGLOMERATIVE METHOD CLUSTER COMPUTATIONS

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

# Visualizing Ward's linkage with clusters
# Plot dendrogram with colored cluster borders
par(mfrow=c(1,1))
k <- 7  # choose some number by looking at it, I picked 7
groups <- cutree(wardlink, k = k)
plot(wardlink, labels = labels(chaps)[[1]])
rect.hclust(wardlink, k = k, border = 2:8)

# Calculating Balance for Ward Method K=7
tab <- table(groups)
tab
tab / sum(tab) # percentage of chapters in each cluster
sd(as.numeric(tab)) # standard deviation of cluster sizes

##### K MEANS CLUSTERING METHOD

k <- 9  # choose num clusters, this is just a starting point, will check others later
k_means <- kmeans(chaps, centers = k, nstart = 25)  # nstart = 25 tries different random starts
groups <- k_means$cluster
table(groups) # cluster sizes / how many chapters sorted into each cluster
sd(as.numeric(table(groups))) # SD of cluster sizes

##### EXPLORE THE BEST CLUSTER METHOD AND K SIZE --------------------

# agglomerative clustering methods
methods <- list(
  complete = complink,
  average  = avglink,
  ward     = wardlink
)
# k clusters to try
ks <- 5:12 # this range was selected after looking at the wards dendrogram

# this is code to print them SD of cluster sizes in a matrix
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

# Compute SD of cluster sizes (balance)
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

# Leaning towards k-means as the best method, by balance
# and by silhouette score. Although, all the silhouette scores are pretty bad.


##### EXPLORE AND INTERPRET RESULTS --------------------------------------

k <- 5  # choose number of clusters
k_means <- kmeans(chaps, centers = k, nstart = 25)
clusters <- k_means$cluster

tfidf <- as.matrix(chaps)
cluster_means <- aggregate(tfidf, by = list(cluster = clusters), FUN = mean)
rownames(cluster_means) <- paste0("Cluster_", cluster_means$cluster)
cluster_means$cluster <- NULL
cluster_means <- as.matrix(cluster_means)
# Now cluster_means is an 8 × p matrix:
# rows = clusters
# columns = words
# values = mean TF–IDF per cluster

# extract top words from each cluster
top_words <- list()
for (i in 1:5) {
  # sort words by decreasing mean tf-idf
  sorted <- sort(cluster_means[i, ], decreasing = TRUE)
  # take the top 10 most characteristic words
  top_words[[paste0("Cluster_", i)]] <- head(sorted, 10)
}
top_words

# view which chapters went with which clusters
cluster_members <- split(text_names, clusters)
cluster_members

### Potential LDA code
library(MASS)
lda_fit <- lda(clusters ~ ., data = data.frame(clusters = clusters, tfidf))
lda_fit


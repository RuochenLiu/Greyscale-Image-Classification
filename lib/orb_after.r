library(data.table)
library(dplyr)
library(gbm)

orb_features <- fread('../data/orb_feature.csv',header = FALSE)

orb_features <- as.data.frame(orb_features)
colnames(orb_features)[33] <- c("Image_index")
dim(orb_features)

set.seed(333)
orb_features_cluster <- kmeans(orb_features[,c(1:32)], centers = 500)
length(orb_features_cluster$size)
length(orb_features_cluster$cluster)

num_of_image <- orb_features[dim(orb_features)[1],dim(orb_features)[2]]

index <- rep(1:length(orb_features_cluster$cluster),1)
orb_cluster_500 <- matrix(NA, nrow = 500, ncol = num_of_image)

#new prob matrix 
for (i in 1:num_of_image){
  if (length(index[orb_features$Image_index == i]) == 0)
    orb_cluster_500[,i] = 0
  else{
    kp_list <- index[orb_features$Image_index == i]
    # clusters# 500
    for (k in 1 : 500){
      cluster_list <- index[orb_features_cluster$cluster == k]
      total_kp <- length(kp_list)
      num = 0
      for (j in 1:length(cluster_list)){
        if (cluster_list[j] >= min(kp_list) & cluster_list[j] <= max(kp_list))
        num = num + 1
      }
      orb_cluster_500[k,i] <- num / total_kp
  }
  }
  print(i)}


apply(orb_cluster_500, 2, function(i) sum(i))
apply(orb_cluster_500, 2, function(i) sum(i))[1000:2000]
orb_features_500 <- t(orb_cluster_500)

save(orb_features_500, file = "../data/orb_features_500.RData")



library(mlbench)
library(caret)
library(cluster)
library(stats)
library(factoextra)
library(e1071)
library(LICORS)
library(ggdendro)
#library(fpc)
library(dbscan)
library(tidyverse)


set.seed(123)

setwd("/home/utku/ceng474/clustering")
training<-read.csv('ALS_TrainingData_2223.csv')
#testing<-read.csv('ALS_TestingData_78.csv')
training_cols <- colnames(training)
#testing_cols <- colnames(testing)

#remove rejected cols
rejected_cols <- c('ID', 'Hematocrit_median', 'Hematocrit_min', 'mouth_median', 'Platelets_min', 'SubjectID')
training_cols %in% rejected_cols
training_cols<-training_cols[! training_cols %in% rejected_cols]


training <- training[training_cols]
write.csv(training, file="training2.csv", row.names = FALSE)
str(training)
summary(training)




training_scaled <- as.data.frame(scale(training, center = TRUE, scale = TRUE))
skewed <- vector()
for(i in 1:length(colnames(training_scaled))){
  if((skewness(training_scaled[,i]) > 0.5) | (skewness(training_scaled[,i]) < -0.5)){
    skewed[length(skewed)+1] <- i
  }
}

skewed
skewness(training_scaled[,2])
preprocessParams <- preProcess(training_scaled[,skewed], method=c("YeoJohnson", "center", "scale"))
print(preprocessParams)
training_scaled[,skewed] <- predict(preprocessParams, training_scaled[,skewed])
skewness(training_scaled[,2])

# head(training_scaled)
# 
training_cols2 <- colnames(training_scaled)
rejected_cols2 <- c('Hemoglobin_median')
training_cols2 %in% rejected_cols2
training_cols2<-training_cols2[! training_cols2 %in% rejected_cols2]
training_scaled <- training_scaled[training_cols2]

# write.csv(training_scaled, file="training_scaled.csv", row.names = FALSE)
#distance <- get_dist(training)
distance <- get_dist(training_scaled)


# PCA

# pca <- prcomp(training, center = TRUE, scale = TRUE)
# summary(pca)
# 
# screeplot(pca, type = "l", npcs = 40, main = "Screeplot of the first 40 PCs")
# abline(h = 1, col="red", lty=5)
# legend("topright", legend=c("Eigenvalue = 1"),
#        col=c("red"), lty=5, cex=0.6)
# cumpro <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
# plot(cumpro[0:40], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
# abline(v = 28, col="blue", lty=5)
# abline(h = 0.80718, col="blue", lty=5)
# legend("topleft", legend=c("Cut-off @ PC28"),
#        col=c("blue"), lty=5, cex=0.6)
# 
# plot(pca$x[,1],pca$x[,2], xlab="PC1 (11.6%)", ylab = "PC2 (06.4%)", main = "PC1 / PC2 - plot")

# elbow method to determine k
# default
# wcss <- vector()
# mean_ss  <- vector()
# for(i in 1:15) wcss[i]<-kmeans(training, i, nstart=50, iter.max=15)$tot.withinss
# plot(1:15, wcss, type="b", main=paste('Elbow Method'), xlab='k (number of clusters)',ylab='Within Cluster Sum of Squares')
# 
# for(i in 2:15) {
#    km <- kmeans(training, i, nstart=25, iter.max=15)
#    ss <- silhouette(km$cluster, distance)
#    mean_ss[i-1] <- mean(ss[, 3])
# }
# plot(2:15, mean_ss, type="b", xlab='k (number of clusters)',ylab='Average Silhouette Value')

fviz_nbclust(training, kmeans, method = "wss")
fviz_nbclust(training, kmeans, method = "silhouette")
# gap_stat <- clusGap(training, FUN = kmeans, nstart = 10, K.max = 10, B = 10)
# print(gap_stat, method="firstmax")
# fviz_gap_stat(gap_stat)

#scaled
fviz_nbclust(training_scaled, kmeans, method = "wss")
fviz_nbclust(training_scaled, kmeans, method = "silhouette")
# gap_stat <- clusGap(training_scaled, FUN = kmeans, nstart = 25, K.max = 10, B = 25)
# print(gap_stat, method="firstmax")
# fviz_gap_stat(gap_stat)

# k = 2 

kmm2 = kmeans(training_scaled, 2, nstart = 25, iter.max = 15)
kmm2
kmm2$size
fviz_cluster(kmm2, data = training_scaled)
sil2 = silhouette(kmm2$cluster, distance)
fviz_silhouette(sil2)
center_dif2 <- vector()
for(i in 1:94){
  center_dif2[i] <- abs(kmm2$centers[1,i] - kmm2$centers[2,i])
}
barplot(center_dif2, xlab = "Feature #", ylab="Difference between cluster centers")

kmm2$centers

# k = 3

kmm3 = kmeans(training_scaled, 3, nstart = 25, iter.max = 15)
kmm3
fviz_cluster(kmm3, data = training_scaled)
sil3 = silhouette(kmm3$cluster, distance)
fviz_silhouette(sil3)
kmm3$centers

par(mfrow=c(1, 1), mar=c(4, 4, 4, 2))
myColors <- c("darkblue", "red", "green", "brown", "pink", "purple", "yellow")
barplot(t(kmm3$centers), beside = TRUE, xlab="cluster", 
        ylab="value", col = myColors)


# 
# kmm4 = kmeans(training_scaled, 4, nstart = 25, iter.max = 15)
# kmm4
# fviz_cluster(kmm4, data = training_scaled)
# sil4 = silhouette(kmm4$cluster, distance)
# plot(sil4, border=NA)
# fviz_silhouette(sil4)

distance_unscaled = get_dist(training)

# k = 2

kmm_unscaled2 = kmeans(training, 2, nstart = 25, iter.max = 15)
sil_unscaled2 = silhouette(kmm_unscaled2$cluster, distance_unscaled)
fviz_silhouette(sil_unscaled2)
fviz_cluster(kmm_unscaled2, data = training)
center_dif <- vector()
for(i in 1:95){
  center_dif[i] <- abs(kmm_unscaled2$centers[1,i] - kmm_unscaled2$centers[2,i])
}
barplot(center_dif, xlab = "Feature #", ylab="Difference between cluster centers")
summary(training)
fviz_cluster(kmm_unscaled2, data = training, choose.vars = c("onset_delta_mean", "mouth_min"))


# k = 3

kmm_unscaled3 = kmeans(training, 3, nstart = 25, iter.max = 15)
sil_unscaled3 = silhouette(kmm_unscaled3$cluster, distance_unscaled)
fviz_silhouette(sil_unscaled3)
#fviz_cluster(kmm_unscaled3, data = training)



# kmeans++
# k = 2
kmmpp2 <- kmeanspp(training_scaled, k = 2, start = "random", iter.max = 50, nstart = 25)
sil_pp2 <- silhouette(kmmpp2$cluster, distance)
fviz_silhouette(sil_pp2)
fviz_cluster(kmmpp2, data = training_scaled)
# k = 3
kmmpp3 <- kmeanspp(training_scaled, k = 3, start = "random", iter.max = 50, nstart = 25)
sil_pp3 <- silhouette(kmmpp3$cluster, distance)
fviz_silhouette(sil_pp3)
fviz_cluster(kmmpp3, data = training_scaled)

# Hierarchical Clustering

dend_single=hclust(distance, method='single')
dend_complete=hclust(distance, method='complete')
dend_wardD=hclust(distance, method='ward.D')

sil_single2=silhouette(cutree(dend_single, k = 2), distance)
fviz_silhouette(sil_single2)
sil_single3=silhouette(cutree(dend_single, k = 3), distance)
fviz_silhouette(sil_single3)

sil_single10=silhouette(cutree(dend_single, k = 10), distance)
fviz_silhouette(sil_single10)

sil_complete2=silhouette(cutree(dend_complete, k = 2), distance)
fviz_silhouette(sil_complete2)
sil_complete3=silhouette(cutree(dend_complete, k = 3), distance)
fviz_silhouette(sil_complete3)
comp3 <- hcut(training_scaled, k = 3, hc_method="complete")
fviz_cluster(comp3, ellipse.type="convex")

sil_complete10=silhouette(cutree(dend_complete, k = 10), distance)
fviz_silhouette(sil_complete10)

sil_wardD2=silhouette(cutree(dend_wardD, k = 2), distance)
fviz_silhouette(sil_wardD2)
sil_wardD3=silhouette(cutree(dend_wardD, k = 3), distance)
fviz_silhouette(sil_wardD3)
ward2 <- hcut(training_scaled, k = 2, hc_method = "ward.D")
fviz_cluster(ward2)
ward3 <- hcut(training_scaled, k = 3, hc_method = "ward.D")
fviz_cluster(ward3)

# DBSCAN

dbmodel1 <- dbscan(training_scaled, eps = 0.5, minPts = 5)
dbmodel1
dbmodel2 <- dbscan(training_scaled, eps = 10, minPts = 3)
dbmodel2
fviz_cluster(dbmodel2, training_scaled, stand = FALSE,  geom = "point")
sil_db2 <- silhouette(dbmodel2$cluster, distance)
fviz_silhouette(sil_db2)
dbmodel3 <- dbscan(training_scaled, eps = 9, minPts = 4)
dbmodel3
fviz_cluster(dbmodel3, training_scaled, stand = FALSE,  geom = "point")
sil_db3 <- silhouette(dbmodel3$cluster, distance)
summary(sil_db3)
fviz_silhouette(sil_db3)

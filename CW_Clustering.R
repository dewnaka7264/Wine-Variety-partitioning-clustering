library(readxl)

#set working directory
setwd("D:/2ND YEAR/Machine Learning and Data Mining/CW")
# Read the Excel file 
WineData <- read_xlsx("Whitewine_v6.xlsx")


WineData

colnames(WineData)

head(WineData)

#making sure it represents everything
dim(WineData)

# Get the first 11 columns of the dataset
WineData<- WineData[, 1:11]

boxplot(WineData)
        
colnames(WineData)


#Outlier removing function using IQR method

remove_row_outliers <- function(df) {
  is_outlier <- sapply(df, function(x) {
    Q1 <- quantile(x, 0.25)
    Q3 <- quantile(x, 0.75)
    IQR <- Q3 - Q1
    x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)
  })
  outlier_rows <- apply(is_outlier, 1, any)
  df[!outlier_rows, ]
}

# outlier removed dataset
WineData <- remove_row_outliers(WineData)
dim(WineData)

boxplot(WineData)

# Apply z-score normalization
WineData <- scale(WineData,)

head(WineData)


#nbClust method

#import necessary libraries

install.packages("NbClust")


library(cluster)
library(NbClust)
#library(factoextra)

nbclust_output <- NbClust(
  WineData,  #dataset
  distance = "euclidean",  # Distance metric
  min.nc = 2,  # Minimum number of clusters
  max.nc = 10,  # Maximum number of clusters
  method = "kmeans"  # Clustering method
)

print(nbclust_output)


#Elbow

# Range of number of clusters from 2 to 10
k = 2:10

WSS = sapply(k, function(k) {  # Apply a function for each value of k
  kmeans(WineData, centers=k)$tot.withinss  # K-means clustering with k clusters, calculate WSS
})

# use a line plot to plot the within sum of squares with a different number of k
plot(k, WSS, type="l", xlab= "Number of k", ylab="Within sum of squares")

#Gap Statistics
library(factoextra)
fviz_nbclust(WineData, kmeans, method = 'gap_stat')


# Silhouette Method

fviz_nbclust(WineData, kmeans, method = 'silhouette')

# kmeans

kmeansmodel <- kmeans(WineData,centers = 2)

fviz_cluster(kmeansmodel,data=WineData)
print(kmeansmodel) 


# Calculate BSS and WSS
BSS <- kmeansmodel$betweenss
WSS <- kmeansmodel$tot.withinss

# Calculate ratio of BSS to TSS
TSS <- BSS + WSS
BSS_TSS_ratio <- BSS / TSS

# Print BSS, WSS, and BSS/TSS ratio
cat("BSS:", BSS, "\n")
cat("WSS:", WSS, "\n")
cat("TSS:", TSS, "\n")
cat("BSS/TSS ratio:", BSS_TSS_ratio, "\n")

# Get clustered results
clustered_results <- kmeansmodel$cluster

# Print clustered results
print(clustered_results)

# Display the silhouette plot
# Compute silhouette values
sil <- silhouette(kmeansmodel$cluster, dist(WineData))
# Plot the silhouette
fviz_silhouette(sil)


## 2nd task Objectives

# compute variance of each variable
apply(WineData, 2, var)

# create new data frame with centered variables
scaled_WineData <- apply(WineData, 2, scale)

# Calculate eigenvalues & eigenvectors
Wine.cov <- cov(scaled_WineData)
Wine.eigen <- eigen(Wine.cov)
print(Wine.cov)
print(Wine.eigen)

# Perform PCA analysis
pca_result <- prcomp(scaled_WineData, scale = TRUE)


# Print eigenvalues and eigenvectors
print(summary(pca_result))

# Plot cumulative proportion of variance explained by PCs
plot(pca_result, type = "l", main = "Cumulative Proportion of Variance Explained")

# Choose the number of PCs that provide at least cumulative score > 85%
cumulative_variance <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2)) * 100

selected_pcs <- length(cumulative_variance[cumulative_variance >= 85])

# Print cumulative proportion of variance explained by PCs
print(cumulative_variance)

# Print number of selected PCs
print(selected_pcs)


#selected pc s
print(cumulative_variance[cumulative_variance >= 85])

# Create transformed dataset with selected PCs
transformed_data <- as.data.frame(predict(pca_result, newdata = WineData)[, 1:selected_pcs])

# Print the transformed dataset
head(transformed_data)


# 2.f.


# Determine the optimal number of clusters using NbClust
nbclust_output_pca <- NbClust(transformed_data, min.nc = 2, max.nc = 10, method = "kmeans")

# Print NbClust result for PCA-transformed data
print(nbclust_output_pca)


# Determine the optimal number of clusters using the Elbow method
wss <- sapply(1:10, function(k){kmeans(transformed_data, centers=k)$tot.withinss})
elbow <- fviz_nbclust(transformed_data, kmeans, method = "wss")

# Print Elbow method result for PCA-transformed data
print(elbow)

# Determine the optimal number of clusters using Gap statistics
gap_stat_pca <- clusGap(transformed_data, FUN = kmeans, n = 25, K.max = 10, B = 50)

# Plot Gap statistics for PCA-transformed data
plot_gap_pca <- plot(gap_stat_pca, main = "Gap statistics for PCA-transformed data")

# Print Gap statistics for PCA-transformed data
print(plot_gap_pca)



# Determine the optimal number of clusters using the Silhouette method
silhouette <- fviz_nbclust(transformed_data, kmeans, method = "silhouette")

# Print Silhouette method result for PCA-transformed data
print(silhouette)


#2.g.

# 'transformed_data' is PCA-transformed dataset

# Perform k-means clustering with the most favored k
kmeans_model_pca <- kmeans(transformed_data, centers = 2)

# Print kmeans model
print(kmeans_model_pca)

# Calculate BSS and WSS for PCA-transformed dataset
BSS_pca <- kmeans_model_pca$betweenss
WSS_pca <- kmeans_model_pca$tot.withinss

# Calculate ratio of BSS to TSS
TSS_pca <- BSS_pca + WSS_pca
BSS_TSS_ratio_pca <- BSS_pca / TSS_pca

# Print BSS, WSS, and BSS/TSS ratio for PCA-transformed dataset
cat("BSS for PCA-transformed dataset:", BSS_pca, "\n")
cat("WSS for PCA-transformed dataset:", WSS_pca, "\n")
cat("BSS/TSS ratio for PCA-transformed dataset:", BSS_TSS_ratio_pca, "\n")

# Get clustered results for PCA-transformed dataset
clustered_results_pca <- kmeans_model_pca$cluster

# Print clustered results for PCA-transformed dataset
print(clustered_results_pca)


# 2.h.

# Calculate silhouette widths
sil_width <- silhouette(kmeans_model_pca$cluster, dist(transformed_data))

# Plot silhouette plot
plot_silhouette <- fviz_silhouette(sil_width)

# Print silhouette plot
print(plot_silhouette)

# Average silhouette width score
avg_sil_width <- mean(sil_width[, "sil_width"])

# Print average silhouette width score
cat("Average Silhouette Width Score:", avg_sil_width, "\n")

# 2.i.

install.packages("fpc")
library(fpc)


# 'kmeans_model_pca$cluster' contains the cluster assignments obtained from the "new" k-means attempt

# Calculate Calinski-Harabasz index
ch_index <- calinhara(transformed_data, kmeans_model_pca$cluster)

# Print Calinski-Harabasz index
cat("Calinski-Harabasz Index:", ch_index, "\n")



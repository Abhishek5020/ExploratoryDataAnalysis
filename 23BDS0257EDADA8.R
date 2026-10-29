#23BDS0257 K-Means Clustering
#1. Load the dataset shared through e-mail
data <- read.csv("school93_98.csv")
head(data)

#23BDS0257
#2. View the dataset
View(data)
print(data)

#23BDS0257
#3. Check missing values
colSums(is.na(data))

#23BDS0257
#4. Check for the dependent variable distribution (table)
table(data$year)

#23BDS0257
#5. Select only numerical features for K-Means clustering
numeric_data <- data[sapply(data, is.numeric)]
numeric_data <- numeric_data[, !names(numeric_data) %in% c("rownames")]

#23BDS0257
#6. Standardize the data
numeric_data <- numeric_data[, sapply(numeric_data, function(x) all(is.finite(x)))]
numeric_data <- numeric_data[complete.cases(numeric_data), ]
scaled_data <- scale(numeric_data)

print(dim(scaled_data))
print(anyNA(scaled_data))
print(any(!is.finite(scaled_data)))

#23BDS0257
#7. Apply K-Means Clustering for center as 2
set.seed(123)
kmeans_2 <- kmeans(scaled_data, centers = 2, nstart = 25)

#23BDS0257
#8. Display K-Means results
print(kmeans_2)

#23BDS0257
#9. Display cluster centers
print(kmeans_2$centers)

#23BDS0257
#10. Calculate WCSS for K = 1 to 10
wcss <- numeric(10)

set.seed(123)
for (k in 1:10) {
  km <- kmeans(scaled_data, centers = k, nstart = 25)
  wcss[k] <- km$tot.withinss
}

print(wcss)

#23BDS0257
#11. Display the elbow plot
plot(1:10, wcss,
     type = "b",
     pch = 19,
     xlab = "Number of Clusters (K)",
     ylab = "WCSS",
     main = "23BDS0257 - Elbow Plot for K-Means")

#23BDS0257
#12. Apply K-Means clustering based on the elbow recommendation
set.seed(123)
kmeans_final <- kmeans(scaled_data, centers = 3, nstart = 25)

print(kmeans_final)
print(kmeans_final$centers)
table(kmeans_final$cluster)

#23BDS0257 Hierarchical Clustering
#1. Load the dataset shared through e-mail
data_hc <- read.csv("school93_98.csv")
head(data_hc)

#23BDS0257
#2. View the dataset
View(data_hc)
print(data_hc)

#23BDS0257
#3. Check missing values
print(colSums(is.na(data_hc)))

#23BDS0257
#4. Select only numerical attributes
numeric_hc <- data_hc[sapply(data_hc, is.numeric)]
numeric_hc <- numeric_hc[, !names(numeric_hc) %in% c("rownames")]
numeric_hc <- numeric_hc[, sapply(numeric_hc, function(x) all(is.finite(x)))]
numeric_hc <- na.omit(numeric_hc)
head(numeric_hc)

#23BDS0257
#5. Standardize the data
scaled_hc <- scale(numeric_hc)
head(scaled_hc)

#23BDS0257
#6. Calculate Euclidean distance
euclidean_dist <- dist(scaled_hc, method = "euclidean")
head(as.matrix(euclidean_dist))

#23BDS0257
#7. Perform Hierarchical Clustering using Ward's method
hc <- hclust(euclidean_dist, method = "ward.D2")
head(hc$merge)

#23BDS0257
#8. Plot the dendrogram
plot(hc,
     main = "23BDS0257 - Hierarchical Clustering Dendrogram",
     xlab = "Observations",
     ylab = "Height",
     sub = "")

#23BDS0257
#9. Divide the data into 3 clusters and display cluster assignments
hc_clusters <- cutree(hc, k = 3)
print(hc_clusters)

#23BDS0257
#10. Display number of samples in each cluster
print(table(hc_clusters))
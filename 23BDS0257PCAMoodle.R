#23BDS0257
# 1. Select numerical variables and examine their basic statistical characteristics
data <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]
summary(data)

#23BDS0257
# 2. Standardize the selected variables before applying PCA
# Standardization is required because the variables have different scales and units.
# It converts each variable to mean = 0 and standard deviation = 1,
# so that no variable dominates PCA due to its larger numerical scale.
data_scaled <- scale(data)
data_scaled

#23BDS0257
# 3. Perform PCA using prcomp()
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)
summary(pca_result)

#23BDS0257
# 4. Determine the proportion of variance explained by each principal component
variance_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
variance_explained

# Display variance explained as percentages
variance_percentage <- variance_explained * 100
variance_percentage

# Cumulative proportion of variance explained
cumulative_variance <- cumsum(variance_explained)
cumulative_variance

#23BDS0257
# 5. Identify the minimum number of PCs required to explain at least 80% variance
num_components <- which(cumulative_variance >= 0.80)[1]
num_components

#23BDS0257
# 6. Construct a scree plot and interpret the importance of PCs
par(mar = c(4, 4, 2, 1))
plot(variance_percentage, type = "b",
     main = "23BDS0257 Scree Plot",
     xlab = "Principal Components",
     ylab = "Percentage of Variance Explained",
     pch = 19)

#23BDS0257
# 7. Examine PCA loadings and identify variables contributing most strongly
# to PC1 and PC2
loadings <- pca_result$rotation[, 1:2]
loadings

# Absolute loadings to compare the strength of contribution
abs_loadings <- abs(loadings)
abs_loadings

#23BDS0257
# 8. Create a PCA biplot and interpret relationships among automobiles
# and original variables
biplot(pca_result,
       main = "23BDS0257 PCA Biplot of mtcars",
       col = c("black", "red"))
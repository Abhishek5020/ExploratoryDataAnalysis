#23BDS0257
# 1. Load the dataset and verify that the required packages are loaded

library(ggplot2)
library(ggfortify)
library(FactoMineR)
library(factoextra)

#Dataset was imported manually, use:
df <- read.csv(file.choose())
print("Required packages loaded successfully")


#23BDS0257
# 2. Display the total number of rows and columns in the dataset
cat("Total rows:", nrow(df), "\n")
cat("Total columns:", ncol(df), "\n")


#23BDS0257
# 3. Extract numeric columns, clean the data, and normalize using scale()
numeric_data <- df[, sapply(df, is.numeric), drop = FALSE]

# Remove columns containing only missing values
numeric_data <- numeric_data[, colSums(!is.na(numeric_data)) > 0, drop = FALSE]

# Replace missing values with the respective column mean
for (i in seq_along(numeric_data)) {
  numeric_data[[i]][is.na(numeric_data[[i]])] <-
    mean(numeric_data[[i]], na.rm = TRUE)
}

# Remove columns with zero variance
numeric_data <- numeric_data[
  , sapply(numeric_data, sd) > 0, drop = FALSE
]

# Normalize the numeric data
data_scaled <- scale(numeric_data)

print("First six rows of scaled data:")
print(head(data_scaled))


#23BDS0257
# 4. Perform Principal Component Analysis and print its summary

pca_result <- prcomp(data_scaled, center = FALSE, scale. = FALSE)

print(summary(pca_result))


#23BDS0257
# 5. Plot a scree plot showing percentage variance with data labels
# Set the y-axis limit from 0 to 60

variance_percent <- (pca_result$sdev^2 /
                       sum(pca_result$sdev^2)) * 100

scree_data <- data.frame(
  Component = paste0("PC", seq_along(variance_percent)),
  Variance = variance_percent
)

ggplot(scree_data, aes(x = Component, y = Variance)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = round(Variance, 2)), vjust = -0.4) +
  scale_y_continuous(limits = c(0, 60)) +
  labs(
    title = "23BDS0257 Scree Plot - Percentage of Variance",
    x = "Principal Components",
    y = "Percentage of Variance (%)"
  ) +
  theme_minimal()


#23BDS0257
# 6. Create a PCA biplot with non-overlapping labels and a proper title

fviz_pca_biplot(
  pca_result,
  geom.ind = "point",
  geom.var = "arrow",
  label = "var",
  repel = TRUE,
  title = "23BDS0257 PCA Biplot - School Dataset"
)

#23BDS0257
# 7. Extract PCA scores into pca_scores and display the first six rows

pca_scores <- as.data.frame(pca_result$x)

print("First six rows of PCA scores:")
print(head(pca_scores, 6))


#23BDS0257
# 8. Draw a scatter plot of PC1 vs PC2 with blue circular points

ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(color = "blue", shape = 16, size = 2.5) +
  labs(
    title = "PCA Scatter Plot - 23BDS0257",
    x = "Principal Component 1 (PC1)",
    y = "Principal Component 2 (PC2)"
  ) +
  theme_minimal()

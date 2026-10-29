# -----------------------------------------
# Question 1: Import the given dataset
# -----------------------------------------
print("23BDS0257")

dataset <- read.csv("school93_98.csv")

print("Dataset Imported Successfully")


# -----------------------------------------
# Question 2: Display the first 10 records
# -----------------------------------------
print("23BDS0257")

head(dataset, 10)


# -----------------------------------------
# Question 3: Display the last 10 records
# -----------------------------------------
print("23BDS0257")

tail(dataset, 10)


# -----------------------------------------
# Question 4: Find the total number of rows and columns
# -----------------------------------------
print("23BDS0257")

cat("Number of Rows =", nrow(dataset), "\n")
cat("Number of Columns =", ncol(dataset), "\n")


# -----------------------------------------
# Question 5: Display the names of all variables (columns)
# -----------------------------------------
print("23BDS0257")

colnames(dataset)


# -----------------------------------------
# Question 6: Display the data type (structure) of each variable
# -----------------------------------------
print("23BDS0257")

str(dataset)


# -----------------------------------------
# Question 7: Display the summary statistics of the dataset
# -----------------------------------------
print("23BDS0257")

summary(dataset)


# -----------------------------------------
# Question 8: Determine the number of missing values in each column
# -----------------------------------------
print("23BDS0257")

colSums(is.na(dataset))


# -----------------------------------------
# Question 9: Display the total number of missing values
# -----------------------------------------
print("23BDS0257")

sum(is.na(dataset))


# -----------------------------------------
# Question 10: Display the number of missing values in each column
# -----------------------------------------
print("23BDS0257")

sapply(dataset, function(x) sum(is.na(x)))

# -----------------------------------------
# Question 11: Identify rows containing at least one missing value
# -----------------------------------------
print("23BDS0257")

missing_rows <- dataset[!complete.cases(dataset), ]
head(missing_rows, 10)


# -----------------------------------------
# Question 12: Remove all rows containing missing values
# -----------------------------------------
print("23BDS0257")

clean_dataset <- na.omit(dataset)

head(clean_dataset, 10)


# -----------------------------------------
# Question 13: Replace missing values in a numerical column using Mean
# -----------------------------------------
print("23BDS0257")

dataset$math4[is.na(dataset$math4)] <-
  mean(dataset$math4, na.rm = TRUE)

head(dataset, 10)


# -----------------------------------------
# Question 14: Replace missing values in another numerical column using Median
# -----------------------------------------
print("23BDS0257")

dataset$exppp[is.na(dataset$exppp)] <-
  median(dataset$exppp, na.rm = TRUE)

head(dataset, 10)


# -----------------------------------------
# Question 15: Replace missing values in a categorical column using Mode
# -----------------------------------------
print("23BDS0257")

dataset$lunch <- as.factor(dataset$lunch)

mode_value <- names(sort(table(dataset$lunch), decreasing = TRUE))[1]

dataset$lunch[is.na(dataset$lunch)] <- mode_value

head(dataset, 10)


# -----------------------------------------
# Question 16: Forward Fill using zoo::na.locf()
# -----------------------------------------
print("23BDS0257")

library(zoo)

forward_fill <- dataset

forward_fill[] <- lapply(forward_fill, function(x)
  na.locf(x, na.rm = FALSE))

head(forward_fill, 10)


# -----------------------------------------
# Question 17: Backward Fill using zoo::na.locf()
# -----------------------------------------
print("23BDS0257")

backward_fill <- dataset

backward_fill[] <- lapply(backward_fill, function(x)
  na.locf(x, fromLast = TRUE, na.rm = FALSE))

head(backward_fill, 10)


# -----------------------------------------
# Question 18: Replace remaining missing values with 0 or "Unknown"
# -----------------------------------------
print("23BDS0257")

constant_fill <- dataset

constant_fill[] <- lapply(constant_fill, function(x)
{
  if(is.numeric(x))
  {
    x[is.na(x)] <- 0
  }
  else
  {
    x[is.na(x)] <- "Unknown"
  }
  x
})

head(constant_fill, 10)


# -----------------------------------------
# Question 19: Remove duplicate records
# -----------------------------------------
print("23BDS0257")

dataset_no_duplicates <- unique(dataset)

head(dataset_no_duplicates, 10)


# -----------------------------------------
# Question 20: Convert appropriate variables into Factors
# -----------------------------------------
print("23BDS0257")

dataset$distid <- as.factor(dataset$distid)
dataset$schid <- as.factor(dataset$schid)
dataset$year <- as.factor(dataset$year)
dataset$lunch <- as.factor(dataset$lunch)

str(dataset)

# -----------------------------------------
# Question 21: Rename one or more column names
# -----------------------------------------
print("23BDS0257")
library(dplyr)
dataset <- dataset |>
  rename(
    Enrollment = enrol,
    Math_Score = math4
  )

head(dataset, 10)


# -----------------------------------------
# Question 22: Create a new derived column
# -----------------------------------------
print("23BDS0257")

dataset$Expense_Per_Student <- dataset$exppp / dataset$Enrollment

head(dataset, 10)


# -----------------------------------------
# Question 23: Display summary statistics
# -----------------------------------------
print("23BDS0257")

summary(dataset)


# -----------------------------------------
# Question 24: Find the minimum and maximum values
# -----------------------------------------
print("23BDS0257")

cat("Minimum Math Score =", min(dataset$Math_Score, na.rm = TRUE), "\n")
cat("Maximum Math Score =", max(dataset$Math_Score, na.rm = TRUE), "\n")


# -----------------------------------------
# Question 25: Calculate the range
# -----------------------------------------
print("23BDS0257")

range(dataset$Math_Score, na.rm = TRUE)


# -----------------------------------------
# Question 26: Calculate the variance
# -----------------------------------------
print("23BDS0257")

var(dataset$Math_Score, na.rm = TRUE)


# -----------------------------------------
# Question 27: Calculate the standard deviation
# -----------------------------------------
print("23BDS0257")

sd(dataset$Math_Score, na.rm = TRUE)


# -----------------------------------------
# Question 28: Calculate the Interquartile Range (IQR)
# -----------------------------------------
print("23BDS0257")

IQR(dataset$Math_Score, na.rm = TRUE)
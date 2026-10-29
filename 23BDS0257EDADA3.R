# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)

# Import dataset
school <- read.csv("school93_98.csv", stringsAsFactors = FALSE)

# ============================================================
# 1. Import a dataset into R
# ============================================================
#23BDS0257
# Q1. Import a dataset into R.
school <- read.csv("school93_98.csv", stringsAsFactors = FALSE)
print(head(school))

# ============================================================
# 2. Display the first and last six records
# ============================================================
#23BDS0257
# Q2. Display the first and last six records of the dataset.
cat("\nFirst 6 records:\n")
print(head(school, 6))

cat("\nLast 6 records:\n")
print(tail(school, 6))

# ============================================================
# 3. Identify total number of missing values
# ============================================================
#23BDS0257
# Q3. Identify the total number of missing values in the dataset.
total_missing <- sum(is.na(school))
cat("\nTotal missing values:", total_missing, "\n")

# ============================================================
# 4. Display all records containing at least one missing value
# ============================================================
#23BDS0257
# Q4. Display all records containing at least one missing value.
missing_records <- school[rowSums(is.na(school)) > 0, ]
print(missing_records)

# ============================================================
# 5. Remove columns having more than 70% missing values
# ============================================================
#23BDS0257
# Q5. Remove columns having more than 70 percentage of missing values.
missing_percentage <- colMeans(is.na(school)) * 100

cat("\nMissing percentage of each column:\n")
print(missing_percentage)

school <- school[, missing_percentage <= 70]

cat("\nDataset after removing columns with >70% missing values:\n")
print(names(school))

# ============================================================
# 6. Replace missing numerical values with the mean
# ============================================================
#23BDS0257
# Q6. Replace missing numerical values with the mean.
numeric_columns <- names(school)[sapply(school, is.numeric)]

for (col in numeric_columns) {
  mean_value <- mean(school[[col]], na.rm = TRUE)
  school[[col]][is.na(school[[col]])] <- mean_value
}

cat("\nMissing numerical values replaced with mean.\n")

# ============================================================
# 7. Replace missing numerical values with the median
# ============================================================
#23BDS0257
# Q7. Replace missing numerical values with the median.
# Reload original dataset so Q7 can independently demonstrate median replacement.
school_median <- read.csv("school93_98.csv", stringsAsFactors = FALSE)

missing_percentage_median <- colMeans(is.na(school_median)) * 100
school_median <- school_median[, missing_percentage_median <= 70]

numeric_columns <- names(school_median)[sapply(school_median, is.numeric)]

for (col in numeric_columns) {
  median_value <- median(school_median[[col]], na.rm = TRUE)
  school_median[[col]][is.na(school_median[[col]])] <- median_value
}

cat("\nMissing numerical values replaced with median.\n")

# ============================================================
# 8. Replace missing categorical values with the mode
# ============================================================
#23BDS0257
# Q8. Replace missing categorical values with the mode.
school_mode <- read.csv("school93_98.csv", stringsAsFactors = FALSE)

get_mode <- function(x) {
  x <- x[!is.na(x)]
  unique_values <- unique(x)
  unique_values[which.max(tabulate(match(x, unique_values)))]
}

categorical_columns <- names(school_mode)[
  sapply(school_mode, function(x) is.character(x) || is.factor(x))
]

for (col in categorical_columns) {
  mode_value <- get_mode(school_mode[[col]])
  school_mode[[col]][is.na(school_mode[[col]])] <- mode_value
}

cat("\nMissing categorical values replaced with mode.\n")

# ============================================================
# 9. Identify duplicate rows
# ============================================================
#23BDS0257
# Q9. Identify duplicate rows if any.
duplicate_rows <- school[duplicated(school), ]

cat("\nDuplicate rows:\n")
print(duplicate_rows)

cat("\nNumber of duplicate rows:", sum(duplicated(school)), "\n")

# ============================================================
# 10. Remove duplicate rows
# ============================================================
#23BDS0257
# Q10. Remove the duplicate rows if any.
school <- school[!duplicated(school), ]

cat("\nNumber of rows after removing duplicates:", nrow(school), "\n")

# ============================================================
# 11. Identify columns stored with incorrect data types
# ============================================================
#23BDS0257
# Q11. Identify columns stored with incorrect data types.
cat("\nStructure of dataset:\n")
str(school)

cat("\nData types of columns:\n")
print(sapply(school, class))

# year should be numeric/integer
# Binary year indicator variables should also be numeric
# If any variable such as lunch is accidentally character, it can be converted to numeric.

if (is.character(school$lunch)) {
  school$lunch <- as.numeric(school$lunch)
}

# ============================================================
# 12. Convert character variables into factors
# ============================================================
#23BDS0257
# Q12. Convert character variables into factors if any.
character_columns <- names(school)[sapply(school, is.character)]

for (col in character_columns) {
  school[[col]] <- as.factor(school[[col]])
}

cat("\nCharacter variables converted to factors.\n")
str(school)

# ============================================================
# 13. Convert text values into lowercase/uppercase
# ============================================================
#23BDS0257
# Q13. Convert text values into lowercase/uppercase.

# Demonstration using character columns before converting to factor
school_text <- read.csv("school93_98.csv", stringsAsFactors = FALSE)

character_columns <- names(school_text)[sapply(school_text, is.character)]

for (col in character_columns) {
  school_text[[col]] <- str_to_lower(school_text[[col]])
}

cat("\nCharacter values converted to lowercase.\n")
print(head(school_text))

# To convert text to uppercase:
for (col in character_columns) {
  school_text[[col]] <- str_to_upper(school_text[[col]])
}

cat("\nCharacter values converted to uppercase.\n")
print(head(school_text))

# ============================================================
# 14. Detect outliers using boxplots
# ============================================================
#23BDS0257
# Q14. Detect outliers using boxplots.

ggplot(school, aes(y = exppp)) +
  geom_boxplot() +
  labs(title = "Boxplot of Expenditure per Pupil",
       y = "Expenditure per Pupil")

ggplot(school, aes(y = math4)) +
  geom_boxplot() +
  labs(title = "Boxplot of Math4",
       y = "Math4")

# ============================================================
# 15. Identify outliers using IQR method
# ============================================================
#23BDS0257
# Q15. Identify outliers using the interquartile range method.

Q1 <- quantile(school$exppp, 0.25, na.rm = TRUE)
Q3 <- quantile(school$exppp, 0.75, na.rm = TRUE)
IQR_value <- IQR(school$exppp, na.rm = TRUE)

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers <- school %>%
  filter(exppp < lower_bound | exppp > upper_bound)

cat("\nLower bound:", lower_bound, "\n")
cat("Upper bound:", upper_bound, "\n")
cat("Outliers in exppp:\n")
print(outliers)

# ============================================================
# 16. Remove extreme outliers
# ============================================================
#23BDS0257
# Q16. Remove extreme outliers from the dataset.

school_clean <- school %>%
  filter(exppp >= lower_bound & exppp <= upper_bound)

cat("\nRows before removing outliers:", nrow(school), "\n")
cat("Rows after removing outliers:", nrow(school_clean), "\n")

# ============================================================
# 17. Create histogram and interpret distribution
# ============================================================
#23BDS0257
# Q17. Create a histogram for a numerical variable and interpret its distribution.

ggplot(school_clean, aes(x = exppp)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of Expenditure per Pupil",
       x = "Expenditure per Pupil",
       y = "Frequency")

# Interpretation:
# The histogram shows how expenditure per pupil is distributed.
# If values are concentrated around the centre, the distribution is concentrated.
# A long tail on one side indicates skewness.

# ============================================================
# 18. Add density curve to histogram
# ============================================================
#23BDS0257
# Q18. Add a density curve to the histogram.

ggplot(school_clean, aes(x = exppp)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30) +
  geom_density() +
  labs(title = "Histogram with Density Curve",
       x = "Expenditure per Pupil",
       y = "Density")

# ============================================================
# 19. Create a violin plot
# ============================================================
#23BDS0257
# Q19. Create a violin plot.

ggplot(school_clean, aes(x = factor(year), y = exppp)) +
  geom_violin() +
  labs(title = "Violin Plot of Expenditure by Year",
       x = "Year",
       y = "Expenditure per Pupil")

# ============================================================
# 20. Create a dot plot for a small numerical dataset
# ============================================================
#23BDS0257
# Q20. Create a dot plot for a small numerical dataset.

small_data <- data.frame(
  value = school_clean$math4[1:min(30, nrow(school_clean))]
)

ggplot(small_data, aes(x = value)) +
  geom_dotplot(binwidth = 2) +
  labs(title = "Dot Plot of Math4",
       x = "Math4",
       y = "Count")

# ============================================================
# 21. Create frequency table for categorical variable
# ============================================================
#23BDS0257
# Q21. Create a frequency table for a categorical variable.

frequency_table <- table(school_clean$year)
cat("\nFrequency table of year:\n")
print(frequency_table)

# ============================================================
# 22. Calculate proportion and percentage of each category
# ============================================================
#23BDS0257
# Q22. Calculate the proportion and percentage of each category.

proportion_table <- prop.table(frequency_table)
percentage_table <- proportion_table * 100

cat("\nProportion of each year:\n")
print(proportion_table)

cat("\nPercentage of each year:\n")
print(percentage_table)

# ============================================================
# 23. Create a bar chart showing frequency of each category
# ============================================================
#23BDS0257
# Q23. Create a bar chart showing the frequency of each category.

ggplot(school_clean, aes(x = factor(year))) +
  geom_bar() +
  labs(title = "Frequency of Schools by Year",
       x = "Year",
       y = "Frequency")

# ============================================================
# 24. Create horizontal bar chart for several categories
# ============================================================
#23BDS0257
# Q24. Create a horizontal bar chart for a variable with several categories.

ggplot(school_clean, aes(x = factor(year))) +
  geom_bar() +
  coord_flip() +
  labs(title = "Horizontal Bar Chart of Year",
       x = "Year",
       y = "Frequency")

# ============================================================
# 25. Create percentage bar chart
# ============================================================
#23BDS0257
# Q25. Create a percentage bar chart instead of a frequency bar chart.

ggplot(school_clean, aes(x = factor(year), y = after_stat(count / sum(count)))) +
  geom_bar() +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage Bar Chart of Year",
       x = "Year",
       y = "Percentage")

# ============================================================
# 26. Create pie chart for categorical variable
# ============================================================
#23BDS0257
# Q26. Create a pie chart for a categorical variable with a small number of categories.

year_frequency <- school_clean %>%
  count(year)

ggplot(year_frequency, aes(x = "", y = n, fill = factor(year))) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Pie Chart of Schools by Year",
       fill = "Year") +
  theme_void()

# ============================================================
# 27. Create lollipop chart for categorical frequencies
# ============================================================
#23BDS0257
# Q27. Create a lollipop chart for categorical frequencies.

year_frequency <- school_clean %>%
  count(year)

ggplot(year_frequency, aes(x = factor(year), y = n)) +
  geom_segment(aes(xend = factor(year), y = 0, yend = n)) +
  geom_point(size = 3) +
  labs(title = "Lollipop Chart of Year Frequencies",
       x = "Year",
       y = "Frequency")

# ============================================================
# 28. Create a dot chart
# ============================================================
#23BDS0257
# Q28. Create a dot chart.

ggplot(year_frequency, aes(x = n, y = factor(year))) +
  geom_point(size = 4) +
  labs(title = "Dot Chart of Year Frequencies",
       x = "Frequency",
       y = "Year")

# ============================================================
# 29. Create a density plot using ggplot2
# ============================================================
#23BDS0257
# Q29. Create a density plot using ggplot2.

ggplot(school_clean, aes(x = math4)) +
  geom_density() +
  labs(title = "Density Plot of Math4",
       x = "Math4",
       y = "Density")

# ============================================================
# 30. Use facet_wrap() to display distribution across groups
# ============================================================
#23BDS0257
# Q30. Use facet_wrap() to display the distribution of one variable across different groups while interpreting each distribution separately.

ggplot(school_clean, aes(x = exppp)) +
  geom_histogram(bins = 20) +
  facet_wrap(~year) +
  labs(title = "Distribution of Expenditure per Pupil Across Years",
       x = "Expenditure per Pupil",
       y = "Frequency")

# Interpretation:
# Each panel represents the distribution of expenditure per pupil
# for a particular year.
# The shape, centre, spread and possible skewness can be compared
# separately across the different years.
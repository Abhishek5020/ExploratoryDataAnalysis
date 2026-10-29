#23BDS0257
# Question 1: Load the dataset shared via email
school93_98 <- read.csv("school93_98.csv")
head(school93_98)
str(school93_98)
summary(school93_98)


#23BDS0257
# Question 2: Compute summary statistics: Mean, Median, Minimum, Maximum, Range, Variance, Standard Deviation, IQR, Skewness and Kurtosis
#install.packages("e1071")
library(e1071)
x <- school93_98$exppp
mean_value <- mean(x, na.rm = TRUE)
median_value <- median(x, na.rm = TRUE)
minimum_value <- min(x, na.rm = TRUE)
maximum_value <- max(x, na.rm = TRUE)
range_value <- diff(range(x, na.rm = TRUE))
variance_value <- var(x, na.rm = TRUE)
sd_value <- sd(x, na.rm = TRUE)
iqr_value <- IQR(x, na.rm = TRUE)
skewness_value <- skewness(x, na.rm = TRUE)
kurtosis_value <- kurtosis(x, na.rm = TRUE)
summary_stats <- c(Mean = mean_value, Median = median_value, Minimum = minimum_value, 
                   Maximum = maximum_value, Range = range_value, Variance = variance_value, 
                   Standard_Deviation = sd_value, IQR = iqr_value, Skewness = skewness_value, 
                   Kurtosis = kurtosis_value)
summary_stats

#23BDS0257
# Question 3: Store the computed values in a single structured data.frame or named vector and print it
summary_stats_df <- data.frame(Statistic = names(summary_stats), Value = as.numeric(summary_stats))
print(summary_stats_df)


#23BDS0257
# Question 4: Create a frequency distribution table and relative frequency distribution table for a categorical variable and generate a Pie Chart and Bar Plot
school93_98$year <- as.factor(school93_98$year)
year_frequency <- table(school93_98$year)
year_frequency
year_relative_frequency <- prop.table(year_frequency)
year_relative_frequency
pie(year_frequency, main = "23BDS0257 Distribution of Schools by Year", labels = paste(names(year_frequency), year_frequency))
barplot(year_frequency, main = "23BDS0257 Frequency Distribution of Year", xlab = "Year", ylab = "Frequency", col = "skyblue")


#23BDS0257
# Question 5: Create a frequency distribution table with bin intervals for a numerical variable and plot its Histogram and Box Plot to check for outliers
enrol_breaks <- seq(floor(min(school93_98$enrol, na.rm = TRUE)), ceiling(max(school93_98$enrol, na.rm = TRUE)), length.out = 8)
enrol_bins <- cut(school93_98$enrol, breaks = enrol_breaks, include.lowest = TRUE)
enrol_frequency <- table(enrol_bins)
enrol_frequency
hist(school93_98$enrol, breaks = 8, main = "23BDS0257 Histogram of Enrollment", xlab = "Enrollment", ylab = "Frequency", col = "lightblue")
boxplot(school93_98$enrol, main = "23BDS0257 Box Plot of Enrollment", ylab = "Enrollment", col = "lightgreen")


#23BDS0257
# Question 6: Choose two categorical variables, construct a Contingency Table and compute row and column percentages using prop.table()
school93_98$lunch_cat <- cut(school93_98$lunch, breaks = 3, labels = c("Low", "Medium", "High"))
contingency_table <- table(school93_98$year, school93_98$lunch_cat)
contingency_table
row_percentages <- prop.table(contingency_table, margin = 1) * 100
row_percentages
column_percentages <- prop.table(contingency_table, margin = 2) * 100
column_percentages


#23BDS0257
# Question 7: Create a Stacked Bar Plot or Grouped Bar Plot to visualize the relationship
barplot(contingency_table, beside = FALSE, main = "23BDS0257 Year and Lunch Category", xlab = "Year", ylab = "Frequency", legend.text = TRUE, col = c("lightblue", "lightgreen", "lightpink"))
barplot(contingency_table, beside = TRUE, main = "23BDS0257 Grouped Bar Plot of Year and Lunch Category", xlab = "Year", ylab = "Frequency", legend.text = TRUE, col = c("lightblue", "lightgreen", "lightpink"))

# Question 8: Select one categorical variable and one numerical variable and compute mean, median and standard deviation grouped by each category using aggregate()
mean_by_year <- aggregate(exppp ~ year, data = school93_98, FUN = mean, na.rm = TRUE)
median_by_year <- aggregate(exppp ~ year, data = school93_98, FUN = median, na.rm = TRUE)
sd_by_year <- aggregate(exppp ~ year, data = school93_98, FUN = sd, na.rm = TRUE)
mean_by_year
median_by_year
sd_by_year


#23BDS0257
# Question 9: Generate Side-by-Side Box Plots and a Violin Plot to visualize the numerical distribution across categories
boxplot(exppp ~ year, data = school93_98, main = "23BDS0257 Side-by-Side Box Plot of Expenditure by Year", xlab = "Year", ylab = "Expenditure per Pupil", col = "lightblue")
#install.packages("ggplot2")
library(ggplot2)
ggplot(school93_98, aes(x = year, y = exppp)) + geom_violin(fill = "lightblue") + labs(title = "23BDS0257 Violin Plot of Expenditure by Year", x = "Year", y = "Expenditure per Pupil")


#23BDS0257
# Question 10: Select two numerical variables, calculate Pearson and Spearman Correlation Coefficients and construct a Scatter Plot with a fitted linear regression trend line
pearson_correlation <- cor(school93_98$exppp, school93_98$math4, use = "complete.obs", method = "pearson")
spearman_correlation <- cor(school93_98$exppp, school93_98$math4, use = "complete.obs", method = "spearman")
pearson_correlation
spearman_correlation
plot(school93_98$exppp, school93_98$math4, main = "23BDS0257 Expenditure vs Mathematics Score", xlab = "Expenditure per Pupil", ylab = "Math Score", pch = 19)
abline(lm(math4 ~ exppp, data = school93_98), col = "red", lwd = 2)


#23BDS0257
# Question 11: Select two categorical features and one numerical feature, calculate Mean and Standard Deviation across all combinations and create a Faceted Box Plot and Grouped Bar Chart with Error Bars
summary_combinations <- aggregate(exppp ~ year + lunch_cat, data = school93_98, FUN = function(x) c(Mean = mean(x, na.rm = TRUE), SD = sd(x, na.rm = TRUE)))
summary_combinations
ggplot(school93_98, aes(x = lunch_cat, y = exppp)) + geom_boxplot() + facet_wrap(~ year) + labs(title = "23BDS0257 Faceted Box Plot of Expenditure", x = "Lunch Category", y = "Expenditure per Pupil")
group_summary <- aggregate(exppp ~ year + lunch_cat, data = school93_98, FUN = mean, na.rm = TRUE)
group_sd <- aggregate(exppp ~ year + lunch_cat, data = school93_98, FUN = sd, na.rm = TRUE)
group_summary$SD <- group_sd$exppp
ggplot(group_summary, aes(x = year, y = exppp, fill = lunch_cat)) + geom_bar(stat = "identity", position = "dodge") + geom_errorbar(aes(ymin = exppp - SD, ymax = exppp + SD), position = position_dodge(width = 0.9), width = 0.2) + labs(title = "23BDS0257 Grouped Bar Chart with Error Bars", x = "Year", y = "Mean Expenditure per Pupil")


#23BDS0257
# Question 12: Select one categorical variable and two numerical variables, construct a Scatter Plot with points colored or shaped by the categorical feature and compute correlation separately for each category level
ggplot(school93_98, aes(x = exppp, y = math4, color = lunch_cat, shape = lunch_cat)) + geom_point() + labs(title = "23BDS0257 Expenditure vs Math Score by Lunch Category", x = "Expenditure per Pupil", y = "Math Score")
correlation_by_lunch <- by(school93_98, school93_98$lunch_cat, function(data) cor(data$exppp, data$math4, use = "complete.obs"))
correlation_by_lunch


#23BDS0257
# Question 13: Select at least 3 numerical features, generate a full Correlation Matrix and plot it as a Heatmap
numeric_data <- school93_98[, c("enrol", "exppp", "math4")]
correlation_matrix <- cor(numeric_data, use = "complete.obs", method = "pearson")
correlation_matrix
heatmap(correlation_matrix, main = "23BDS0257 Correlation Matrix Heatmap", symm = TRUE, col = heat.colors(20))


#23BDS0257
# Question 14: Produce a Pairs Plot / Scatterplot Matrix to examine pairwise relationships and distributions simultaneously
pairs(school93_98[, c("enrol", "exppp", "math4")], main = "23BDS0257 Scatterplot Matrix of Numerical Variables")
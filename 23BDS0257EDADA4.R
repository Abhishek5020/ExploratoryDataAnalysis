# Load required libraries
library(ggplot2)
library(corrplot)

# Load the dataset
data <- read.csv("school93_98.csv")

# Convert year into a categorical variable
data$year_factor <- factor(data$year)

# Create enrolment groups for categorical analysis
data$enrol_group <- cut(data$enrol,
                        breaks = 3,
                        labels = c("Low", "Medium", "High"))

# Q1: Load the dataset into R and examine the variables for bivariate analysis.
print("23BDS0257")

str(data)
summary(data)


# Q2: Select two categorical variables, create a contingency table, and display their relationship using Base R and ggplot2 stacked bar plots.
print("23BDS0257")

contingency_table <- table(data$year_factor, data$enrol_group)
print(contingency_table)

barplot(contingency_table,
        main = "23BDS0257 - Stacked Bar Plot: Year vs Enrolment Group",
        xlab = "Enrolment Group",
        ylab = "Frequency",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow", "lightgray", "lightcyan"),
        legend = rownames(contingency_table))

ggplot(data, aes(x = enrol_group, fill = year_factor)) +
  geom_bar() +
  labs(title = "23BDS0257 - Stacked Bar Plot: Year vs Enrolment Group",
       x = "Enrolment Group",
       y = "Frequency",
       fill = "Year") +
  theme_minimal()


# Q3: Select two categorical variables and create a grouped side-by-side bar plot to compare their frequency distributions.
print("23BDS0257")

ggplot(data, aes(x = enrol_group, fill = year_factor)) +
  geom_bar(position = "dodge") +
  labs(title = "23BDS0257 - Grouped Bar Plot: Year vs Enrolment Group",
       x = "Enrolment Group",
       y = "Frequency",
       fill = "Year") +
  theme_minimal()


# Q4: Select two numerical variables and generate a scatter plot to analyze their relationship.
print("23BDS0257")

ggplot(data, aes(x = enrol, y = exppp)) +
  geom_point(alpha = 0.6) +
  labs(title = "23BDS0257 - Scatter Plot: Enrolment vs Expenditure",
       x = "Enrolment",
       y = "Expenditure per Pupil") +
  theme_minimal()


# Q5: Select two appropriate variables and construct a line plot to visualize their relationship or trend.
print("23BDS0257")

year_summary <- aggregate(exppp ~ year, data = data, FUN = mean, na.rm = TRUE)

ggplot(year_summary, aes(x = year, y = exppp)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "23BDS0257 - Line Plot: Average Expenditure by Year",
       x = "Year",
       y = "Average Expenditure per Pupil") +
  theme_minimal()


# Q6: Select suitable numerical variables and generate a heatmap to represent the relationships among the variables.
print("23BDS0257")

numeric_data <- data[, c("lunch", "enrol", "exppp", "math4")]
cor_matrix <- cor(numeric_data, use = "complete.obs")

heatmap(cor_matrix,
        main = "23BDS0257 - Correlation Heatmap",
        xlab = "Variables",
        ylab = "Variables",
        col = heat.colors(20),
        symm = TRUE)


# Q7: Select suitable variables and construct a bar chart to compare values or frequencies between two variables.
print("23BDS0257")

year_summary <- aggregate(cbind(enrol, exppp) ~ year,
                          data = data,
                          FUN = mean,
                          na.rm = TRUE)

bar_data <- rbind(year_summary$enrol,
                  year_summary$exppp / 10)

barplot(bar_data,
        beside = TRUE,
        names.arg = year_summary$year,
        main = "23BDS0257 - Average Enrolment and Expenditure by Year",
        xlab = "Year",
        ylab = "Average Value",
        col = c("lightblue", "lightgreen"),
        legend.text = c("Enrolment", "Expenditure / 10"))


# Q8: Select one numerical variable and one categorical variable and generate a density plot comparing the numerical distribution across categories.
print("23BDS0257")

ggplot(data, aes(x = exppp, fill = year_factor)) +
  geom_density(alpha = 0.4, na.rm = TRUE) +
  labs(title = "23BDS0257 - Density Plot: Expenditure Distribution by Year",
       x = "Expenditure per Pupil",
       y = "Density",
       fill = "Year") +
  theme_minimal()


# Q9: Select one categorical variable and one numerical variable and create a box plot comparing the numerical distribution across groups.
print("23BDS0257")

ggplot(data, aes(x = year_factor, y = exppp)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "23BDS0257 - Box Plot: Expenditure Distribution by Year",
       x = "Year",
       y = "Expenditure per Pupil") +
  theme_minimal()


# Q10: Select one categorical variable and one numerical variable and construct a violin plot to visualize distribution and density across categories.
print("23BDS0257")

ggplot(data, aes(x = year_factor, y = exppp)) +
  geom_violin(na.rm = TRUE) +
  labs(title = "23BDS0257 - Violin Plot: Expenditure Distribution by Year",
       x = "Year",
       y = "Expenditure per Pupil") +
  theme_minimal()


# Q11: Combine a violin plot and box plot to compare the distribution of a numerical variable across categories.
print("23BDS0257")

ggplot(data, aes(x = year_factor, y = exppp)) +
  geom_violin(fill = "lightblue", na.rm = TRUE) +
  geom_boxplot(width = 0.15, fill = "white", na.rm = TRUE) +
  labs(title = "23BDS0257 - Violin and Box Plot: Expenditure by Year",
       x = "Year",
       y = "Expenditure per Pupil") +
  theme_minimal()


# Q12: Compute the correlation matrix for suitable numerical variables and generate a correlogram to study relationship strength and direction.
print("23BDS0257")

numeric_data <- data[, c("lunch", "enrol", "exppp", "math4")]
cor_matrix <- cor(numeric_data, use = "complete.obs")

print(cor_matrix)

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         title = "23BDS0257 - Correlogram",
         mar = c(0, 0, 2, 0))


# Q13: Add suitable titles, axis labels, legends, and graphical parameters to all visualizations.
print("23BDS0257")

print("All plots include appropriate titles, axis labels, legends, and graphical parameters.")
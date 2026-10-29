#Dataset contain all numerical variables.
#So for the plots, treating:
#year → categorical
#distid → categorical
#schid → categorical

# Q1: Load the dataset and identify numerical and categorical variables.
print("23BDS0257")

data <- read.csv("school93_98.csv")
str(data)
summary(data)

numerical_vars <- names(data)[sapply(data, is.numeric)]
categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]

print("Numerical Variables:")
print(numerical_vars)

print("Categorical Variables:")
print(categorical_vars)

# Q2: Create a scatter plot using two numerical variables and represent the third variable using color.
print("23BDS0257")

library(ggplot2)
ggplot(data, aes(x = enrol, y = exppp, color = math4)) +
  geom_point() +
  labs(title = "23BDS0257 Enrolment vs Expenditure per Pupil",
       x = "Enrolment",
       y = "Expenditure per Pupil",
       color = "Math Score") +
  theme_minimal()

# Q3: Create a scatter plot using two numerical variables, color for the third variable, and shape for the fourth variable.
print("23BDS0257")
data$lunch_group <- cut(data$lunch,
                        breaks = 3,
                        labels = c("Low", "Medium", "High"))
ggplot(data, aes(x = enrol, y = exppp,
                 color = factor(year),
                 shape = lunch_group)) +
  geom_point(size = 2.5, alpha = 0.7) +
  labs(title = "23BDS0257  Enrolment vs Expenditure by Year and Lunch Group",
       x = "Enrolment",
       y = "Expenditure per Pupil",
       color = "Year",
       shape = "Lunch Group") +
  theme_minimal()

# Q4: Create a scatter plot using two numerical variables, color for the third variable, and point size for the fourth numerical variable.
print("23BDS0257")

ggplot(data, aes(x = enrol, y = exppp, color = math4, size = lunch)) +
  geom_point(alpha = 0.7) +
  labs(title = "23BDS0257  Enrolment vs Expenditure with Math Score and Lunch",
       x = "Enrolment",
       y = "Expenditure per Pupil",
       color = "Math Score",
       size = "Lunch") +
  theme_minimal()

# Q5: Construct a bubble plot using three numerical variables with the third variable determining bubble size.
print("23BDS0257")

ggplot(data, aes(x = enrol, y = math4, size = exppp)) +
  geom_point(alpha = 0.6) +
  labs(title = "23BDS0257  Bubble Plot of Enrolment, Math Score and Expenditure",
       x = "Enrolment",
       y = "Math Score",
       size = "Expenditure per Pupil") +
  theme_minimal()

# Q6: Generate subplots using facet_wrap() based on an appropriate categorical variable.
print("23BDS0257")

ggplot(data, aes(x = enrol, y = math4)) +
  geom_point() +
  facet_wrap(~year) +
  labs(title = "23BDS0257  Enrolment vs Math Score Across Years",
       x = "Enrolment",
       y = "Math Score") +
  theme_minimal()

# Q7: Generate subplots using facet_grid() with two categorical variables arranged in rows and columns.
print("23BDS0257")

data$enrol_group <- cut(data$enrol,
                        breaks = 3,
                        labels = c("Low", "Medium", "High"))

ggplot(data, aes(x = exppp, y = math4)) +
  geom_point(na.rm = TRUE) +
  facet_grid(factor(year) ~ enrol_group) +
  labs(title = "23BDS0257  Math Score vs Expenditure by Year and Enrolment Group",
       x = "Expenditure per Pupil",
       y = "Math Score") +
  theme_minimal()

# Q8: Construct a pairs plot using at least four numerical variables to identify relationships and correlations.
print("23BDS0257")

pairs(data[, c("enrol", "exppp", "math4", "lunch")],
      main = "23BDS0257  Pairs Plot of School Variables",
      pch = 19)

# Q9: Create a parallel coordinate plot using four numerical variables and differentiate observations using a categorical variable.
print("23BDS0257")

library(GGally)

data_q9 <- data[, c("enrol", "exppp", "math4", "lunch", "year")]
data_q9 <- na.omit(data_q9)
data_q9$year <- factor(data_q9$year)

ggparcoord(data_q9,
           columns = 1:4,
           groupColumn = 5,
           scale = "uniminmax",
           alphaLines = 0.4) +
  labs(title = "23BDS0257  Parallel Coordinate Plot of School Variables",
       x = "Variables",
       y = "Scaled Values",
       color = "Year") +
  theme_minimal()

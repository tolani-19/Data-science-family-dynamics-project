# Load necessary libraries
library(ggplot2)
library(corrplot)

# Load the datasets
d1 <- read.table("/Users/motolanikay-salami/Downloads/student+performance/student/student-mat.csv", 
                 sep = ";", header = TRUE)

d2 <- read.table("/Users/motolanikay-salami/Downloads/student+performance (1)/student/student-por.csv", 
                 sep = ";", header = TRUE)

# Merge the two datasets on common columns
d3 <- merge(d1, d2, by = c("school", "sex", "age", "address", "famsize", "Pstatus", 
                           "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet"))

# Combine G1, G2, and G3 from both datasets into one column for each course
d3$G1_total <- d3$G1.x + d3$G1.y
d3$G2_total <- d3$G2.x + d3$G2.y
d3$G3_total <- d3$G3.x + d3$G3.y


# Initialize a new column for family income score
d3$family_income_score <- 0

# Add points based on family size (famsize)
# If family size is greater than 3, add 1 point
d3$family_income_score <- ifelse(d3$famsize == "GT3", d3$family_income_score + 1, d3$family_income_score)

# Add points for mother's education (Medu)
# Higher education adds more points
d3$family_income_score <- d3$family_income_score + d3$Medu

# Add points for father's education (Fedu)
# Higher education adds more points
d3$family_income_score <- d3$family_income_score + d3$Fedu

# Add points for mother's job (Mjob)
# Certain jobs give higher points (e.g., teacher, health worker)
d3$family_income_score <- d3$family_income_score + ifelse(d3$Mjob == "teacher", 4,
                                                          ifelse(d3$Mjob == "health", 4,
                                                                 ifelse(d3$Mjob == "services", 2, 
                                                                        ifelse(d3$Mjob == "at_home", 1, 2))))

# Add points for father's job (Fjob)
# Similarly, certain jobs give higher points (e.g., teacher, health worker)
d3$family_income_score <- d3$family_income_score + ifelse(d3$Fjob == "teacher", 4,
                                                          ifelse(d3$Fjob == "health", 4,
                                                                 ifelse(d3$Fjob == "services", 2, 
                                                                        ifelse(d3$Fjob == "at_home", 1, 2))))

# Print the first few rows to see the results
head(d3)


# Visualize the distribution of family income score and G3 (final grade)
# Histogram for Family Income Score
ggplot(d3, aes(x = family_income_score)) +
  geom_histogram(bins = 15, fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Family Income Score", x = "Family Income Score", y = "Count")

# Histogram for G3(Final Grade)
ggplot(d3, aes(x = G3_total)) +
  geom_histogram(bins = 15, fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Final Grade (G3)", x = "Final Grade (G3)", y = "Count")


# Create a scatter plot to visualize the relationship between Family Income Score and G3
ggplot(d3, aes(x = family_income_score, y = G3_total)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(title = "Family Income Score vs Final Grade",
       x = "Family Income Score", y = "Final Grade (G3)")


# Fit the linear model
model <- lm(G3_total ~ family_income_score, data = d3)

# Add Residuals vs Fitted Values plot
ggplot(data = data.frame(Fitted = fitted(model), Residuals = resid(model)), 
       aes(x = Fitted, y = Residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()

qqnorm(resid(model))
qqline(resid(model), col = "red")







# Load required libraries
library(dplyr)
library(ggplot2)

# Load the datasets
d1 <- read.table("/Users/motolanikay-salami/Downloads/student+performance/student/student-mat.csv",  sep = ";", header = TRUE)

d2 <- read.table("/Users/motolanikay-salami/Downloads/student+performance (1)/student/student-por.csv",  sep = ";", header = TRUE)

# Merge the two datasets on common columns
d3 <- merge(d1, d2, by = c("school", "sex", "age", "address", "famsize", "Pstatus", 
                           "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet"))

# Combine G1, G2, and G3 from both datasets into one column for each course, to get total scores
d3$G1_total <- d3$G1.x + d3$G1.y
d3$G2_total <- d3$G2.x + d3$G2.y
d3$G3_total <- d3$G3.x + d3$G3.y


# Merge the 'famsup.x' and 'famsup.y' columns into a new column 'famsup_combined'
d3$famsup_combined <- ifelse(d3$famsup.x == "yes" | d3$famsup.y == "yes", "yes", "no")
# Fit the model
model <- lm(G3_total ~ famsup_combined, data = d3)

# Plot residuals vs fitted values
library(ggplot2)
ggplot(data = data.frame(fitted = fitted(model), residuals = residuals(model)), 
       aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.6) +  # Scatter plot of residuals
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +  # Horizontal line at 0
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()
qqnorm(resid(model))
qqline(resid(model), col = "red")

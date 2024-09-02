# Load required libraries
library(tidyverse) # For data manipulation and visualization
library(dplyr)
#library(GGally)   # For pairwise plots
library(scales)
library(ggplot2)
library(reshape2)


Original_Data <- read.csv("KidneyData.csv")

# Fit a logistic regression model
logistic_model <- glm(Target_Variable ~ ., data = Original_Data, family = binomial)

# Print the summary of the model
summary(logistic_model)

# Explore the distribution of the target variable
ggplot(Original_Data, aes(x = Target_Variable)) +
  geom_bar(fill = "steelblue") +
  labs(x = "Target Variable", y = "Count") +
  ggtitle("Distribution of Target Variable")

# Explore the relationship between the target variable and other variables
# using pairwise scatter plots
ggplot(Original_Data, aes(x = Target_Variable, y = .)) +
  geom_point() +
  facet_wrap(~., scales = "free") +
  labs(x = "Target Variable", y = "Other Variables") +
  ggtitle("Relationship between Target Variable and Other Variables")

# Calculate and visualize the correlation matrix
cor_matrix <- cor(Original_Data)
ggplot(melt(cor_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Variables", y = "Variables", fill = "Correlation") +
  ggtitle("Correlation Matrix")
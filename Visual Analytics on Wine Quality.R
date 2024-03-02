library(tidyverse)
library(gridExtra)
library(ggplot2)
library(corrplot)
library(reshape2)
library(broom)

is.na(winequality.red)
is.na(winequality.white)

summary(winequality.red)
summary(winequality.white)

# RED WINE
corr_matrix <- cor(winequality.red)
corrplot(corr_matrix, method = "number", type = "full", tl.cex = 0.7)

# Fixed acidity VS Quality
ggplot(winequality.red, aes(x = quality, y = fixed.acidity, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Fixed acidity") +
  scale_fill_discrete(name = "Quality rating")

# Volatile acidity VS Quality
ggplot(winequality.red, aes(x = quality, y = volatile.acidity, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Volatile acidity") +
  scale_fill_discrete(name = "Quality rating")

# Citric Acid VS Quality
ggplot(winequality.red, aes(x = quality, y = citric.acid, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Citric Acid") +
  scale_fill_discrete(name = "Quality rating")

# Residual Sugar VS Quality
ggplot(winequality.red, aes(x = quality, y = residual.sugar, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Residual Sugar") +
  scale_fill_discrete(name = "Quality rating")

# Chlorides VS Quality
ggplot(winequality.red, aes(x = quality, y = chlorides, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Chlorides") +
  scale_fill_discrete(name = "Quality rating")

# Free Sulfur Dioxide VS Quality
ggplot(winequality.red, aes(x = quality, y = free.sulfur.dioxide, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Free Sulfur Dioxide") +
  scale_fill_discrete(name = "Quality rating")

# Total Sulfur Dioxide VS Quality
ggplot(winequality.red, aes(x = quality, y = total.sulfur.dioxide, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Total Sulfur Dioxide") +
  scale_fill_discrete(name = "Quality rating")

# Density VS Quality
ggplot(winequality.red, aes(x = quality, y = density, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Density") +
  scale_fill_discrete(name = "Quality rating")

# pH VS Quality
ggplot(winequality.red, aes(x = quality, y = pH, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "pH") +
  scale_fill_discrete(name = "Quality rating")

# Sulphates VS Quality
ggplot(winequality.red, aes(x = quality, y = sulphates, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Sulphates") +
  scale_fill_discrete(name = "Quality rating")

# Alcohol VS Quality
ggplot(winequality.red, aes(x = quality, y = alcohol, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Alcohol") +
  scale_fill_discrete(name = "Quality rating")


# Distribution of quality to check whether there is a bias or not
ggplot(winequality.red, aes(x = quality)) +
  geom_histogram(binwidth = 1, color = "black", fill = "yellow") +
  ggtitle("Distribution of Red Wine Quality")


# Melt correlation matrix into long format
cor_matrix_melted <- melt(corr_matrix)

# Create heatmap of correlation matrix
ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Correlation Matrix Heatmap Between Variables")


# Split the data into training and testing sets
set.seed(37)
train_indices <- sample(nrow(winequality.red), 0.7*nrow(winequality.red))
train_data <- winequality.red[train_indices,]
test_data <- winequality.red[-train_indices,]

# Fit a linear regression model to predict red wine quality based on chemical properties
model <- lm(quality ~ ., data=train_data)

# Evaluate the model on the testing set
predictions <- predict(model, newdata=test_data)
mse <- mean((test_data$quality - predictions)^2)
print(paste("Mean squared error:", mse))


################################################################


# White wine

# White WINE
corr_matrix2 <- cor(winequality.white)
corrplot(corr_matrix2, method = "number", type = "full", tl.cex = 0.7)

# Fixed acidity VS Quality
ggplot(winequality.white, aes(x = quality, y = fixed.acidity, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Fixed acidity") +
  scale_fill_discrete(name = "Quality rating")

# Volatile acidity VS Quality
ggplot(winequality.white, aes(x = quality, y = volatile.acidity, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Volatile acidity") +
  scale_fill_discrete(name = "Quality rating")

# Citric Acid VS Quality
ggplot(winequality.white, aes(x = quality, y = citric.acid, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Citric Acid") +
  scale_fill_discrete(name = "Quality rating")

# Residual Sugar VS Quality
ggplot(winequality.white, aes(x = quality, y = residual.sugar, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Residual Sugar") +
  scale_fill_discrete(name = "Quality rating")

# Chlorides VS Quality
ggplot(winequality.white, aes(x = quality, y = chlorides, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Chlorides") +
  scale_fill_discrete(name = "Quality rating")

# Free Sulfur Dioxide VS Quality
ggplot(winequality.white, aes(x = quality, y = free.sulfur.dioxide, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Free Sulfur Dioxide") +
  scale_fill_discrete(name = "Quality rating")

# Total Sulfur Dioxide VS Quality
ggplot(winequality.white, aes(x = quality, y = total.sulfur.dioxide, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Total Sulfur Dioxide") +
  scale_fill_discrete(name = "Quality rating")

# Density VS Quality
ggplot(winequality.white, aes(x = quality, y = density, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Density") +
  scale_fill_discrete(name = "Quality rating")

# pH VS Quality
ggplot(winequality.white, aes(x = quality, y = pH, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "pH") +
  scale_fill_discrete(name = "Quality rating")

# Sulphates VS Quality
ggplot(winequality.white, aes(x = quality, y = sulphates, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Sulphates") +
  scale_fill_discrete(name = "Quality rating")

# Alcohol VS Quality
ggplot(winequality.white, aes(x = quality, y = alcohol, fill = factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality rating", y = "Alcohol") +
  scale_fill_discrete(name = "Quality rating")


# Distribution of quality to check whether there is a bias or not
ggplot(winequality.white, aes(x = quality)) +
  geom_histogram(binwidth = 1, color = "black", fill = "yellow") +
  ggtitle("Distribution of White Wine Quality")


# Melt correlation matrix into long format
cor_matrix_melted2 <- melt(corr_matrix2)

# Create heatmap of correlation matrix
ggplot(cor_matrix_melted2, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Correlation Matrix Heatmap Between Variables")

# Split the data into training and testing sets
set.seed(37)
train_indices2 <- sample(nrow(winequality.white), 0.7*nrow(winequality.white))
train_data2 <- winequality.white[train_indices2,]
test_data2 <- winequality.white[-train_indices2,]

# Fit a linear regression model to predict white wine quality based on chemical properties
model2 <- lm(quality ~ ., data=train_data2)
summary(model2)

# Evaluate the model on the testing set
predictions2 <- predict(model2, newdata=test_data2)
mse2 <- mean((test_data2$quality - predictions2)^2)
print(paste("Mean squared error:", mse2))

plot(model, which = 1)
plot(model2, which = 1)

ggplot(model, aes(x = .fitted, y = .resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted", y = "Residuals") +
  ggtitle("Red Wine")

ggplot(model2, aes(x = .fitted, y = .resid)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Fitted", y = "Residuals") + 
  ggtitle("White wine")



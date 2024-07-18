# Load the dataset from URL
df <- read.csv("https://raw.githubusercontent.com/JaySquare87/DTA395-1Rollins/main/CodeProjects/RPG.csv", header = TRUE, sep = ",")

library(leaps)
library(tidyverse)
library(dplyr) 
# Code Project Starts Here.

# Multiple Linear Regression 

# Create a matrix of the Armor, Weapon Physical and Magic columns

# Used chat GPT to help us be able to use the select function as it was resulting in an error
# so it told me to use the dplyr
X <- df |> dplyr::select(Armor, Weapon, Physical, Magic)

# Add a column of 1s to the matrix
X <- cbind(1, X)

# convert X into a matrix
X <- as.matrix(X)

# Calculate beta vector
# Using this method, you can calculate the beta vector for any number of variables
beta <- solve(t(X) %*% X) %*% t(X) %*% df$Level

# Calculate y_hat
y_hat <- X %*% beta

# Add y_hat to the dataframe
df <- df |> mutate(Level_Predictor = y_hat)

# Multi linear regression using lm() function
multi_fit <- lm(Level ~ Armor + Weapon + Physical, Magic, data = df)

# summary of multi_fit
multi_lm_summary <- summary(multi_fit)

# Calculate the accuracy of the model
multi_lm_accuracy <- 1 - multi_lm_summary$sigma / mean(df$Level)

# Split the dataset into training and testing subsets
set.seed(123)  
train_indices <- sample(1:nrow(df), 0.7 * nrow(df))
train_data <- df[train_indices, ]
test_data <- df[-train_indices, ]

# Fit the model on the training subset
X_train <- train_data |> dplyr::select(Armor, Weapon, Physical, Magic)
X_train <- cbind(1, X_train)
X_train <- as.matrix(X_train)
y_train <- train_data$Level
beta_train <- solve(t(X_train) %*% X_train) %*% t(X_train) %*% y_train

# Calculate y_hat for the training subset
y_hat_train <- X_train %*% beta_train

# Create a linear regression model
multi_fit_train <- lm(Level ~ Armor + Weapon + Physical, Magic, data = train_data)
multi_lm_train_summary <- summary(multi_fit_train)

# Predict the test set
test_data$pred <- predict(multi_fit_train, newdata = test_data)

# Calculate the accuracy of the model
1 - sqrt(mean((test_data$Level - test_data$Level_Predictor)^2)) / mean(test_data$Level)

# Use the leaps library and regsubsets() function to calculate Cp, BIC, and adjusted R^2
reg_model <- regsubsets(Level ~ Armor + Weapon + Physical + Magic + FBoss + Class, data = df, method = "exhaustive")

# Summary of the model
summary_reg_model <- summary(reg_model)

# Extract Cp, BIC, and adjusted R^2
cp_values <- summary_reg_model$cp
which.min(summary_reg_model$cp)

bic_values <- summary_reg_model$bic
which.min(summary_reg_model$bic)

adj_r2_values <- summary_reg_model$adjr2
which.max(summary_reg_model$adjr2)

## We will choose to use 4 or 5 criterions to predict the level of a character 
## as that is what the BIC and Cp subset selection methods gave us 
## (which are the best options rather than adjusted R^2)



#--------------------------------------------------------------------------------------------------------



# Multiple Logistic Regression
multiple_logistic_regression <- function(beta, X, y) {
  # Calculate probabilities using logistic function
  z <- X %*% beta  # Matrix multiplication
  # z is the dot product of X and beta
  # beta_0 * x_0 + beta_1 * x_1 + ... + beta_p * x_p
  
  p <- exp(z) / (1 + exp(z))
  # This is the logistic function
  
  # Likelihood function. Same as above.
  likelihood <- -sum(log(y * p + (1 - y) * (1 - p)))
  
  return(likelihood)
}

# Add a column to put False True Values to 0s and 1s
df <- df |> mutate(FBossGLM = ifelse(FBoss == "True", 1, 0))

df$intercept <- 1  # Add an intercept column
# Same as in linear regression, you need to add an intercept column of 1s
X <- as.matrix(df[, c('intercept', 'Weapon', 'Magic')])  # Independent variables
y <- df$FBossGLM  # Dependent variable

# Initial values for beta_0 and beta_1
initial_betas <- rep(0, ncol(X))
# the rep() function is used to repeat the value 0 ncol(X) times

# Optimize to find the best beta_0 and beta_1
optimized_betas <- optim(initial_betas, multiple_logistic_regression, X = X, y = y, control = list(maxit = 1000))
# Notice the control parameter. It is used to set the maximum number of iterations to 1000
# The default is 500. We set it to 1000 to make sure the optimization converges
# If the optimization does not converge, it will return an error

optimized_betas

logfit <- glm(FBossGLM ~ intercept + Weapon + Magic, data = df, family = "binomial")
summary.logfit <- summary(logfit)

# Calculate the accuracy of the model
boss_pred <- predict(logfit, type = "response")
y_hat <- ifelse(boss_pred > 0.5, 1, 0)
mean(y_hat == df$FBossGLM)

library(boot)
# K-Fold Cross Validation
cv.glmktest <- cv.glm(df, logfit, K=10)
cv.glmktest$delta

#--------------------------------------------------------------------------------------------------------


# Create a QDA model to predict species from body mass and flipper length

# First calculate prior probabilities for all species
magic_prior <- sum(df$Class == "Magician") / nrow(df)
warrior_prior <- sum(df$Class == "Warrior") / nrow(df)
tank_prior <- sum(df$Class == "Tank") / nrow(df)
sorc_prior <- sum(df$Class == "Sorcerer") / nrow(df)
battle_prior <- sum(df$Class == "Battlemage") / nrow(df)
knight_prior <- sum(df$Class == "Knight") / nrow(df)

library(MASS)

qda <- qda(Class ~ Magic + Weapon, data = df,
           prior = c(magic_prior, warrior_prior, tank_prior, sorc_prior, battle_prior, knight_prior))

# Create a grid of points
magic_min <- min(df$Magic)
magic_max <- max(df$Magic)
weapon_min <- min(df$Weapon)
weapon_max <- max(df$Weapon)

grid <- expand.grid(Magic = seq(magic_min, magic_max, length.out = 100),
                    Weapon = seq(weapon_min, weapon_max, length.out = 100))

# Predict the species for each point in the grid
grid$Class_pred <- predict(qda, newdata = grid)$class

# Plot the grid with the predicted species
ggplot(df) +
  geom_point(aes(x = Magic, y = Weapon, color = Class)) +
  geom_tile(data = grid, aes(x = Magic, y = Weapon, color = Class_pred), alpha = 0.1)

predictions <- predict(qda, df)$class
mean(predictions == df$Class)

# Create new QDA Model 
qda2 <- qda(Class ~ Magic + Weapon + Physical + Armor, data = df,
           prior = c(magic_prior, warrior_prior, tank_prior, sorc_prior, battle_prior, knight_prior))

predictions2 <- predict(qda2, df,)$class

# Get accuracy of model
mean(predictions2 == df$Class)



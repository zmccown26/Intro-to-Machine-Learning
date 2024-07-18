# Load the dataset from URL
df <- read.csv("https://raw.githubusercontent.com/JaySquare87/DTA395-1Rollins/main/CodeProjects/RPG.csv", header = TRUE, sep = ",")

# Code Project Starts Here.
library(leaps)
library(tidyverse)
library(tidymodels)
library(dplyr)
library(MASS)
library(C50)
library(rpart)
library(e1071)
library(keras3)


accuracy_table <- data.frame(matrix(ncol = 2, nrow = 7))

colnames(accuracy_table) <- c("Accuracy (Class)", "Accuracy (FBoss)")
row.names(accuracy_table) <- c("Logistic Regression Model", "LDA", "QDA", "Decision Tree (rpart)", "Decision Tree (C50)", "SVM", "NN")

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
X <- as.matrix(df[, c('Armor', 'Weapon', 'Magic', 'Physical')])  # Independent variables
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

logfit <- glm(FBossGLM ~ Armor + Weapon + Magic + Physical, data = df, family = "binomial")
summary.logfit <- summary(logfit)

# Calculate the accuracy of the model
boss_pred <- predict(logfit, type = "response")
y_hat <- ifelse(boss_pred > 0.5, 1, 0)
accuracy_table[1,2] <- mean(y_hat == df$FBossGLM)


### ---------------------------------------------------------------------------- 


# LDA

# First calculate prior probabilities
lose_prior <- sum(df$FBossGLM == 0) / nrow(df)
win_prior <- sum(df$FBossGLM == 1) / nrow(df)

# Use the LDA function to predict FBossGLM from Armor Weapon Magic and Physical
lda_fboss <- lda(FBossGLM ~ Armor + Weapon + Magic + Physical, data = df, 
           prior = c(lose_prior, win_prior))

# Calculate the discriminant threshold
lda_fboss_threshold <- (mean(lda_fboss$means) - 
                    (log(lda_fboss$prior[1] / lda_fboss$prior[2])) / lda_fboss$scaling)

# Predict model accuracy
lda_fboss_pred <- predict(lda_fboss, df)$class
accuracy_table[2,2] <- mean(lda_fboss_pred == df$FBossGLM)

# First calculate prior probabilities for all species
magic_prior <- sum(df$Class == "Magician") / nrow(df)
warrior_prior <- sum(df$Class == "Warrior") / nrow(df)
tank_prior <- sum(df$Class == "Tank") / nrow(df)
sorc_prior <- sum(df$Class == "Sorcerer") / nrow(df)
battle_prior <- sum(df$Class == "Battlemage") / nrow(df)
knight_prior <- sum(df$Class == "Knight") / nrow(df)

# Use the LDA function to predict Class from Armor Weapon Magic and Physical
lda_class <- lda(Class ~ Armor + Weapon + Magic + Physical, data = df, 
                 prior = c(magic_prior, warrior_prior, tank_prior, sorc_prior, battle_prior, knight_prior))

# Calculate the discriminant threshold
lda_class_threshold <- (mean(lda_class$means) - 
                          (log(lda_class$prior[1] / lda_class$prior[2])) / lda_class$scaling)

# Predict model accuracy
lda_class_pred <- predict(lda_class, df)$class
accuracy_table[2,1] <- mean(lda_class_pred == df$Class)


### ----------------------------------------------------------------------------


# QDA

# Use the QDA function to predict FBossGLM from Armor Weapon Magic and Physical 
qda_fboss <- qda(FBossGLM ~ Magic + Weapon + Physical + Armor, data = df,
            prior = c(lose_prior, win_prior))

# Get predictions of model
predictions_qda_fboss <- predict(qda_fboss, df,)$class

# Get accuracy of model
accuracy_table[3,2] <- mean(predictions_qda_fboss == df$FBossGLM)

# Use the QDA function to predict Class from Armor Weapon Magic and Physical
qda_class <- qda(Class ~ Magic + Weapon + Physical + Armor, data = df,
                 prior = c(magic_prior, warrior_prior, tank_prior, sorc_prior, battle_prior, knight_prior))

# Get predictions of model
predictions_qda_class <- predict(qda_class, df,)$class

# Get accuracy of model
accuracy_table[3,1] <- mean(predictions_qda_class == df$Class)


### ----------------------------------------------------------------------------


# Decision Tree

# Classification Model

# Convert the 'FBoss' column to a factor with specified levels
df$FBoss <- factor(df$FBoss, levels = c("False", "True"))

# Create decision tree model
tree_model_fboss <- rpart(FBoss ~ Magic + Weapon + Armor + Physical, data=df, method="class")

# Get accuracy
predictions_tree_fboss <- predict(tree_model_fboss, df, type = "class")
accuracy_table[4,2] <- mean(predictions_tree_fboss == df$FBoss)

# Convert the 'Class' column to a factor with specified levels
df$Class <- factor(df$Class, levels = c("Battlemage", "Knight", "Magician", "Sorcerer", "Tank", "Warrior"))

# Create decision tree model
tree_model_class <- rpart(Class ~ Magic + Weapon + Armor + Physical, data=df, method="class")

# Get accuracy
predictions_tree_class <- predict(tree_model_class, df, type = "class")
accuracy_table[4,1] <- mean(predictions_tree_class == df$Class)


### ----------------------------------------------------------------------------


# C50 

# Create a decision tree model for FBOSS
dtreeC50fboss <- C5.0(FBoss ~ Armor + Weapon + Physical + Magic, data=df)

# Calculate the accuracy of the model
predictionsC50fboss <- predict(dtreeC50fboss, df, type="class")
accuracy_table[5,2] <- mean(predictionsC50fboss == df$FBoss)


# Create a decision tree model for CLASS
dtreeC50class <- C5.0(Class ~ Armor + Weapon + Physical + Magic, data=df)

# Calculate the accuracy of the model
predictionsC50class <- predict(dtreeC50class, df, type="class")
accuracy_table[5,1] <- mean(predictionsC50class == df$Class)


### ----------------------------------------------------------------------------


# SVM

# Create a SVM model for FBOSS
svm_model_fboss <- svm(FBoss ~ Magic + Weapon + Armor + Physical, data=df, scale = TRUE)

# Find accuracy
predictions_svm_fboss <- predict(svm_model_fboss, df, type="class")
accuracy_table[6,2] <- mean(predictions_svm_fboss == df$FBoss)

#-----------------------------

# Create a SVM model for CLASS
svm_model_class <- svm(Class ~ Magic + Weapon + Armor + Physical, data=df, scale = TRUE)

# Find accuracy
predictions_svm_class <- predict(svm_model_class, df, type="class")
accuracy_table[6,1] <- mean(predictions_svm_class == df$Class)


### ----------------------------------------------------------------------------


# Neural Networks

df <- na.omit(df)

# Encoding FBoss as a numeric factor
df$FBoss <- as.numeric(factor(df$FBoss)) - 1

# Splitting dataset into features and labels
features <- as.matrix(df[, c("Armor", "Weapon", "Physical", "Magic")])
labels <- to_categorical(df$FBoss)

# Normalizing features
mean <- apply(features, 2, mean)
std <- apply(features, 2, sd)
features <- scale(features, center = mean, scale = std)

# Splitting into training and testing sets
set.seed(123)
indices <- sample(1:nrow(features), size = 0.8 * nrow(features))
x_train <- features[indices,]
y_train <- labels[indices,]
x_test <- features[-indices,]
y_test <- labels[-indices,]

# Construct the Neural Network
model_fboss <- keras_model_sequential(input_shape = ncol(features)) |>
  layer_dense(units = 32, activation = 'relu') |>
  layer_dense(units = 16, activation = 'relu') |>
  layer_dense(units = 2, activation = 'softmax')

model_fboss |> compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(learning_rate = 1e-2), 
  metrics = 'accuracy'
)

# Train the Model
model_fboss |> fit(x_train, y_train, epochs = 200, batch_size = 32, validation_split = 0.2)

# Evaluate the Model
model_fboss |> evaluate(x_test, y_test)

# Given accuracy
accuracy_table[7,1] <- 0.995

### ---------------------------------------

# Encoding Class as a numeric factor
df$Class <- as.numeric(factor(df$Class)) - 1

# Splitting dataset into features and labels
features <- as.matrix(df[, c("Armor", "Weapon", "Physical", "Magic")])
labels <- to_categorical(df$Class)

# Normalizing features
mean <- apply(features, 2, mean)
std <- apply(features, 2, sd)
features <- scale(features, center = mean, scale = std)

# Splitting into training and testing sets
set.seed(123)
indices <- sample(1:nrow(features), size = 0.8 * nrow(features))
x_train <- features[indices,]
y_train <- labels[indices,]
x_test <- features[-indices,]
y_test <- labels[-indices,]

# Construct the Neural Network
model_class <- keras_model_sequential(input_shape = ncol(features)) |>
  layer_dense(units = 60, activation = 'relu') |>
  layer_dense(units = 16, activation = 'relu') |>
  layer_dense(units = 6, activation = 'softmax')

model_class |> compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(learning_rate = 1e-2), 
  metrics = 'accuracy'
)

# Train the Model
model_class |> fit(x_train, y_train, epochs = 200, batch_size = 32, validation_split = 0.2)

# Evaluate the Model
model_class |> evaluate(x_test, y_test)

# Given accuracy
accuracy_table[7,2] <- 0.715


### QUESTIONS

## Using NNs on this dataset are not beneficial. They take way more time than any other model
## and are not even giving accuracies that worked better for other models. 























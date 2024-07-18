
# Load tidyverse
library(tidyverse)

# Load the ISLR2 library
library(ISLR2)

# Load the Auto dataset
data(Auto)

# Set the seed to 1
set.seed(1)

# This is a code you will use for the linear regression validation analysis of this lab
# Create a function with a loop to calculate the accuracy of the model
# for different training percentages as input
accuracy_function <- function(training_percentage) {
  N <- nrow(Auto)
  accuracies <- c()
  for (i in 1:1000) {
    train <- sample(N, floor(training_percentage*N))
    lm.fit <- lm(mpg ~ horsepower, data = Auto, subset = train)
    if(training_percentage == 1) {
      MSE <- mean((Auto$mpg - predict(lm.fit, Auto))^2)
    } else {
      MSE <- mean((Auto$mpg - predict(lm.fit, Auto))[-train]^2)
    }
    RSE <- sqrt((MSE * N) / (N - 2))
    accuracy <- 1 - RSE / mean(Auto$mpg)
    accuracies <- c(accuracies, accuracy)
  }
  return(accuracies)
}


test1 <- accuracy_function(0.3)
test2 <- accuracy_function(0.5)
test3 <- accuracy_function(0.9)


tests <- c(rep('test1', 1000), rep('test2', 1000), rep('test3', 1000))

df <- data.frame(tests = tests, accuracy = c(test1, test2, test3))


# Plot density
ggplot(df, aes(x = accuracy, fill = tests)) +
  geom_density(alpha = 0.5) +
  labs(x = "Accuracy", y = "Density")

# Plot histogram
ggplot(df, aes(x = accuracy, fill = tests)) +
  geom_histogram(alpha = 0.5) +
  labs(x = "Accuracy", y = "Density")

# Check variances
var(test1)
var(test2)
var(test3)



# Load the iris dataset
data(iris)

iris_sub <- iris[iris$Species != "setosa", ]

iris_sub <- iris_sub |> mutate(Species = ifelse(Species == "virginica", 1, 0))

# This is a code you will use for the logistic regression validation analysis of this lab
# create a function that takes in the training percentage and returns the accuracy
get_accuracy <- function(training_percentage) {
  N <- nrow(iris_sub)
  accuracies <- c()
  for (i in 1:1000) {
    train <- sample(N, floor(training_percentage*N))
    glm.fit <- glm(Species ~ Sepal.Length, data = iris_sub, family = binomial, subset = train)
    if(training_percentage == 1) {
      accuracies <- c(accuracies, 
                      mean((iris_sub$Species == ifelse(predict(glm.fit, iris_sub, type = "response") > 0.5, 1, 0))))
    } else {
      accuracies <- c(accuracies, 
                      mean((iris_sub$Species == ifelse(predict(glm.fit, iris_sub, type = "response") > 0.5, 1, 0))[-train]))
    }
  }
  return(accuracies)
}


test1log <- get_accuracy(0.3)
test2log <- get_accuracy(0.5)
test3log <- get_accuracy(0.9)


testslog <- c(rep('test1log', 1000), rep('test2log', 1000), rep('test3log', 1000))

dflog <- data.frame(testslog = testslog, accuracy = c(test1log, test2log, test3log))

# Plot density
ggplot(dflog, aes(x = accuracy, fill = tests)) +
  geom_density(alpha = 0.5) +
  labs(x = "Accuracy", y = "Density")


library(boot)

# Leave One Out Cross Validation
glm.fit <- glm(mpg ~ poly(horsepower, 5), data = Auto, family = gaussian)
cv.glmtest <- cv.glm(Auto, glm.fit)
cv.glmtest$delta

# K-Fold Cross Validation
cv.glmktest <- cv.glm(Auto, glm.fit, K=10)
cv.glmktest$delta

# Bootstrap
boot.fn <- function(data, index) {
  return(coef(lm(mpg ~ horsepower, data = data, subset = index)))
}

boot.fn(Auto, 1:392)




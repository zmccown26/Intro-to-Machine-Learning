# Load the tidyverse library
library(tidyverse)

# Load the Default dataset
# the read.csv function reads a csv file and returns a data frame
df <- read.csv("https://raw.githubusercontent.com/JaySquare87/DTA395-1Rollins/main/Week%203/Default.csv", header=TRUE, sep=",")

# Create some plot to explore the data

# Plot balance vs income
ggplot(df, aes(x = balance, y = income)) +
  geom_point(aes(shape=student, color=default)) +
  labs(x = "Balance", y = "Income")

# Too crowded. Let's sample 2000 entries from df

# sample 2000 entries from df
sample_df <- df[sample(nrow(df), 2000), ]

# Plot balance vs income
ggplot(sample_df, aes(x = balance, y = income)) +
  geom_point(aes(shape=student, color=default)) +
  labs(x = "Balance", y = "Income")

# We will plot boxplots to compare the distribution of balance, income, and default by student
# In a boxplot, it doesn't matter the quantity of data points, we can see the distribution of the data
# Therefore, we will use the original df

# plot balance vs default
ggplot(df, aes(x = default, y = balance)) +
  geom_boxplot(aes(shape=student, color=student)) +
  labs(x = "Balance", y = "Default")

# plot default vs income by student
ggplot(df, aes(x = default, y = income)) +
  geom_boxplot(aes(shape=student, color=student)) +
  labs(x = "Default", y = "Income")

# As we can see from the plots, the distribution of balance differ between default and no default
# Whether it is default or not, the distribution of income is similar
# Therefore, it is obvious that balance is a good predictor of default

# Analysis

# Convert default to numeric
df <- df |> mutate(default = ifelse(default == "Yes", 1, 0))

# Convert student to numeric
df <- df |> mutate(student = ifelse(student == "Yes", 1, 0))

# When making classification models, always convert your variables to numerics
# default and student values are either "Yes", or "No", so we convert them to 1s and 0s

# Logistic Regression
# We can use the glm() function to fit a logistic regression model
# We will do that later.
# However, we will first do this manually.
# You need to learn how to do this because in the Coding Project, the glm()
# function will fail to fit the model.
# BTW when we say "fit the model", we mean "find the best beta_0 and beta_1, beta_2, ... beta_p"

# Logistic Regression Function
logistic_regression <- function(beta, x, y) {
  # x is your input variable. In this case, it is balance
  # y is your output variable. In this case, it is default
  # beta is a vector of coefficients. In this case, it is beta_0 and beta_1
  
  # Calculate probabilities using logistic function
  #p <- 1 / (1 + exp(-(beta[1] + beta[2] * x)))
  # the commented line is the same as the line below. It is just simplified
  p <- exp(beta[1] + beta[2] * x) / (1 + exp(beta[1] + beta[2] * x))
  # This is the equation we use to find the probability of default based on balance
  
  # Likelihood function
  likelihood <- -sum(log(y * p + (1 - y) * (1 - p)))
  # We use this function to find the best beta_0 and beta_1
  # This is the best function for optimization

  print(beta) # uncomment this line to see the beta values change over time

  return(likelihood)

}

# Initial values for beta_0 and beta_1
initial_betas <- c(0, 0)

# Optimize to find the best beta_0 and beta_1
optimized_betas <- optim(initial_betas, logistic_regression, x = df$balance, y = df$default)

# So what exactly is happening here?
# The optim function is trying to find the best beta_0 and beta_1
# The logistic_regression function is the function that the optim will use to find the best beta_0 and beta_1
# The optim function will take the initial_betas that we provided
# The optim function will then run the logistic_regression function and get new betas
# If the betas values changed, the optim function will run the logistic_regression function again with the new betas values
# The optim function will keep doing this until the betas values stop changing
# The optim function will then return the best betas values
# Uncheck the print(betas) line to see the betas values change over time

optimized_betas
# to access the values, you have to access the sublist par
optimized_betas$par

# Linear Regression using glm() function
# It is just one line of code. It is that simple

lr.fit <- glm(default ~ balance, data = df, family = "binomial")
# default is the dependent variable
# ~ is the tilde symbol. It separates the dependent variable from the independent variable
# balance is the independent variable
# data = df is the data frame
# family = "binomial" is the family of the model. It is binomial because we are doing logistic regression


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


# Prepare the data for logistic regression
# We will present the data in a matrix form for faster computation
df$intercept <- 1  # Add an intercept column
# Same as in linear regression, you need to add an intercept column of 1s
X <- as.matrix(df[, c('intercept', 'balance', 'income', 'student')])  # Independent variables matrix
y <- df$default  # Dependent variable

# Initial values for beta_0 and beta_1
initial_betas <- rep(0, ncol(X))
# the rep() function is used to repeat the value 0 ncol(X) times

# Optimize to find the best beta_0 and beta_1
optimized_betas <- optim(initial_betas, multiple_logistic_regression, X = X, y = y, control = list(maxit = 1000))
# Notice the control parameter. It is used to set the maximum number of iterations to 1000
# The default is 500. We set it to 1000 to make sure the optimization converges
# If the optimization does not converge, it will return an error

optimized_betas

# Fit the model using glm
glm.fit <- glm(default ~ balance + income + student, data = df, family = binomial)

# Check the accuracy of the model
summary(glm.fit)

# Check the accuracy of the model manually
# Calculate the probabilities
z <- X %*% optimized_betas$par
p <- exp(z) / (1 + exp(z))

# Calculate the predicted classes
predicted_classes <- ifelse(p > 0.5, 1, 0)

# Calculate the accuracy
accuracy <- mean(predicted_classes == y)

accuracy
# Yes. The accuracy is 97%. That is pretty good for a such an old model
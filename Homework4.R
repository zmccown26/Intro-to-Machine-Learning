# Load the leaps library
library(leaps)

# Set seed to 123
set.seed(123)

# Specific number of rows
n <- 1000

# Specify the number of predictors
num_predictors <- 10 # Change this number as needed

# Initialize an empty dataframe for predictors
df <- data.frame(matrix(ncol = num_predictors, nrow = n))
colnames(df) <- paste0("X", 1:num_predictors)

# Fill dataframe with random values for each predictor
for(i in 1:num_predictors) {
  df[,i] <- rnorm(n, mean = 0, sd = 1)
}

# Create a vector called noise with random numbers from 1 to 10
noise <- runif(1000, 1, 10) # You can adjust the range as needed

# How many variables will you choose for the best subset
k <- 5
right_betas <- runif(k, 1, 5) # You can adjust the range as needed

# Define the best predictors
best_predictors <- sample(num_predictors, k, replace=F)
X <- as.matrix(df[, best_predictors])
z <- X %*% right_betas

# Generate response variable Y based on a linear combination of some predictors + noise
Y <- z + noise

# Your code starts here. You can also adjust the parameters above.
# The parameters are:
# - n: number of rows
# - num_predictors: number of predictors
# - noise: vector of random numbers
# - k: number of variables for the best subset
# - right_betas: vector of random numbers
# - best_predictors: vector of random numbers

# Create table to store data
table1 <- data.frame(matrix(ncol = 3, nrow = 3))
colnames(table1) <- c("Adjusted (R^2)", "Cp", "BIC")
row.names(table1) <- c("Best Subset Selection", "Stepwise Forward Selection", "Stepwise Backward Selection")

# Add Y values to df dataset
df <- cbind(Y, df)

# Perform best subset selection on Y
regfit.full <- regsubsets(Y ~ ., df, nvmax = 10)
reg.summary <- summary(regfit.full)

# Find best adjusted R squared (max)
best.subset.adjr2 <- which.max(reg.summary$adjr2)
table1[1,1] <- reg.summary$adjr2[best.subset.adjr2]

# Find the best Cp (min)
best.subset.cp <- which.min(reg.summary$cp)
table1[1,2] <- reg.summary$cp[best.subset.cp]

# Find the best BIC (min)
best.subset.bic <- which.min(reg.summary$bic)
table1[1,3] <- reg.summary$bic[best.subset.bic]



# Forward Stepwise
regfit.fwd <- regsubsets(Y ~ ., data = df, nvmax=10, method="forward")
reg.fwd.summary <- summary(regfit.fwd)

# Find best adjusted R squared (max)
forward.adjr2 <- which.max(reg.fwd.summary$adjr2)
table1[2,1] <- reg.fwd.summary$adjr2[forward.adjr2]

# Find the best Cp (min)
forward.cp <- which.min(reg.fwd.summary$cp)
table1[2,2] <- reg.fwd.summary$cp[forward.cp]

# Find the best BIC (min)
forward.bic <- which.min(reg.fwd.summary$bic)
table1[2,3] <- reg.fwd.summary$bic[forward.bic]



# Backward Stepwise
# Forward Stepwise
regfit.bwd <- regsubsets(Y ~ ., data = df, nvmax=10, method="backward")
reg.bwd.summary <- summary(regfit.bwd)

# Find best adjusted R squared (max)
backward.adjr2 <- which.max(reg.bwd.summary$adjr2)
table1[3,1] <- reg.bwd.summary$adjr2[backward.adjr2]

# Find the best Cp (min)
backward.cp <- which.min(reg.bwd.summary$cp)
table1[3,2] <- reg.bwd.summary$cp[backward.cp]

# Find the best BIC (min)
backward.bic <- which.min(reg.bwd.summary$bic)
table1[3,3] <- reg.bwd.summary$bic[backward.bic]

#-------------------------------------------------------------------------------

# Create df with num_predictors 8 and k 6, and betas ranging from -3 to 3

# Set seed to 50
set.seed(50)

# Specific number of rows
n <- 1000

# Specify the number of predictors
num_predictors <- 8 # Change this number as needed

# Initialize an empty dataframe for predictors
df <- data.frame(matrix(ncol = num_predictors, nrow = n))
colnames(df) <- paste0("X", 1:num_predictors)

# Fill dataframe with random values for each predictor
for(i in 1:num_predictors) {
  df[,i] <- rnorm(n, mean = 0, sd = 1)
}

# Create a vector called noise with random numbers from 1 to 10
noise <- runif(1000, 1, 10) # You can adjust the range as needed

# How many variables will you choose for the best subset
k <- 6
right_betas <- runif(k, -3, 3) # You can adjust the range as needed

# Define the best predictors
best_predictors <- sample(num_predictors, k, replace=F)
X <- as.matrix(df[, best_predictors])
z <- X %*% right_betas

# Generate response variable Y based on a linear combination of some predictors + noise
Y <- z + noise


# Create table to store data
table2 <- data.frame(matrix(ncol = 3, nrow = 3))
colnames(table2) <- c("Adjusted (R^2)", "Cp", "BIC")
row.names(table2) <- c("Best Subset Selection", "Stepwise Forward Selection", "Stepwise Backward Selection")

# Add Y values to df dataset
df <- cbind(Y, df)

# Perform best subset selection on Y
regfit.full <- regsubsets(Y ~ ., df, nvmax = 8)
reg.summary <- summary(regfit.full)

# Find best adjusted R squared (max)
best.subset.adjr2 <- which.max(reg.summary$adjr2)
table2[1,1] <- reg.summary$adjr2[best.subset.adjr2]

# Find the best Cp (min)
best.subset.cp <- which.min(reg.summary$cp)
table2[1,2] <- reg.summary$cp[best.subset.cp]

# Find the best BIC (min)
best.subset.bic <- which.min(reg.summary$bic)
table2[1,3] <- reg.summary$bic[best.subset.bic]



# Forward Stepwise
regfit.fwd <- regsubsets(Y ~ ., data = df, nvmax=8, method="forward")
reg.fwd.summary <- summary(regfit.fwd)

# Find best adjusted R squared (max)
forward.adjr2 <- which.max(reg.fwd.summary$adjr2)
table2[2,1] <- reg.fwd.summary$adjr2[forward.adjr2]

# Find the best Cp (min)
forward.cp <- which.min(reg.fwd.summary$cp)
table2[2,2] <- reg.fwd.summary$cp[forward.cp]

# Find the best BIC (min)
forward.bic <- which.min(reg.fwd.summary$bic)
table2[2,3] <- reg.fwd.summary$bic[forward.bic]



# Backward Stepwise
# Forward Stepwise
regfit.bwd <- regsubsets(Y ~ ., data = df, nvmax=8, method="backward")
reg.bwd.summary <- summary(regfit.bwd)

# Find best adjusted R squared (max)
backward.adjr2 <- which.max(reg.bwd.summary$adjr2)
table2[3,1] <- reg.bwd.summary$adjr2[backward.adjr2]

# Find the best Cp (min)
backward.cp <- which.min(reg.bwd.summary$cp)
table2[3,2] <- reg.bwd.summary$cp[backward.cp]

# Find the best BIC (min)
backward.bic <- which.min(reg.bwd.summary$bic)
table2[3,3] <- reg.bwd.summary$bic[backward.bic]


#-------------------------------------------------------------------------------

# Create df with less noise on same model as 1

# Set seed to 100
set.seed(123)

# Specific number of rows
n <- 1000

# Specify the number of predictors
num_predictors <- 10 # Change this number as needed

# Initialize an empty dataframe for predictors
df <- data.frame(matrix(ncol = num_predictors, nrow = n))
colnames(df) <- paste0("X", 1:num_predictors)

# Fill dataframe with random values for each predictor
for(i in 1:num_predictors) {
  df[,i] <- rnorm(n, mean = 0, sd = 1)
}

# Create a vector called noise with random numbers from 1 to 10
noise <- runif(1000, 1, 2) # You can adjust the range as needed

# How many variables will you choose for the best subset
k <- 5
right_betas <- runif(k, 1, 5) # You can adjust the range as needed

# Define the best predictors
best_predictors <- sample(num_predictors, k, replace=F)
X <- as.matrix(df[, best_predictors])
z <- X %*% right_betas

# Generate response variable Y based on a linear combination of some predictors + noise
Y <- z + noise


# Create table to store data
table3 <- data.frame(matrix(ncol = 3, nrow = 3))
colnames(table3) <- c("Adjusted (R^2)", "Cp", "BIC")
row.names(table3) <- c("Best Subset Selection", "Stepwise Forward Selection", "Stepwise Backward Selection")

# Add Y values to df dataset
df <- cbind(Y, df)

# Perform best subset selection on Y
regfit.full <- regsubsets(Y ~ ., df, nvmax = 10)
reg.summary <- summary(regfit.full)

# Find best adjusted R squared (max)
best.subset.adjr2 <- which.max(reg.summary$adjr2)
table3[1,1] <- reg.summary$adjr2[best.subset.adjr2]

# Find the best Cp (min)
best.subset.cp <- which.min(reg.summary$cp)
table3[1,2] <- reg.summary$cp[best.subset.cp]

# Find the best BIC (min)
best.subset.bic <- which.min(reg.summary$bic)
table3[1,3] <- reg.summary$bic[best.subset.bic]



# Forward Stepwise
regfit.fwd <- regsubsets(Y ~ ., data = df, nvmax=10, method="forward")
reg.fwd.summary <- summary(regfit.fwd)

# Find best adjusted R squared (max)
forward.adjr2 <- which.max(reg.fwd.summary$adjr2)
table3[2,1] <- reg.fwd.summary$adjr2[forward.adjr2]

# Find the best Cp (min)
forward.cp <- which.min(reg.fwd.summary$cp)
table3[2,2] <- reg.fwd.summary$cp[forward.cp]

# Find the best BIC (min)
forward.bic <- which.min(reg.fwd.summary$bic)
table3[2,3] <- reg.fwd.summary$bic[forward.bic]



# Backward Stepwise
# Forward Stepwise
regfit.bwd <- regsubsets(Y ~ ., data = df, nvmax=10, method="backward")
reg.bwd.summary <- summary(regfit.bwd)

# Find best adjusted R squared (max)
backward.adjr2 <- which.max(reg.bwd.summary$adjr2)
table3[3,1] <- reg.bwd.summary$adjr2[backward.adjr2]

# Find the best Cp (min)
backward.cp <- which.min(reg.bwd.summary$cp)
table3[3,2] <- reg.bwd.summary$cp[backward.cp]

# Find the best BIC (min)
backward.bic <- which.min(reg.bwd.summary$bic)
table3[3,3] <- reg.bwd.summary$bic[backward.bic]


### Reflection

# The variations in values across tables underscore the sensitivity of model performance to key parameters 
# (num_predictors, k, noise, etc). Increasing predictors often improves Adjusted R^2, but higher complexity penalizes 
# models (Cp, BIC). Elevated noise levels lead to reduced model accuracy and increased complexity penalties. 
# The specific values of betas and predictor selection influence overall model outcomes. Best Subset Selection 
# tends to yield higher Adjusted R^2 but may overfit, while Stepwise methods prioritize simplicity. The findings 
# emphasize the importance of careful parameter selection in achieving a balance between model fit and 
# complexity in data analysis.





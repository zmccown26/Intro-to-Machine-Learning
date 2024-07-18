# Load the tidyverse library
library(tidyverse)

# Load the iris dataset
df <- iris

# If species is Virginica put 1 else put 0
df <- df |> mutate(virginica = ifelse(Species == "virginica", 1, 0))

# Logistic Regression (using the multiple logistic regression function)
logistic_regression <- function(beta, X, y) {
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

# Add a column of 1s to the matrix
df <- cbind(1, df)

# Create new data frame with variable we want 
X.SepLen <- df |> select(1, Sep.Length = Sepal.Length)

# Convert X.SepLen into a matrix
X.SepLen <- as.matrix(X.SepLen)

# Initialize betas
init_betas.SepLen <- c(0, 0)

# Optim function
optim_betas.SepLen <- optim(init_betas.SepLen, logistic_regression, X = X.SepLen, y = df$virginica)

# Glm function to compare
glm.fit.SepLen <- glm(virginica ~ Sepal.Length, data = df, family = "binomial")

# Summary of glm
glm.fit.SepLen_sum <- summary(glm.fit.SepLen)

# Get beta 0 value
beta_0_z.SepLen = glm.fit.SepLen_sum$coefficients["(Intercept)", "z value"]

# Get beta 1 value
beta_1_z.SepLen = glm.fit.SepLen_sum$coefficients["Sepal.Length", "z value"]

# Check to see if betas are valid
beta_0_z.SepLen.check <- !(beta_0_z.SepLen >= -2 & beta_0_z.SepLen <= 2)
beta_1_z.SepLen.check <- !(beta_1_z.SepLen >= -2 & beta_1_z.SepLen <= 2)

# Create accuracy variables
z.SepLen <- X.SepLen %*% optim_betas.SepLen$par
p.SepLen <- exp(z.SepLen) / (1 + exp(z.SepLen))

# Calculate the predicted classes
predicted_classes <- ifelse(p.SepLen > 0.5, 1, 0)

# Calculate the accuracy
accuracy.SepLen <- mean(predicted_classes == df$virginica)



# ------------------------------------------------------




# Create new data frame with variable we want 
X.SepLenWid <- df |> select(1, Sep.Length = Sepal.Length, Sep.Width = Sepal.Width)

# Convert X.SepLen into a matrix
X.SepLenWid <- as.matrix(X.SepLenWid)

# Initialize betas
init_betas.SepLenWid <- c(0, 0, 0)

# Optim function
optim_betas.SepLenWid <- optim(init_betas.SepLenWid, logistic_regression, X = X.SepLenWid, y = df$virginica)

# Glm function to compare
glm.fit.SepLenWid <- glm(virginica ~ Sepal.Length + Sepal.Width, data = df, family = "binomial")

# Summary of glm
glm.fit.SepLenWid_sum <- summary(glm.fit.SepLenWid)

# Get beta 0 value
beta_0_z.SepLenWid = glm.fit.SepLenWid_sum$coefficients["(Intercept)", "z value"]

# Get beta 1 value
beta_1_z.SepLenWid = glm.fit.SepLenWid_sum$coefficients["Sepal.Length", "z value"]

# Get beta 2 value
beta_2_z.SepLenWid = glm.fit.SepLenWid_sum$coefficients["Sepal.Width", "z value"]

# Check to see if betas are valid
beta_0_z.SepLenWid.check <- !(beta_0_z.SepLenWid >= -2 & beta_0_z.SepLenWid <= 2)
beta_1_z.SepLenWid.check <- !(beta_1_z.SepLenWid >= -2 & beta_1_z.SepLenWid <= 2)
beta_2_z.SepLenWid.check <- !(beta_2_z.SepLenWid >= -2 & beta_2_z.SepLenWid <= 2)

# Create accuracy variables
z.SepLenWid <- X.SepLenWid %*% optim_betas.SepLenWid$par
p.SepLenWid <- exp(z.SepLenWid) / (1 + exp(z.SepLenWid))

# Calculate the predicted classes
predicted_classes_2 <- ifelse(p.SepLenWid > 0.5, 1, 0)

# Calculate the accuracy
accuracy.SepLenWid <- mean(predicted_classes == df$virginica)


##### Question #####

# Yes the accuracy decreased little and this is because the second
# variable that was added (Width) lead to incorrect predictions and we saw 
# this by looking at the z score for beta_2_z.SepLenWid as it fell in 
# between -2 and 2 causing it to not be statistically significant. 







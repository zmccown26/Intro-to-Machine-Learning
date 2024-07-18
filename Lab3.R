# Load the MASS library
library(MASS)

# Load the tidyverse package
library(tidyverse)

# It is better to load tidyverse after MASS so that the Select() function
# from dplyr mask the Select() function from MASS
# In this case, the Select() function from dplyr is used

# Linear Discriminant Analysis with hypothetical data

# Create a normal distribution of 60 values with a mean of 5 and variance of 2
spam_sale <- rnorm(60, mean = 5, sd = sqrt(2))

# Create a normal distribution of 40 values with a mean of 2 and variance of 1
ham_sale <- rnorm(40, mean = 2, sd = sqrt(1))

# Create a data frame with the spam and ham sales
sales <- data.frame(sales = c(spam_sale, ham_sale), 
                    type = c(rep("spam", 60), rep("ham", 40)))
# the rep() function repeats the labels for the email type.
# 60 times for spam and 40 times for ham, based on the number of values in the 
# spam_sale and ham_sale vectors we defined above.

# Plot a histogram of sales
sales |> ggplot(aes(x = sales, fill = type)) +
  geom_histogram(binwidth = 0.5, position = "dodge") +
  labs(title = "Sales Distribution", x = "Sales", y = "Frequency") +
  theme_minimal()

# plot density
sales |> ggplot(aes(x = sales, fill = type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Sales Distribution", x = "Sales", y = "Density") +
  theme_minimal()

# Create a factor of type
sales$type <- as.factor(sales$type)
levels(sales$type) <- c("spam", "ham")
# We use the as.factor() function to convert the type column to a factor.
# We then use the levels() function to change the levels of the factor to "spam" and "ham".
# This is done to ensure that the levels of the factor are in the correct order.
# So that when we create the LDA model and define the priori probabilities, the levels are in the correct order.

# Create lda model for spam and ham sales
lda_model <- lda(type ~ sales, data = sales, prior = c(0.6, 0.4))
# The prior argument is used to define the priori probabilities of the levels of the factor.
# 60% for spam and 40% for ham because if you look at the data frame, there are 60 spam and 40 ham emails.

# Calculate the discriminant threshold
threshold <- (mean(lda_model$means) - 
                (log(lda_model$prior[1]/lda_model$prior[2])) / (lda_model$scaling))

# lda_model$means: This is accessing the means of the groups in the LDA model. The $ operator is used to access elements of a list or data frame.
# mean(lda_model$means): This is calculating the mean of those group means.
# lda_model$prior: This is accessing the prior probabilities of the groups in the LDA model.
# log(lda_model$prior[1]/lda_model$prior[2]): This is calculating the logarithm of the ratio of the first and second prior probabilities.
# lda_model$scaling: This is accessing the scaling coefficients for the linear discriminants.

threshold

# Plot the density plot with threshold
sales |> ggplot(aes(x = sales, fill = type)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = threshold, linetype = "dashed") +
  labs(title = "Sales Distribution", x = "Sales", y = "Density") +
  theme_minimal()

# the geom_vline() function is used to add a vertical line to the plot.

# Real world example

# Iris dataset

# Load the iris dataset
data(iris)

# Plot the speal length of the iris dataset
iris |> ggplot(aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5) +
  labs(title = "Sepal Length Distribution", x = "Sepal Length", y = "Density") +
  theme_minimal()

# Create a subset of versicolor and virginica
iris_virg_vers <- iris |> 
  filter(Species %in% c("versicolor", "virginica"))

# Plot the Speal Length for the subset
iris_virg_vers |> ggplot(aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5) +
  labs(title = "Sepal Length Distribution", x = "Sepal Length", y = "Density") +
  theme_minimal()

# Check whether the Sepal Length is normally distributed for virginica
iris_virg_vers$Sepal.Length[iris_virg_vers$Species == "virginica"] |> 
  shapiro.test()
iris_virg_vers$Sepal.Length[iris_virg_vers$Species == "versicolor"] |> 
  shapiro.test()

# The Shapiro-Wilk test is used to test the normality of the Sepal Length for the virginica and versicolor species.
# The results show that the Sepal Length is normally distributed for both species.
# The p-value is greater than 0.05 for both species.
# This means that we can use the LDA model to classify the species based on the Sepal Length.

# Fix the levels of Species in subset
# Set the levels of Species to virginica and versicolor
iris_virg_vers$Species <- factor(iris_virg_vers$Species, levels = c("virginica", "versicolor"))


# Create a lda model for the subset
lda_model_iris <- lda(Species ~ Sepal.Length, 
                      data = iris_virg_vers)

# Caluclate the discriminant threshold
threshold_iris <- (mean(lda_model_iris$means) - 
                    (log(lda_model_iris$prior[1]/lda_model_iris$prior[2])) / (lda_model_iris$scaling))

# Plot the density plot with threshold
iris_virg_vers |> ggplot(aes(x = Sepal.Length, fill = Species)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = threshold_iris, linetype = "dashed") +
  labs(title = "Sepal Length Distribution", x = "Sepal Length", y = "Density") +
  theme_minimal()

# Add the Sepal Width variable

# Plot Sepal Length and Width for the subset
iris_virg_vers |> 
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(title = "Sepal Length and Width", 
       x = "Sepal Length", 
       y = "Sepal Width") +
  theme_minimal()

# Create a lda model for the subset with Sepal Width
lda_model_iris_2 <- lda(Species ~ Sepal.Length + Sepal.Width, 
                        data = iris_virg_vers)

# Calculate the slope
slope <- -lda_model_iris_2$scaling[1] / lda_model_iris_2$scaling[2]

# Calculate the intercept
intercept <- -mean(lda_model_iris_2$means %*% lda_model_iris_2$scaling)
intercept <- -intercept / lda_model_iris_2$scaling[2]

# Plot the Sepal Length and Width with the discriminant line
iris_virg_vers |> 
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  geom_abline(intercept = intercept, slope = slope) +
  labs(title = "Sepal Length and Width", 
       x = "Sepal Length", 
       y = "Sepal Width") +
  theme_minimal()

# Create a lda model for the entire iris dataset
lda_model_iris_3 <- 
  lda(Species ~ Sepal.Length + Sepal.Width, data = iris)

# Plot scatter of Sepal Length and Width
iris |> 
  ggplot(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(title = "Sepal Length and Width", 
       x = "Sepal Length", 
       y = "Sepal Width") +
  theme_minimal()

# We will create a grid of points to plot the decision boundary of the LDA model.
# This is easier than calculating the decision boundaries manually.

# Set the boundaries for the grid
max_Sepal.Length = max(iris$Sepal.Length) + 0.2
min_Sepal.Length = min(iris$Sepal.Length) - 0.2

max_Sepal.Width = max(iris$Sepal.Width) + 0.2
min_Sepal.Width = min(iris$Sepal.Width) - 0.2

# Create a grid
grid <- expand.grid(Sepal.Length = seq(min_Sepal.Length, max_Sepal.Length, length.out = 200), 
              Sepal.Width = seq(min_Sepal.Width, max_Sepal.Width, length.out = 200))

# Plot grid
plot(grid)

# Predict the species of the points in the grid
grid$Species <- predict(lda_model_iris_3, newdata = grid)$class

# Plot the grid with the species
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  geom_tile(data = grid, aes(x = Sepal.Length, y = Sepal.Width, fill = Species), alpha = 0.1) +
  labs(title = "Sepal Length and Width", 
       x = "Sepal Length", 
       y = "Sepal Width") +
  theme_minimal()

# Do a quadratic discriminant analysis
qda_model_iris <- qda(Species ~ Sepal.Length + Sepal.Width, data = iris)

# Predict the grid using qda
grid$Species <- predict(qda_model_iris, newdata = grid)$class

# plot the grid with the species
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = Species)) +
  geom_tile(data = grid, aes(x = Sepal.Length, y = Sepal.Width, fill = Species), alpha = 0.5) +
  labs(title = "Sepal Length and Width", 
       x = "Sepal Length", 
       y = "Sepal Width") +
  theme_minimal()

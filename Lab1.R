# Load the tidyverse library
library(tidyverse) #The tidy verse is a collection of R packages that share common data representations and API design.
library(patchwork) # The goal of patchwork is to make it ridiculously simple to combine separate ggplots into the same graphic.

# Read dataset from the following URL
# https://github.com/JaySquare87/DTA395-1Rollins/blob/main/Week%202/Advertising.csv
df <- read.csv("https://raw.githubusercontent.com/JaySquare87/DTA395-1Rollins/main/Week%202/Advertising.csv")

# Plot TV against Sales
p1 <- ggplot(df, aes(x = TV, y = sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "TV", y = "Sales")

# Plot Radio against sales
p2 <- ggplot(df, aes(x = radio, y = sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Radio", y = "Sales")

# Plot Newspaper against sales
p3 <- ggplot(df, aes(x = newspaper, y = sales)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Newspaper", y = "Sales")

# Combine plots
# This is where patchwork comes in handy
p1 + p2 + p3

# Create a linear regression between sales and TV
lm(df$sales ~ df$TV)

# Multiple linear regression between sales and TV, Radio and Newspaper
lm(df$sales ~ df$TV + df$radio + df$newspaper)

# In this class we will try to avoid using the lm() function unless you want to
# verify your answers or get the summary of the model which contains the p-values
# t-statisics, RSE, R^2, F-statistic, etc.

# -------------------------------------------------------------

# Simple Linear Regression between sales and TV

# Create a dataframe of the sales and TV columns
TVdf <- df |> select(sales, TV)

# Let us try and create the columns we need to calculate the slope and intercept
# Check slide #10 from the Linear Regression slides.

# Create a column that is the difference between TV and the mean of TV
# In this code, we called it tv_diff. You can call it whatever you want.
TVdf <- TVdf |> mutate(tv_diff = TV - mean(TV))

# Create a columns that is the difference between sales and the mean of sales
TVdf <- TVdf |> mutate(sales_diff = sales - mean(sales))

# Create a column that is the square of tv_diff
TVdf <- TVdf |> mutate(tv_diff_sq = tv_diff^2)

# Create a column that is the multiplication of tv_diff and sales_diff
TVdf <- TVdf |> mutate(tv_diff_sales_diff = tv_diff * sales_diff)

# Calculate beta_1
beta_1 <- sum(TVdf$tv_diff_sales_diff) / sum(TVdf$tv_diff_sq)

# Calculate beta_0
beta_0 <- mean(TVdf$sales) - beta_1 * mean(TVdf$TV)

# y_hat = beta_0 + beta_1 * x
TVdf <- TVdf |> mutate(sales_pred = beta_0 + beta_1 * TV)

# -------------------------------------------------------------

# Create a linear regression between sales and TV
tv.fit <- lm(sales ~ TV, data = df)

# the summary() function gives us the p-values, t-statistics, RSE, R^2, F-statistic, etc.
summary(tv.fit)

# -------------------------------------------------------------

# Create a dataframe of the t-distribution
t_dist <- data.frame(x = seq(-3, 3, 0.01), y = dt(seq(-3, 3, 0.01), df = 198))

# plot t_dist
# add a vertical line at 2 and -2
ggplot(t_dist, aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = 2, linetype = "dashed") +
  geom_vline(xintercept = -2, linetype = "dashed")

# Check the whiteboard I drew in class. It is posted on CANVAS

# -------------------------------------------------------------

# Multiple Linear Regression

# Create a matrix of the TV, Radio and Newspaper columns
X <- df |> select(TV, radio, newspaper)

# Add a column of 1s to the matrix
X <- cbind(1, X)

# convert X into a matrix
X <- as.matrix(X)

# Calculate beta vector
# Using this method, you can calculate the beta vector for any number of variables
beta <- solve(t(X) %*% X) %*% t(X) %*% df$sales

# Calculate y_hat
y_hat <- X %*% beta

# Add y_hat to the dataframe
df <- df |> mutate(sales_pred = y_hat)

# Multi linear regression using lm() function
multi_fit <- lm(sales ~ TV + radio + newspaper, data = df)

# summary of multi_fit
summary(multi_fit)

# Multi linear regression without newspaper
multi_fit_no_news <- lm(sales ~ TV + radio, data = df)

# summary of multi_fit_no_news
summary(multi_fit_no_news)

# -------------------------------------------------------------

# Create a dataframe of the f-distribution
f_dist <- data.frame(x = seq(0, 5, 0.01), y = df(seq(0, 5, 0.01), df1 = 2, df2 = 197))

# plot f_dist
# add a vertical line at 3
ggplot(f_dist, aes(x = x, y = y)) +
  geom_line() +
  geom_vline(xintercept = 3, linetype = "dashed")

# Check the whiteboard I drew in class. It is posted on CANVAS





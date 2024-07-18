# Load the dataset from URL
df <- read.csv("https://raw.githubusercontent.com/JaySquare87/DTA395-1Rollins/main/CodeProjects/RPG.csv", header = TRUE, sep = ",")

# Code Project Starts Here.

### REGRESSION TREE

library(MASS)
library(tidymodels)

# Create a decision tree model specification
tree_spec <- decision_tree() |>
  set_engine("rpart") |>
  set_mode("regression")

# Fit the model to the training data
tree_fit <- tree_spec |>
  fit(Level ~ Armor + Weapon + Physical + Magic, data = df)

# Create new values for variables and predict
new_data <- tribble(
  ~Armor, ~Weapon, ~Physical, ~Magic,
  30, 20, 20, 10
)

# Predict
predictions <- tree_fit |>
  predict(new_data = new_data)

print(predictions)

# Make prediction on the testing set
predictions <- tree_fit |>
  predict(new_data = df) |>
  pull(.pred)

# Calculate RMSE and R-squared
model_performance <- df |>
  mutate(predictions = predictions) |>
  metrics(truth = Level, estimate = predictions)

model_performance

# Model accuracy
(1 - model_performance |> filter(.metric == "rmse") |> pull(.estimate) / mean(df$Level)) * 100


##------------------------------------------------------------------------------

### CLASSIFICATION MODEL

library(rpart)
library(rpart.plot)
library(vip)
library(tidymodels)

# Convert the 'Class' column to a factor with specified levels
df$Class <- factor(df$Class, levels = c("Battlemage", "Knight", "Magician", "Sorcerer", "Tank", "Warrior"))

# Create decision tree model
tree_model <- rpart(Class ~ Magic + Weapon, data=df, method="class")

# Plot the decision tree using rpart.plot
rpart.plot(tree_model, type=4, extra=101, under=TRUE, fallen.leaves=TRUE, cex=0.8, tweak=1.2)

# This model is ignoring the Battlemage and Knight classes as there is 
# not enough data from them to predict. These two classes are then distributed 
# between the others and are seen as uncertain. Battlemage was mostly distributed to
# Sorcerer and Knight was mostly distributed to Warrior. 

# Create a grid based on Magic and Weapon
Magic.min <- min(df$Magic)
Magic.max <- max(df$Magic)
Weapon.min <- min(df$Weapon)
Weapon.max <- max(df$Weapon)

grid <- expand.grid(
  Magic = seq(Magic.min, Magic.max, length.out = 100),
  Weapon = seq(Weapon.min, Weapon.max, length.out = 100)
)

# Predict the grid
grid$Class <- predict(tree_model, grid, type="class")

# Scatter plot of Weapon and Magic
ggplot() +
  geom_point(data=df, aes(x=Magic, y=Weapon, color=Class)) +
  geom_tile(data=grid, aes(x=Magic, y=Weapon, fill=Class), alpha=0.5) +
  labs(title="Scatter plot of Magic and Weapon", x="Magic", y="Weapon")

#install.packages("C50")
library(C50)

# Create decision tree model
tree_model_c <- C5.0(Class ~ Magic + Weapon, data=df)

grid$Class <- predict(tree_model_c, grid, type = "class")

# Scatter plot of Weapon and Magic
ggplot() +
  geom_point(data=df, aes(x=Magic, y=Weapon, color=Class)) +
  geom_tile(data=grid, aes(x=Magic, y=Weapon, fill=Class), alpha=0.5) +
  labs(title="Scatter plot of Magic and Weapon", x="Magic", y="Weapon")


##------------------------------------------------------------------------------

### USING THE C50 LIBRARY

plot(tree_model_c)

# The C50 model over fits the data WAY too much as it splits 
# the data way more than necessary and makes it impossible 
# to interpret

# Scatter plot of Weapon and Magic
ggplot() +
  geom_point(data=df, aes(x=Magic, y=Weapon, color=Class)) +
  geom_tile(data=grid, aes(x=Magic, y=Weapon, fill=Class), alpha=0.5) +
  labs(title="Scatter plot of Magic and Weapon", x="Magic", y="Weapon")

# Find accuracy
predictions_tree_model_c <- predict(tree_model_c, df, type="class")
mean(predictions_tree_model_c == df$Class)

##------------------------------------------------------------------------------

### SUPPORT VECTOR MACHINES

### USING THE DEAULT PARAMETERS OF SVM

#install.packages("e1071")
library(e1071)

# Create a SVM model with the best parameters
svm_model <- svm(Class ~ Magic + Weapon, data=df, scale = TRUE)

grid$Class <- predict(svm_model, grid, type = "class")

# Plot
ggplot() +
  geom_point(data=df, aes(x=Magic, y=Weapon, color=Class)) +
  geom_tile(data=grid, aes(x=Magic, y=Weapon, fill=Class), alpha=0.5) +
  labs(title="Scatter plot of Magic and Weapon", x="Magic", y="Weapon")

# Find accuracy
predictions_svm <- predict(svm_model, df, type="class")
mean(predictions_svm == df$Class)

# The model is not accurate at all with only a 42% accuracy.
# The only good thing is that it does not overfit like the C50 model.


### FINDING THE OPTIMAL COST AND GAMMA

# Tune the model
set.seed(1)
tune.out <- tune(svm, Class ~ Magic + Weapon, data=df, 
                 kernel="radial", 
                 ranges=list(cost=c(0.1, 1, 10, 100, 1000, 10000), 
                             gamma=c(0.5, 1, 2, 3, 4)))

# Summary of the tuning
summary(tune.out)

# Create a SVM model with optimal cost and gamma
optimal_svm_model <- svm(Class ~ Magic + Weapon, data=df, scale = TRUE, cost = 0.1, gamma = 0.5)

grid$Class <- predict(optimal_svm_model, grid, type = "class")

# Plot
ggplot() +
  geom_point(data=df, aes(x=Magic, y=Weapon, color=Class)) +
  geom_tile(data=grid, aes(x=Magic, y=Weapon, fill=Class), alpha=0.5) +
  labs(title="Scatter plot of Magic and Weapon", x="Magic", y="Weapon")

## The new plot has taken out the classes of Battlemage and Knight (overfitted less)



# Load the rpart library
library(rpart)

# Load the rpart.plot library
library(rpart.plot)

# Load the e1071 library
library(e1071)

# Load the keras3 library
library(keras3)

# Load the data
# Read the dataset from the URL
df <- read.csv("https://raw.githubusercontent.com/JaySquare87/DTA395-1Rollins/main/Midterm/songs_normalize.csv", header = TRUE, sep = ",")

# Your code starts here...

# Create decision tree model
tree_model <- rpart(explicit ~ speechiness + danceability + energy + loudness + acousticness + instrumentalness + liveness + valence + tempo, data=df, method="class")

# Plot the decision tree using rpart.plot
rpart.plot(tree_model, type=4, extra=101, under=TRUE, fallen.leaves=TRUE, cex=0.8, tweak=1.2)

# Calculate the accuracy of the model
predictions <- predict(tree_model, df, type="class")
mean(predictions == df$explicit)

# Creating the confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = df$explicit)
print(conf_matrix)

# Create a data frame for new data
new_song <- data.frame(
  speechiness = 0.04,
  danceability = 0.7,
  energy = 0.8,
  loudness = 0,  
  acousticness = 0,
  instrumentalness = 0,
  liveness = 0,
  valence = 0,
  tempo = 0,
  duration_ms = 0,
  mode = 0,
  key = 0,
  popularity = 0
)

# Predict the explicitness of the new song
prediction <- predict(tree_model, new_song, type="class")
print(prediction)


### ----------------------------------------------------------------------------


# Load the C50 library
library(C50)

df$explicit <- factor(df$explicit, levels = c("True", "False"))

# Create decision tree model
tree_model2 <- C5.0(explicit ~ speechiness + danceability + energy, data=df)

# Plot the decision tree
plot(tree_model2)

# Calculate the accuracy of the model
predictions2 <- predict(tree_model2, df, type="class")
mean(predictions2 == df$explicit)

# Creating the confusion matrix
conf_matrix2 <- table(Predicted = predictions2, Actual = df$explicit)
print(conf_matrix2)


### ----------------------------------------------------------------------------


# Create an SVM model with speechiness and energy
svm_model <- svm(explicit ~ speechiness + energy, data = df, type = "C-classification", kernel = "linear")

# Generate a grid
grid_values <- expand.grid(
  speechiness = seq(min(df$speechiness), max(df$speechiness), length.out = 100),
  energy = seq(min(df$energy), max(df$energy), length.out = 100)
)

# Predict on the grid
grid_values$explicit <- predict(svm_model, newdata = grid_values, type = "class")

library(ggplot2)

ggplot() +
  geom_point(data=df, aes(x=speechiness, y=energy, color=explicit)) +
  geom_tile(data=grid_values, aes(x=speechiness, y=energy, fill=explicit), alpha=0.5) +
  labs(title="Scatter plot of speechiness and energy", x="speechiness", y="energy")

# Calculate the accuracy of the model
predictions3 <- predict(svm_model, df, type="class")
mean(predictions3 == df$explicit)

# Creating the confusion matrix
conf_matrix3 <- table(Predicted = predictions3, Actual = df$explicit)
print(conf_matrix3)


### ----------------------------------------------------------------------------


# Preprocess the Data
df <- na.omit(df)

# Encoding species as a numeric factor
df$explicit <- as.numeric(factor(df$explicit)) - 1

# Splitting dataset into features and labels
features <- as.matrix(df[, c("speechiness", "danceability", "energy", "loudness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")])
labels <- to_categorical(df$explicit)

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
model <- keras_model_sequential(input_shape = ncol(features)) |>
  layer_dense(units = 32, activation = 'relu') |>
  layer_dense(units = 16, activation = 'relu') |>
  layer_dense(units = length(unique(df$explicit)), activation = 'softmax')

model |> compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam', 
  metrics = 'accuracy'
)

summary(model)

# Train the Model
history <- model %>% fit(
  x_train, y_train, 
  epochs = 200, 
  batch_size = 32, 
  validation_split = 0.2
)

# Evaluate the Model
evaluation <- model %>% evaluate(x_test, y_test)
print(evaluation)

plot(history)

# Construct other Neural Network
model2 <- keras_model_sequential(input_shape = ncol(features)) |>
  layer_dense(units = 32, activation = 'relu') |>
  layer_dense(units = 16, activation = 'relu') |>
  layer_dense(units = 8, activation = 'relu') |>
  layer_dense(units = length(unique(df$explicit)), activation = 'softmax')

model2 |> compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(), 
  metrics = 'accuracy'
)

summary(model2)

# Train the Model
history2 <- model2 %>% fit(
  x_train, y_train, 
  epochs = 200, 
  batch_size = 32, 
  validation_split = 0.2
)

# Evaluate the Model
evaluation2 <- model2 %>% evaluate(x_test, y_test)
print(evaluation2)

plot(history2)


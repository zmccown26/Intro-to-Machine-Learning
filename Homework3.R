# Load the MASS library
library(MASS)

# Load the tidyverse library
library(tidyverse)

# Load the wine dataset
# https://archive.ics.uci.edu/dataset/109/wine
wine <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", sep = ",", header = FALSE)
names(wine) <- c("Class", "Alcohol", "MalicAcid", "Ash", "AlcalinityOfAsh", "Magnesium", "TotalPhenols", "Flavanoids", "NonflavanoidPhenols", "Proanthocyanins", "ColorIntensity", "Hue", "OD280/OD315OfDilutedWines", "Proline")

# Homework starts here

c1norm.test <- wine$Alcohol[wine$Class == 1] |> shapiro.test()
c2norm.test <- wine$Alcohol[wine$Class == 2] |> shapiro.test()
c3norm.test <- wine$Alcohol[wine$Class == 3] |> shapiro.test()

wine_1_3 <- wine |> filter(Class %in% c(1, 3))

s1.priori_c1 <- sum(wine_1_3$Class == 1) / nrow(wine_1_3)
s1.priori_c3 <- sum(wine_1_3$Class == 3) / nrow(wine_1_3)

wine_1_3$Class <- factor(wine_1_3$Class, levels = c(1, 3))

wine13_lda_model <- lda(Class ~ Alcohol, data = wine_1_3, prior = c(s1.priori_c1, s1.priori_c3))

wine13_lda_threshold <- (mean(wine13_lda_model$means) - 
                           (log(wine13_lda_model$prior[1]/wine13_lda_model$prior[2])) / (wine13_lda_model$scaling))

wine13_lda_threshold_density <- ggplot(aes(x = Alcohol, fill = Class), data = wine_1_3) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = wine13_lda_threshold, linetype = "dashed") +
  labs(title = "Alcohol Distribution", x = "Alcohol", y = "Density") +
  theme_minimal()


#-----------------------------------------------------------------------------------------


wine13_lda_model_2 <- lda(Class ~ Alcohol + MalicAcid, data = wine_1_3, prior = c(s1.priori_c1, s1.priori_c3))

wine13_lda_model_2.slope <- -wine13_lda_model_2$scaling[1] / wine13_lda_model_2$scaling[2]

wine13_lda_model_2.intercept <- -mean(wine13_lda_model_2$means %*% wine13_lda_model_2$scaling)
wine13_lda_model_2.intercept <- -wine13_lda_model_2.intercept / wine13_lda_model_2$scaling[2]

wine3_lda_model_2_scatter <- 
  ggplot(aes(x = Alcohol, y = MalicAcid, color = Class), data = wine_1_3) +
  geom_point() +
  geom_abline(intercept = wine13_lda_model_2.intercept, slope = wine13_lda_model_2.slope) +
  labs(title = "Alcohol and Malic Acid", 
       x = "Alcohol", 
       y = "Malic Acid") +
  theme_minimal()


#-----------------------------------------------------------------------------------------


wine$Class <- factor(wine$Class, levels = c(1,2,3))

s3.priori_c1 <- sum(wine$Class == 1) / nrow(wine)
s3.priori_c2 <- sum(wine$Class == 2) / nrow(wine)
s3.priori_c3 <- sum(wine$Class == 3) / nrow(wine)

wine_lda_model <- lda(Class ~ Alcohol + MalicAcid, data = wine, prior = c(s3.priori_c1, s3.priori_c2, s3.priori_c3))

max_Alcohol = max(wine$Alcohol) + 0.2
min_Alcohol = min(wine$Alcohol) - 0.2

max_MalicAcid = max(wine$MalicAcid) + 0.2
min_MalicAcid = min(wine$MalicAcid) - 0.2

grid_x <- seq(min_Alcohol, max_Alcohol, length.out = 100)
grid_y <- seq(min_MalicAcid, max_MalicAcid, length.out = 100)

grid <- expand.grid(Alcohol = grid_x, MalicAcid = grid_y)

plot(grid)

grid$Class <- predict(wine_lda_model, newdata = grid)$class

wine_lda_model_scatter <- ggplot(data = wine, aes(x = Alcohol, y = MalicAcid, color = Class)) +
  geom_point() +
  geom_tile(data = grid, aes(x = Alcohol, y = MalicAcid, fill = Class), alpha = 0.1) +
  labs(title = "Alcohol and Malic Acid", 
       x = "Alcohol", 
       y = "Malic Acid") +
  theme_minimal()


#-----------------------------------------------------------------------------------------


wine_qda_model <- qda(Class ~ Alcohol + MalicAcid, data = wine, prior = c(s3.priori_c1, s3.priori_c2, s3.priori_c3))

grid$Class <- predict(wine_qda_model, newdata = grid)$class

wine_qda_model_scatter <- ggplot(data = wine, aes(x = Alcohol, y = MalicAcid, color = Class)) +
  geom_point() +
  geom_tile(data = grid, aes(x = Alcohol, y = MalicAcid, fill = Class), alpha = 0.1) +
  labs(title = "Alcohol and Malic Acid", 
       x = "Alcohol", 
       y = "Malic Acid") +
  theme_minimal()









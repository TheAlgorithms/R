# Perceptron

A simple linear classifier using the perceptron learning rule. This implementation supports binary classification and multiclass classification with direct multiclass perceptron updates.

``` r
library(R6)
source("../machine_learning/perceptron.r")

# example data for binary classification
X <- matrix(c(
  0.1, 0.3,
  0.2, 0.1,
  0.9, 0.8,
  0.7, 0.9
), ncol = 2, byrow = TRUE)

y <- factor(c("class1", "class1", "class2", "class2"))

model <- Perceptron$new(learning_rate = 0.1, n_epochs = 20, shuffle = FALSE, random_state = 42)
model$fit(X, y)

predictions <- model$predict(X)
print(predictions)
print(model$score(X, y))
```

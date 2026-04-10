# perceptron.r
# Perceptron classifier implementation in R
# A simple linear classifier using the perceptron learning rule.
# Supports binary classification and multiclass classification
# using direct multiclass perceptron updates.
# Time Complexity: O(n_epochs * n_samples * n_features)
# Space Complexity: O(n_classes * n_features)

library(R6)

Perceptron <- R6Class("Perceptron",
  public = list(
    learning_rate = NULL,
    n_epochs = NULL,
    shuffle = NULL,
    fit_intercept = NULL,
    random_state = NULL,
    classes = NULL,
    weights = NULL,
    is_multiclass = NULL,

    initialize = function(learning_rate = 0.1,
                          n_epochs = 100,
                          shuffle = TRUE,
                          fit_intercept = TRUE,
                          random_state = NULL) {
      self$learning_rate <- learning_rate
      self$n_epochs <- n_epochs
      self$shuffle <- shuffle
      self$fit_intercept <- fit_intercept
      self$random_state <- random_state
    },

    fit = function(X, y) {
      if (is.data.frame(X)) X <- as.matrix(X)
      if (!is.matrix(X)) stop("X must be a numeric matrix or data.frame.")
      if (!is.numeric(X)) stop("X must contain numeric features.")
      if (any(is.na(X))) stop("X must not contain missing values.")

      if (is.character(y)) y <- factor(y)
      if (is.factor(y)) {
        self$classes <- levels(y)
      } else {
        self$classes <- sort(unique(y))
      }

      if (length(y) != nrow(X)) stop("Length of y must match rows of X.")
      if (length(self$classes) < 2) stop("Perceptron requires at least two classes.")

      X <- as.matrix(X)
      n_samples <- nrow(X)
      n_features <- ncol(X)
      if (self$fit_intercept) {
        X <- cbind(1, X)
        n_features <- n_features + 1
      }

      if (length(self$classes) == 2) {
        self$is_multiclass <- FALSE
        self$weights <- rep(0, n_features)
      } else {
        self$is_multiclass <- TRUE
        self$weights <- matrix(0, nrow = length(self$classes), ncol = n_features)
      }

      if (!is.null(self$random_state)) {
        set.seed(self$random_state)
      }

      y_encoded <- self$encode_labels(y)

      for (epoch in seq_len(self$n_epochs)) {
        indices <- seq_len(n_samples)
        if (self$shuffle) {
          indices <- sample(indices)
        }

        for (i in indices) {
          x_i <- X[i, ]
          y_i <- y_encoded[i]

          if (self$is_multiclass) {
            scores <- self$weights %*% x_i
            predicted <- which.max(scores)
            if (predicted != y_i) {
              self$weights[y_i, ] <- self$weights[y_i, ] + self$learning_rate * x_i
              self$weights[predicted, ] <- self$weights[predicted, ] - self$learning_rate * x_i
            }
          } else {
            score <- sum(self$weights * x_i)
            if (y_i * score <= 0) {
              self$weights <- self$weights + self$learning_rate * y_i * x_i
            }
          }
        }
      }

      invisible(self)
    },

    predict = function(X_new) {
      if (is.data.frame(X_new)) X_new <- as.matrix(X_new)
      if (is.vector(X_new)) X_new <- matrix(X_new, nrow = 1)
      if (!is.matrix(X_new)) stop("X_new must be a numeric matrix, data.frame, or vector.")
      if (!is.numeric(X_new)) stop("X_new must contain numeric features.")
      if (any(is.na(X_new))) stop("X_new must not contain missing values.")

      if (self$fit_intercept) {
        X_new <- cbind(1, X_new)
      }

      if (self$is_multiclass) {
        scores <- X_new %*% t(self$weights)
        predicted_idx <- apply(scores, 1, which.max)
        return(self$classes[predicted_idx])
      }

      raw_scores <- as.numeric(X_new %*% self$weights)
      labels <- self$classes
      predictions <- ifelse(raw_scores >= 0, labels[2], labels[1])
      return(predictions)
    },

    score = function(X, y) {
      predictions <- self$predict(X)
      if (is.factor(y) || is.character(y)) {
        y <- as.character(y)
        predictions <- as.character(predictions)
      }
      mean(predictions == y)
    },

    encode_labels = function(y) {
      if (self$is_multiclass) {
        if (is.factor(y)) {
          return(as.integer(y))
        }
        return(match(y, self$classes))
      }

      if (is.factor(y)) {
        y <- as.character(y)
      }
      labels <- self$classes
      if (is.factor(labels)) {
        labels <- as.character(labels)
      }
      if (is.null(labels) || length(labels) == 0) {
        labels <- unique(y)
        if (length(labels) != 2) stop("Binary perceptron requires exactly two classes.")
        self$classes <- labels
      } else {
        if (length(labels) != 2) stop("Binary perceptron requires exactly two classes.")
      }
      if (any(!y %in% labels)) {
        stop("Binary perceptron received labels not present in self$classes.")
      }
      y_bin <- ifelse(y == labels[2], 1, -1)
      return(y_bin)
    }
  )
)

# Example usage:
# data(iris)
# X <- as.matrix(iris[, 1:4])
# y <- iris$Species
# model <- Perceptron$new(learning_rate = 0.1, n_epochs = 50, shuffle = TRUE)
# model$fit(X, y)
# preds <- model$predict(X)
# cat('Training accuracy:', model$score(X, y), '\n')

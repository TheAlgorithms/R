# gradient_boosting.r
# Gradient Boosting Algorithm Implementation in R
# A sequential ensemble learning method that builds models iteratively
# Each new model corrects errors made by previous models
#
# Algorithm details:
# - Uses decision trees as weak learners
# - Fits each tree to the residuals (errors) of previous predictions
# - Combines predictions using weighted sum
# - Learning rate controls contribution of each tree
# - Time complexity: O(n_trees * n_samples * log(n_samples))
# - Space complexity: O(n_trees * tree_size)

library(R6)

#' Decision Tree Node
#' Simple decision tree implementation for gradient boosting
DecisionTreeNode <- R6Class(
  "DecisionTreeNode",
  
  public = list(
    feature = NULL,
    threshold = NULL,
    left = NULL,
    right = NULL,
    value = NULL,
    
    initialize = function(feature = NULL, threshold = NULL, 
                         left = NULL, right = NULL, value = NULL) {
      self$feature <- feature
      self$threshold <- threshold
      self$left <- left
      self$right <- right
      self$value <- value
    },
    
    is_leaf = function() {
      return(!is.null(self$value))
    }
  )
)

#' Regression Tree for Gradient Boosting
RegressionTree <- R6Class(
  "RegressionTree",
  
  public = list(
    max_depth = NULL,
    min_samples_split = NULL,
    root = NULL,
    
    initialize = function(max_depth = 3, min_samples_split = 2) {
      self$max_depth <- max_depth
      self$min_samples_split <- min_samples_split
      self$root <- NULL
    },
    
    fit = function(X, y) {
      "Build the decision tree"
      self$root <- private$build_tree(X, y, depth = 0)
      invisible(self)
    },
    
    predict = function(X) {
      "Predict values for input data"
      if (is.vector(X)) {
        X <- matrix(X, nrow = 1)
      }
      apply(X, 1, function(row) private$predict_sample(row, self$root))
    }
  ),
  
  private = list(
    build_tree = function(X, y, depth) {
      "Recursively build decision tree"
      n_samples <- nrow(X)
      
      # Stopping criteria
      if (depth >= self$max_depth || 
          n_samples < self$min_samples_split ||
          length(unique(y)) == 1) {
        return(DecisionTreeNode$new(value = mean(y)))
      }
      
      # Find best split
      best_split <- private$find_best_split(X, y)
      
      if (is.null(best_split)) {
        return(DecisionTreeNode$new(value = mean(y)))
      }
      
      # Split data
      left_idx <- X[, best_split$feature] <= best_split$threshold
      right_idx <- !left_idx
      
      # Build subtrees
      left_subtree <- private$build_tree(
        X[left_idx, , drop = FALSE], 
        y[left_idx], 
        depth + 1
      )
      right_subtree <- private$build_tree(
        X[right_idx, , drop = FALSE], 
        y[right_idx], 
        depth + 1
      )
      
      return(DecisionTreeNode$new(
        feature = best_split$feature,
        threshold = best_split$threshold,
        left = left_subtree,
        right = right_subtree
      ))
    },
    
    find_best_split = function(X, y) {
      "Find the best feature and threshold to split on"
      best_mse <- Inf
      best_split <- NULL
      n_features <- ncol(X)
      n_samples <- nrow(X)
      min_samples_leaf <- max(1, floor(self$min_samples_split / 2))
      
      for (feature in 1:n_features) {
        # Sort feature values and corresponding targets
        sorted_idx <- order(X[, feature])
        sorted_x <- X[sorted_idx, feature]
        sorted_y <- y[sorted_idx]
        
        # Consider only unique values as potential thresholds
        unique_vals <- unique(sorted_x)
        if (length(unique_vals) <= 1) next
        
        # Pre-compute cumulative statistics
        cum_sum <- cumsum(sorted_y)
        cum_sum_sq <- cumsum(sorted_y^2)
        
        # Evaluate splits between unique values
        for (i in 1:(length(unique_vals)-1)) {
          threshold <- (unique_vals[i] + unique_vals[i+1]) / 2
          split_idx <- which(sorted_x <= threshold)
          n_left <- length(split_idx)
          n_right <- n_samples - n_left
          
          # Skip if split doesn't meet minimum samples requirement
          if (n_left < min_samples_leaf || n_right < min_samples_leaf) next
          
          # Calculate MSE using pre-computed statistics
          left_sum <- cum_sum[n_left]
          left_sum_sq <- cum_sum_sq[n_left]
          right_sum <- cum_sum[n_samples] - left_sum
          right_sum_sq <- cum_sum_sq[n_samples] - left_sum_sq
          
          left_mse <- (left_sum_sq - (left_sum^2)/n_left)
          right_mse <- (right_sum_sq - (right_sum^2)/n_right)
          mse <- left_mse + right_mse
          
          if (mse < best_mse) {
            best_mse <- mse
            best_split <- list(feature = feature, threshold = threshold)
          }
        }
      }
      
      return(best_split)
    },
    
    predict_sample = function(x, node) {
      "Predict single sample by traversing tree"
      if (node$is_leaf()) {
        return(node$value)
      }
      
      if (x[node$feature] <= node$threshold) {
        return(private$predict_sample(x, node$left))
      } else {
        return(private$predict_sample(x, node$right))
      }
    }
  )
)

#' Gradient Boosting Regressor
GradientBoostingRegressor <- R6Class(
  "GradientBoostingRegressor",
  
  public = list(
    n_estimators = NULL,
    learning_rate = NULL,
    max_depth = NULL,
    min_samples_split = NULL,
    trees = NULL,
    initial_prediction = NULL,
    early_stopping_rounds = NULL,
    best_iteration = NULL,
    
    initialize = function(n_estimators = 100, learning_rate = 0.1, 
                         max_depth = 3, min_samples_split = 2,
                         early_stopping_rounds = NULL) {
      "Initialize gradient boosting parameters"
      if (n_estimators <= 0 || learning_rate <= 0 || max_depth <= 0) {
        stop("Parameters must be positive")
      }
      
      self$n_estimators <- n_estimators
      self$learning_rate <- learning_rate
      self$max_depth <- max_depth
      self$min_samples_split <- min_samples_split
      self$trees <- list()
      self$initial_prediction <- NULL
    },
    
    fit = function(X, y, verbose = FALSE) {
      "Train the gradient boosting model"
      # Input validation
      if (!is.numeric(y)) {
        stop("Target variable 'y' must be numeric")
      }
      if (is.vector(X)) {
        X <- matrix(X, ncol = 1)
      }
      if (!is.matrix(X) || !is.numeric(X)) {
        stop("Input 'X' must be a numeric matrix or vector")
      }
      if (length(y) != nrow(X)) {
        stop("Number of samples in X and y must match")
      }
      if (any(is.na(X)) || any(is.na(y))) {
        stop("Input contains missing values")
      }
      
      # Initialize with mean of target
      self$initial_prediction <- mean(y)
      predictions <- rep(self$initial_prediction, length(y))
      
      # Build trees sequentially
      for (i in 1:self$n_estimators) {
        # Calculate residuals (negative gradient for MSE loss)
        residuals <- y - predictions
        
        # Fit tree to residuals
        tree <- RegressionTree$new(
          max_depth = self$max_depth,
          min_samples_split = self$min_samples_split
        )
        tree$fit(X, residuals)
        
        # Update predictions
        tree_predictions <- tree$predict(X)
        predictions <- predictions + self$learning_rate * tree_predictions
        
        # Store tree
        self$trees[[i]] <- tree
        
        # Calculate training error
        if (verbose && (i %% 10 == 0 || i == 1)) {
          mse <- mean((y - predictions)^2)
          rmse <- sqrt(mse)
          cat(sprintf("Iteration %d/%d - RMSE: %.4f\n", 
                     i, self$n_estimators, rmse))
        }
      }
      
      invisible(self)
    },
    
    predict = function(X) {
      "Make predictions using the trained model"
      if (is.null(self$initial_prediction)) {
        stop("Model has not been fitted yet")
      }
      
      if (is.vector(X)) {
        X <- matrix(X, ncol = 1)
      }
      
      # Start with initial prediction
      predictions <- rep(self$initial_prediction, nrow(X))
      
      # Add contribution from each tree
      for (tree in self$trees) {
        predictions <- predictions + self$learning_rate * tree$predict(X)
      }
      
      return(predictions)
    },
    
    score = function(X, y) {
      "Calculate R-squared score"
      predictions <- self$predict(X)
      ss_res <- sum((y - predictions)^2)
      ss_tot <- sum((y - mean(y))^2)
      r2 <- 1 - (ss_res / ss_tot)
      return(r2)
    },
    
    get_feature_importance = function() {
      "Calculate relative feature importance based on split frequency and gain"
      if (length(self$trees) == 0) {
        stop("Model has not been fitted yet")
      }
      
      # Count feature usage and gain in splits
      n_features <- ncol(X)  # Assumes X from last fit
      importance <- rep(0, n_features)
      names(importance) <- paste0("Feature_", 1:n_features)
      
      calculate_tree_importance <- function(node, depth = 0) {
        if (is.null(node) || node$is_leaf()) {
          return(NULL)
        }
        
        # Add importance score based on depth (earlier splits are more important)
        feature_idx <- node$feature
        importance[feature_idx] <<- importance[feature_idx] + 1 / (depth + 1)
        
        calculate_tree_importance(node$left, depth + 1)
        calculate_tree_importance(node$right, depth + 1)
      }
      
      # Calculate importance for each tree
      for (tree in self$trees) {
        calculate_tree_importance(tree$root)
      }
      
      # Normalize importance scores
      if (sum(importance) > 0) {
        importance <- importance / sum(importance)
      }
      
      # Sort and return as named vector
      importance <- sort(importance, decreasing = TRUE)
      return(importance)
    }
  )
)

# Demonstration and testing
demonstrate_gradient_boosting <- function() {
  cat("=== Gradient Boosting Algorithm Demo ===\n\n")
  
  # Generate synthetic dataset
  set.seed(42)
  n_samples <- 200
  
  cat("Example 1: Non-linear regression problem\n")
  cat("Generating synthetic data...\n\n")
  
  # Create non-linear relationship
  X <- matrix(runif(n_samples, -3, 3), ncol = 1)
  y <- sin(X[, 1]) + 0.3 * X[, 1]^2 + rnorm(n_samples, 0, 0.1)
  
  # Split into train and test
  train_idx <- sample(1:n_samples, size = 0.8 * n_samples)
  test_idx <- setdiff(1:n_samples, train_idx)
  
  X_train <- matrix(X[train_idx, ], ncol = 1)
  y_train <- y[train_idx]
  X_test <- matrix(X[test_idx, ], ncol = 1)
  y_test <- y[test_idx]
  
  # Train model
  cat("Training Gradient Boosting model...\n")
  model <- GradientBoostingRegressor$new(
    n_estimators = 50,
    learning_rate = 0.1,
    max_depth = 3
  )
  model$fit(X_train, y_train, verbose = TRUE)
  
  # Make predictions
  cat("\nEvaluating model...\n")
  train_pred <- model$predict(X_train)
  test_pred <- model$predict(X_test)
  
  # Calculate metrics
  train_rmse <- sqrt(mean((y_train - train_pred)^2))
  test_rmse <- sqrt(mean((y_test - test_pred)^2))
  train_r2 <- model$score(X_train, y_train)
  test_r2 <- model$score(X_test, y_test)
  
  cat(sprintf("\nResults:\n"))
  cat(sprintf("Train RMSE: %.4f | Train R²: %.4f\n", train_rmse, train_r2))
  cat(sprintf("Test RMSE:  %.4f | Test R²:  %.4f\n\n", test_rmse, test_r2))
  
  # Example 2: Multi-feature problem
  cat("Example 2: Multi-feature regression\n")
  cat("Generating multi-dimensional data...\n")
  
  X_multi <- matrix(rnorm(n_samples * 3), ncol = 3)
  y_multi <- 2 * X_multi[, 1] - 3 * X_multi[, 2] + 
             0.5 * X_multi[, 3]^2 + rnorm(n_samples, 0, 0.5)
  
  # Train-test split
  X_train_multi <- X_multi[train_idx, ]
  y_train_multi <- y_multi[train_idx]
  X_test_multi <- X_multi[test_idx, ]
  y_test_multi <- y_multi[test_idx]
  
  # Train model
  model2 <- GradientBoostingRegressor$new(
    n_estimators = 50,
    learning_rate = 0.1,
    max_depth = 4
  )
  model2$fit(X_train_multi, y_train_multi)
  
  # Evaluate
  test_rmse2 <- sqrt(mean((y_test_multi - model2$predict(X_test_multi))^2))
  test_r2_2 <- model2$score(X_test_multi, y_test_multi)
  
  cat(sprintf("Test RMSE: %.4f | Test R²: %.4f\n\n", test_rmse2, test_r2_2))
  
  # Example 3: Hyperparameter comparison
  cat("Example 3: Impact of learning rate\n")
  learning_rates <- c(0.01, 0.1, 0.5)
  
  for (lr in learning_rates) {
    model_lr <- GradientBoostingRegressor$new(
      n_estimators = 30,
      learning_rate = lr,
      max_depth = 3
    )
    model_lr$fit(X_train, y_train, verbose = FALSE)
    test_rmse_lr <- sqrt(mean((y_test - model_lr$predict(X_test))^2))
    cat(sprintf("Learning rate %.2f - Test RMSE: %.4f\n", lr, test_rmse_lr))
  }
  
  cat("\n=== Demo Complete ===\n")
}

# Run demonstration
if (!interactive()) {
  demonstrate_gradient_boosting()
}
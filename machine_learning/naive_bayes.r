# naive_bayes.r
# Naive Bayes Classification Algorithm
# Probabilistic classifier based on Bayes' Theorem with independence assumption
# Supports Gaussian, Multinomial, and Bernoulli variants

library(R6)

#' Gaussian Naive Bayes Classifier
#' For continuous features - assumes normal distribution
GaussianNB <- R6Class("GaussianNB",
  public = list(
    classes = NULL,
    class_priors = NULL,
    feature_means = NULL,
    feature_vars = NULL,
    epsilon = NULL,
    
    initialize = function(epsilon = 1e-9) {
      self$epsilon <- epsilon
    },
    
    fit = function(X, y) {
      if (!is.matrix(X)) X <- as.matrix(X)
      
      self$classes <- unique(y)
      n_classes <- length(self$classes)
      n_features <- ncol(X)
      n_samples <- nrow(X)
      
      # Calculate class priors P(class)
      self$class_priors <- sapply(self$classes, function(c) sum(y == c) / n_samples)
      
      # Calculate feature statistics for each class
      self$feature_means <- matrix(0, nrow = n_classes, ncol = n_features)
      self$feature_vars <- matrix(0, nrow = n_classes, ncol = n_features)
      
      for (i in 1:n_classes) {
        class_samples <- X[y == self$classes[i], , drop = FALSE]
        self$feature_means[i, ] <- colMeans(class_samples)
        self$feature_vars[i, ] <- apply(class_samples, 2, var)
        # Replace NA variances (single-sample or all-constant) with epsilon
        self$feature_vars[i, is.na(self$feature_vars[i, ])] <- self$epsilon
        # Ensure minimum variance threshold
        self$feature_vars[i, ] <- pmax(self$feature_vars[i, ], self$epsilon)
      }
      
      invisible(self)
    },
    
    predict = function(X) {
      if (!is.matrix(X)) X <- as.matrix(X)
      
      probs <- self$predict_proba(X)
      predictions <- self$classes[apply(probs, 1, which.max)]
      return(predictions)
    },
    
    predict_proba = function(X) {
      if (!is.matrix(X)) X <- as.matrix(X)
      
      n_samples <- nrow(X)
      n_classes <- length(self$classes)
      log_probs <- matrix(0, nrow = n_samples, ncol = n_classes)
      
      for (i in 1:n_classes) {
        # Log of class prior
        log_prior <- log(self$class_priors[i])
        
        # Log likelihood for each feature (Gaussian)
        log_likelihood <- 0
        for (j in 1:ncol(X)) {
          mean_val <- self$feature_means[i, j]
          var_val <- self$feature_vars[i, j]
          
          # Log of Gaussian PDF
          log_likelihood <- log_likelihood + 
            dnorm(X[, j], mean = mean_val, sd = sqrt(var_val), log = TRUE)
        }
        
        log_probs[, i] <- log_prior + log_likelihood
      }
      
      # Convert log probabilities to probabilities
      probs <- exp(log_probs - apply(log_probs, 1, max))
      probs <- probs / rowSums(probs)
      
      colnames(probs) <- self$classes
      return(probs)
    },
    
    score = function(X, y) {
      predictions <- self$predict(X)
      return(mean(predictions == y))
    }
  )
)

#' Multinomial Naive Bayes Classifier
#' For discrete count features (e.g., word counts in text)
MultinomialNB <- R6Class("MultinomialNB",
  public = list(
    classes = NULL,
    class_priors = NULL,
    feature_log_probs = NULL,
    alpha = NULL,
    
    initialize = function(alpha = 1.0) {
      self$alpha <- alpha  # Laplace smoothing parameter
    },
    
    fit = function(X, y) {
      if (!is.matrix(X)) X <- as.matrix(X)
      
      self$classes <- unique(y)
      n_classes <- length(self$classes)
      n_features <- ncol(X)
      n_samples <- nrow(X)
      
      # Calculate class priors P(class)
      self$class_priors <- sapply(self$classes, function(c) sum(y == c) / n_samples)
      
      # Calculate feature probabilities for each class
      self$feature_log_probs <- matrix(0, nrow = n_classes, ncol = n_features)
      
      for (i in 1:n_classes) {
        class_samples <- X[y == self$classes[i], , drop = FALSE]
        
        # Count total features in class
        feature_counts <- colSums(class_samples)
        
        # Apply Laplace smoothing
        smoothed_counts <- feature_counts + self$alpha
        total_count <- sum(smoothed_counts)
        
        # Log probabilities
        self$feature_log_probs[i, ] <- log(smoothed_counts / total_count)
      }
      
      invisible(self)
    },
    
    predict = function(X) {
      if (!is.matrix(X)) X <- as.matrix(X)
      
      probs <- self$predict_proba(X)
      predictions <- self$classes[apply(probs, 1, which.max)]
      return(predictions)
    },
    
    predict_proba = function(X) {
      if (!is.matrix(X)) X <- as.matrix(X)
      
      n_samples <- nrow(X)
      n_classes <- length(self$classes)
      log_probs <- matrix(0, nrow = n_samples, ncol = n_classes)
      
      for (i in 1:n_classes) {
        # Log of class prior
        log_prior <- log(self$class_priors[i])
        
        # Log likelihood
        log_likelihood <- X %*% self$feature_log_probs[i, ]
        
        log_probs[, i] <- log_prior + log_likelihood
      }
      
      # Convert to probabilities
      probs <- exp(log_probs - apply(log_probs, 1, max))
      probs <- probs / rowSums(probs)
      
      colnames(probs) <- self$classes
      return(probs)
    },
    
    score = function(X, y) {
      predictions <- self$predict(X)
      return(mean(predictions == y))
    }
  )
)

#' Bernoulli Naive Bayes Classifier
#' For binary features (presence/absence)
BernoulliNB <- R6Class("BernoulliNB",
  public = list(
    classes = NULL,
    class_priors = NULL,
    feature_probs = NULL,
    alpha = NULL,
    binarize = NULL,
    
    initialize = function(alpha = 1.0, binarize = 0.0) {
      self$alpha <- alpha  # Laplace smoothing
      self$binarize <- binarize  # Threshold for binarization
    },
    
    fit = function(X, y) {
      if (!is.matrix(X)) X <- as.matrix(X)
      
      # Binarize features if threshold is set
      if (!is.null(self$binarize)) {
        X <- ifelse(X > self$binarize, 1, 0)
      }
      
      self$classes <- unique(y)
      n_classes <- length(self$classes)
      n_features <- ncol(X)
      n_samples <- nrow(X)
      
      # Calculate class priors P(class)
      self$class_priors <- sapply(self$classes, function(c) sum(y == c) / n_samples)
      
      # Calculate feature probabilities for each class
      self$feature_probs <- matrix(0, nrow = n_classes, ncol = n_features)
      
      for (i in 1:n_classes) {
        class_samples <- X[y == self$classes[i], , drop = FALSE]
        n_class_samples <- nrow(class_samples)
        
        # Count feature occurrences with Laplace smoothing
        feature_counts <- colSums(class_samples) + self$alpha
        total_count <- n_class_samples + 2 * self$alpha
        
        # Probability of feature = 1
        self$feature_probs[i, ] <- feature_counts / total_count
      }
      
      invisible(self)
    },
    
    predict = function(X) {
      if (!is.matrix(X)) X <- as.matrix(X)
      
      probs <- self$predict_proba(X)
      predictions <- self$classes[apply(probs, 1, which.max)]
      return(predictions)
    },
    
    predict_proba = function(X) {
      if (!is.matrix(X)) X <- as.matrix(X)
      
      # Binarize features if threshold is set
      if (!is.null(self$binarize)) {
        X <- ifelse(X > self$binarize, 1, 0)
      }
      
      n_samples <- nrow(X)
      n_classes <- length(self$classes)
      log_probs <- matrix(0, nrow = n_samples, ncol = n_classes)
      
      for (i in 1:n_classes) {
        # Log of class prior
        log_prior <- log(self$class_priors[i])
        
        # Log likelihood for Bernoulli
        log_likelihood <- 0
        for (j in 1:ncol(X)) {
          p <- self$feature_probs[i, j]
          # P(x_j | class) = p^x_j * (1-p)^(1-x_j)
          log_likelihood <- log_likelihood + 
            X[, j] * log(p + 1e-10) + (1 - X[, j]) * log(1 - p + 1e-10)
        }
        
        log_probs[, i] <- log_prior + log_likelihood
      }
      
      # Convert to probabilities
      probs <- exp(log_probs - apply(log_probs, 1, max))
      probs <- probs / rowSums(probs)
      
      colnames(probs) <- self$classes
      return(probs)
    },
    
    score = function(X, y) {
      predictions <- self$predict(X)
      return(mean(predictions == y))
    }
  )
)

# Example 1: Gaussian Naive Bayes for continuous data
if (FALSE) {
  cat("\n=== Example 1: Gaussian Naive Bayes (Iris Dataset) ===\n\n")
  
  # Load iris dataset
  data(iris)
  set.seed(42)
  
  # Split data
  train_idx <- sample(1:nrow(iris), 0.8 * nrow(iris))
  X_train <- as.matrix(iris[train_idx, 1:4])
  y_train <- iris$Species[train_idx]
  X_test <- as.matrix(iris[-train_idx, 1:4])
  y_test <- iris$Species[-train_idx]
  
  # Train Gaussian Naive Bayes
  gnb <- GaussianNB$new()
  gnb$fit(X_train, y_train)
  
  # Predictions
  predictions <- gnb$predict(X_test)
  accuracy <- gnb$score(X_test, y_test)
  
  cat(sprintf("Accuracy: %.2f%%\n", accuracy * 100))
  
  # Confusion matrix
  cat("\nConfusion Matrix:\n")
  print(table(Predicted = predictions, Actual = y_test))
  
  # Probability predictions
  probs <- gnb$predict_proba(X_test[1:5, ])
  cat("\nPrediction Probabilities (first 5 samples):\n")
  print(round(probs, 4))
  
  # Example 2: Multinomial Naive Bayes for text classification
  cat("\n=== Example 2: Multinomial Naive Bayes (Text Classification) ===\n\n")
  
  # Simulate word count data (document-term matrix)
  set.seed(42)
  
  # Class 0: Sports documents (high counts for sports words)
  sports_docs <- matrix(c(
    rpois(50, lambda = 10),  # word 1: "game"
    rpois(50, lambda = 8),   # word 2: "team"
    rpois(50, lambda = 2),   # word 3: "politics"
    rpois(50, lambda = 1),   # word 4: "election"
    rpois(50, lambda = 5)    # word 5: "player"
  ), ncol = 5)
  
  # Class 1: Politics documents (high counts for politics words)
  politics_docs <- matrix(c(
    rpois(50, lambda = 2),   # word 1: "game"
    rpois(50, lambda = 1),   # word 2: "team"
    rpois(50, lambda = 10),  # word 3: "politics"
    rpois(50, lambda = 8),   # word 4: "election"
    rpois(50, lambda = 1)    # word 5: "player"
  ), ncol = 5)
  
  X <- rbind(sports_docs, politics_docs)
  y <- c(rep("Sports", 50), rep("Politics", 50))
  
  # Split data
  train_idx <- sample(1:nrow(X), 0.8 * nrow(X))
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test <- X[-train_idx, ]
  y_test <- y[-train_idx]
  
  # Train Multinomial Naive Bayes
  mnb <- MultinomialNB$new(alpha = 1.0)
  mnb$fit(X_train, y_train)
  
  # Predictions
  predictions <- mnb$predict(X_test)
  accuracy <- mnb$score(X_test, y_test)
  
  cat(sprintf("Accuracy: %.2f%%\n", accuracy * 100))
  
  cat("\nConfusion Matrix:\n")
  print(table(Predicted = predictions, Actual = y_test))
  
  # Example 3: Bernoulli Naive Bayes for binary features
  cat("\n=== Example 3: Bernoulli Naive Bayes (Binary Features) ===\n\n")
  
  # Simulate binary feature data (spam detection)
  set.seed(42)
  
  # Class 0: Ham (normal email)
  ham_emails <- matrix(c(
    rbinom(50, 1, 0.2),  # feature 1: contains "free"
    rbinom(50, 1, 0.1),  # feature 2: contains "winner"
    rbinom(50, 1, 0.7),  # feature 3: contains "meeting"
    rbinom(50, 1, 0.6),  # feature 4: contains "project"
    rbinom(50, 1, 0.1)   # feature 5: contains "cash"
  ), ncol = 5)
  
  # Class 1: Spam
  spam_emails <- matrix(c(
    rbinom(50, 1, 0.9),  # feature 1: contains "free"
    rbinom(50, 1, 0.8),  # feature 2: contains "winner"
    rbinom(50, 1, 0.1),  # feature 3: contains "meeting"
    rbinom(50, 1, 0.1),  # feature 4: contains "project"
    rbinom(50, 1, 0.9)   # feature 5: contains "cash"
  ), ncol = 5)
  
  X <- rbind(ham_emails, spam_emails)
  y <- c(rep("Ham", 50), rep("Spam", 50))
  
  # Split data
  train_idx <- sample(1:nrow(X), 0.8 * nrow(X))
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test <- X[-train_idx, ]
  y_test <- y[-train_idx]
  
  # Train Bernoulli Naive Bayes
  bnb <- BernoulliNB$new(alpha = 1.0)
  bnb$fit(X_train, y_train)
  
  # Predictions
  predictions <- bnb$predict(X_test)
  accuracy <- bnb$score(X_test, y_test)
  
  cat(sprintf("Accuracy: %.2f%%\n", accuracy * 100))
  
  cat("\nConfusion Matrix:\n")
  print(table(Predicted = predictions, Actual = y_test))
  
  # Show probability predictions for spam detection
  probs <- bnb$predict_proba(X_test[1:5, ])
  cat("\nSpam Probabilities (first 5 emails):\n")
  print(round(probs, 4))
  
  # Example 4: Comparison of variants
  cat("\n=== Example 4: Performance Comparison ===\n\n")
  
  # Generate mixed dataset
  set.seed(42)
  n_samples <- 200
  
  X_class1 <- matrix(rnorm(n_samples * 4, mean = 0, sd = 1), ncol = 4)
  X_class2 <- matrix(rnorm(n_samples * 4, mean = 2, sd = 1), ncol = 4)
  X <- rbind(X_class1, X_class2)
  y <- c(rep("A", n_samples), rep("B", n_samples))
  
  # Split data
  train_idx <- sample(1:nrow(X), 0.8 * nrow(X))
  X_train <- X[train_idx, ]
  y_train <- y[train_idx]
  X_test <- X[-train_idx, ]
  y_test <- y[-train_idx]
  
  # Gaussian NB (best for continuous data)
  gnb <- GaussianNB$new()
  gnb$fit(X_train, y_train)
  gnb_acc <- gnb$score(X_test, y_test)
  
  # Multinomial NB (requires non-negative counts)
  X_train_pos <- abs(X_train)
  X_test_pos <- abs(X_test)
  mnb <- MultinomialNB$new()
  mnb$fit(X_train_pos, y_train)
  mnb_acc <- mnb$score(X_test_pos, y_test)
  
  # Bernoulli NB (binarizes data)
  bnb <- BernoulliNB$new(binarize = 0.0)
  bnb$fit(X_train, y_train)
  bnb_acc <- bnb$score(X_test, y_test)
  
  cat("Accuracy Comparison:\n")
  cat(sprintf("  Gaussian NB:    %.2f%%\n", gnb_acc * 100))
  cat(sprintf("  Multinomial NB: %.2f%%\n", mnb_acc * 100))
  cat(sprintf("  Bernoulli NB:   %.2f%%\n", bnb_acc * 100))
  
  cat("\n✅ Gaussian NB works best for continuous normally-distributed data\n")
  cat("⚙️ Multinomial NB works best for discrete count data (text)\n")
  cat("💡 Bernoulli NB works best for binary features (presence/absence)\n")
}

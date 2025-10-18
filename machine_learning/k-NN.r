# k-Nearest Neighbors implementation in R
#
# Purpose: Simple, readable k-NN from-scratch supporting classification and regression.
# Time Complexity: O(m * n * d) for m test samples, n train samples, d features.
# Space Complexity: O(n * d) for training data + O(m * n) temporarily for distance matrix.
#
# Features:
# - Euclidean distance (squared) computed efficiently with matrix ops
# - Supports classification (factor labels) and regression (numeric labels)
# - Optional distance weighting (inverse-distance)
# - Optional normalization (z-score) using training-set params
# - Safe handling of edge cases (k > n, empty data, NAs)
#
# Usage:
#  model <- knn_train(train_X, train_y, k=5, weighted=TRUE, normalize=TRUE)
#  pred <- knn_predict(model, test_X)
#  pred$predictions  # vector of predictions
#  pred$probs        # (classification) matrix of class probabilities (if requested)

# ---------------------------
# Helpers: z-score normalization
# ---------------------------
zscore_fit <- function(X) {
  mu <- colMeans(X, na.rm = TRUE)
  sigma <- apply(X, 2, sd, na.rm = TRUE)
  sigma[sigma == 0] <- 1.0   # avoid division by zero
  list(mu = mu, sigma = sigma)
}

zscore_transform <- function(X, fit) {
  sweep(sweep(X, 2, fit$mu, "-"), 2, fit$sigma, "/")
}

# ---------------------------
# Training: just store data + normalization params
# ---------------------------
knn_train <- function(X, y, k = 3, weighted = FALSE, normalize = TRUE) {
  #' X: numeric matrix or data.frame (n x d)
  #' y: factor (classification) or numeric vector (regression) of length n
  #' k: number of neighbors
  #' weighted: use inverse-distance weighting (TRUE/FALSE)
  #' normalize: z-score features using train stats (TRUE/FALSE)
  
  if (is.data.frame(X)) X <- as.matrix(X)
  if (!is.matrix(X)) stop("X must be a matrix or data.frame.")
  if (nrow(X) == 0) stop("Training set X is empty.")
  if (length(y) != nrow(X)) stop("Length of y must match number of rows in X.")
  if (k <= 0) stop("k must be positive integer.")
  
  k <- as.integer(k)
  if (k > nrow(X)) {
    warning("k > n (train size). Reducing k to n.")
    k <- nrow(X)
  }
  
  # remove rows with NA in features or labels
  good_idx <- which(apply(X, 1, function(r) !any(is.na(r))) & !is.na(y))
  if (length(good_idx) < nrow(X)) {
    warning(sprintf("Removed %d rows with NA from training data.", nrow(X) - length(good_idx)))
    X <- X[good_idx, , drop = FALSE]
    y <- y[good_idx]
  }
  
  is_classification <- is.factor(y) || is.character(y)
  if (is.character(y)) y <- factor(y)
  
  norm_fit <- NULL
  if (normalize) {
    norm_fit <- zscore_fit(X)
    X <- zscore_transform(X, norm_fit)
  }
  
  list(
    X = X,
    y = y,
    k = k,
    weighted = as.logical(weighted),
    normalize = as.logical(normalize),
    norm_fit = norm_fit,
    is_classification = is_classification,
    classes = if (is_classification) levels(y) else NULL
  )
}

# ---------------------------
# Distance computation (efficient)
# ---------------------------
squared_euclidean_distances <- function(A, B) {
  #' Compute squared Euclidean distances between rows of A (m x d) and B (n x d)
  #' Returns matrix (m x n) where entry (i,j) is ||A[i,] - B[j,]||^2
  if (!is.matrix(A)) A <- as.matrix(A)
  if (!is.matrix(B)) B <- as.matrix(B)
  if (ncol(A) != ncol(B)) stop("Feature dimension mismatch between A and B.")
  
  # rowSums(A^2) is length m; rowSums(B^2) is length n
  A_sq <- rowSums(A * A)
  B_sq <- rowSums(B * B)
  # cross term: A %*% t(B) gives m x n
  cross <- tcrossprod(A, B)  # same as A %*% t(B) but often a bit faster
  # use broadcasting: dist^2 = A_sq - 2*cross + B_sq
  # We build matrix: outer(A_sq, rep(1,n)) - 2*cross + outer(rep(1,m), B_sq)
  outer(A_sq, rep(1, length(B_sq))) - 2 * cross + outer(rep(1, length(A_sq)), B_sq)
}

# ---------------------------
# Prediction
# ---------------------------
knn_predict <- function(model, X_new, return_probs = TRUE, return_neighbors = FALSE) {
  #' model: object from knn_train
  #' X_new: matrix/data.frame of test points (m x d) or single vector (1 x d)
  #' return_probs: for classification, return class probabilities
  #' return_neighbors: return neighbor indices & distances
  if (is.data.frame(X_new)) X_new <- as.matrix(X_new)
  if (is.vector(X_new)) X_new <- matrix(X_new, nrow = 1)
  if (!is.matrix(X_new)) stop("X_new must be matrix/data.frame or vector.")
  if (ncol(X_new) != ncol(model$X)) stop("Feature dimensionality mismatch.")
  
  # normalize if needed
  if (model$normalize && !is.null(model$norm_fit)) {
    X_proc <- zscore_transform(X_new, model$norm_fit)
  } else {
    X_proc <- X_new
  }
  
  m <- nrow(X_proc)
  n <- nrow(model$X)
  k <- model$k
  
  if (n == 0) stop("Model has no training samples.")
  
  # distances: m x n
  dists <- squared_euclidean_distances(X_proc, model$X)
  
  # For each test row, find k smallest distances (ties handled by order)
  idx_mat <- t(apply(dists, 1, function(r) {
    order(r, decreasing = FALSE)[seq_len(k)]
  })) # m x k
  
  dist_mat <- matrix(NA_real_, nrow = m, ncol = k)
  for (i in seq_len(m)) dist_mat[i, ] <- dists[i, idx_mat[i, ]]
  
  # handle zero distances (exact matches) to avoid division by zero in weighting
  eps <- 1e-12
  if (model$is_classification) {
    preds <- vector("character", m)
    probs <- matrix(0, nrow = m, ncol = length(model$classes))
    colnames(probs) <- model$classes
    
    for (i in seq_len(m)) {
      neighbor_idx <- idx_mat[i, ]
      neighbor_labels <- as.character(model$y[neighbor_idx])
      neighbor_dists <- dist_mat[i, ]
      
      if (model$weighted) {
        # weights: 1 / (dist + eps)
        w <- 1 / (neighbor_dists + eps)
        # if any dist==0, set weight large for exact matches
        if (any(neighbor_dists == 0)) {
          w <- as.numeric(neighbor_dists == 0) * 1e12  # very large weight for exact matches
        }
        tab <- tapply(w, neighbor_labels, sum)
      } else {
        tab <- table(neighbor_labels)
      }
      # ensure all classes present
      counts <- rep(0, length(model$classes))
      names(counts) <- model$classes
      tab_names <- names(tab)
      counts[tab_names] <- as.numeric(tab)
      
      # normalize to probabilities
      if (sum(counts) > 0) probs[i, ] <- counts / sum(counts)
      else probs[i, ] <- counts
      
      # choose class with max probability (first tie wins because which.max)
      preds[i] <- colnames(probs)[which.max(probs[i, ])]
    }
    # cast to factor with original levels
    preds <- factor(preds, levels = model$classes)
    
    out <- list(predictions = preds)
    if (return_probs) out$probs <- probs
  } else {
    # regression
    preds_reg <- numeric(m)
    for (i in seq_len(m)) {
      neighbor_idx <- idx_mat[i, ]
      neighbor_vals <- as.numeric(model$y[neighbor_idx])
      neighbor_dists <- dist_mat[i, ]
      if (model$weighted) {
        w <- 1 / (neighbor_dists + eps)
        if (any(neighbor_dists == 0)) {
          w <- as.numeric(neighbor_dists == 0) * 1e12
        }
        preds_reg[i] <- sum(w * neighbor_vals) / sum(w)
      } else {
        preds_reg[i] <- mean(neighbor_vals)
      }
    }
    out <- list(predictions = preds_reg)
  }
  
  if (return_neighbors) {
    out$neighbor_indices <- idx_mat
    out$neighbor_distances <- dist_mat
  }
  
  out
}

# ---------------------------
# Utility: accuracy and confusion (classification)
# ---------------------------
knn_accuracy <- function(y_true, y_pred) {
  if (length(y_true) != length(y_pred)) stop("Lengths mismatch.")
  mean(y_true == y_pred)
}

confusion_matrix <- function(y_true, y_pred) {
  table(Actual = y_true, Predicted = y_pred)
}

# ---------------------------
# Example/Test: Iris classification
# ---------------------------
cat("=== k-NN Example: Iris dataset (classification) ===\n")
data(iris)
set.seed(42)
# Use only numeric features
X <- as.matrix(iris[, 1:4])
y <- factor(iris$Species)

# train/test split 70/30
if (interactive()) {
  n <- nrow(X)
  train_idx <- sample(seq_len(n), size = floor(0.7 * n))
  test_idx <- setdiff(seq_len(n), train_idx)

  X_train <- X[train_idx, , drop = FALSE]
  y_train <- y[train_idx]
  X_test <- X[test_idx, , drop = FALSE]
  y_test <- y[test_idx]

  model <- knn_train(X_train, y_train, k = 5, weighted = TRUE, normalize = TRUE)
  pred <- knn_predict(model, X_test, return_probs = TRUE, return_neighbors = FALSE)

  acc <- knn_accuracy(y_test, pred$predictions)
  cat(sprintf("Test accuracy (k=%d, weighted=%s, normalize=%s): %.4f\n",
              model$k, model$weighted, model$normalize, acc))
  cat("Confusion Matrix:\n")
  print(confusion_matrix(y_test, pred$predictions))
  cat("\n")

  # ---------------------------
  # Example/Test: Regression (toy)
  # ---------------------------
  cat("=== k-NN Example: Toy regression ===\n")
  set.seed(1)
  n_reg <- 200
  X_reg <- matrix(runif(n_reg * 2, -5, 5), ncol = 2)
  y_reg <- X_reg[,1] * 2 - X_reg[,2] * 0.5 + rnorm(n_reg, sd = 0.5)
  train_idx <- sample(seq_len(n_reg), size = 150)
  X_tr <- X_reg[train_idx, , drop=FALSE]; y_tr <- y_reg[train_idx]
  X_te <- X_reg[-train_idx, , drop=FALSE]; y_te <- y_reg[-train_idx]

  model_reg <- knn_train(X_tr, y_tr, k = 7, weighted = TRUE, normalize = TRUE)
  pred_reg <- knn_predict(model_reg, X_te)
  mse <- mean((pred_reg$predictions - y_te)^2)
  cat(sprintf("Regression MSE (k=%d, weighted=%s): %.4f\n\n", model_reg$k, model_reg$weighted, mse))
}
# ---------------------------
# End of script
# ---------------------------

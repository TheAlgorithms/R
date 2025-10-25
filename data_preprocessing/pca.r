# ==============================================
# Principal Component Analysis (PCA)
# ==============================================
# Algorithm: Dimensionality reduction using orthogonal linear transformation
# Framework: Base R
#
# Purpose:
# - Reduce high-dimensional data into a smaller set of uncorrelated variables (principal components)
# - Preserve as much variance as possible
#
# Steps:
# 1. Standardize the dataset (zero mean, unit variance)
# 2. Compute the covariance matrix of the standardized data
# 3. Compute eigenvalues and eigenvectors of the covariance matrix
# 4. Sort eigenvectors by decreasing eigenvalues (most variance first)
# 5. Project original data onto top k eigenvectors to get reduced data
#
# Complexity:
# - Time:  O(n * d^2 + d^3)  where n = samples, d = features
# - Space: O(d^2 + n * d)
#
# Applications:
# - Data visualization, noise reduction, feature extraction
# - Preprocessing for machine learning models
# ==============================================

# PCA Algorithm Implementation (Algorithm only)
pca_algorithm <- function(X, k) {
  # Basic input validation (kept minimal to match repo style)
  if (is.vector(X)) {
    X <- matrix(X, ncol = 1)
  }
  if (!is.matrix(X) || !is.numeric(X)) {
    stop("Input 'X' must be a numeric matrix or vector")
  }
  d <- ncol(X)
  if (k <= 0 || k > d) {
    stop("'k' must be between 1 and the number of columns of X")
  }

  # Step 1: Standardize the data (zero mean, unit variance per feature)
  X_std <- scale(X)

  # Step 2: Compute covariance matrix of standardized data
  cov_matrix <- cov(X_std)

  # Step 3: Eigen decomposition (covariance is symmetric)
  eig <- eigen(cov_matrix)
  eig_values <- eig$values
  eig_vectors <- eig$vectors

  # Step 4: Select top k principal components (eigenvectors)
  top_vectors <- eig_vectors[, 1:k, drop = FALSE]

  # Step 5: Project standardized data onto top k components
  X_reduced <- X_std %*% top_vectors

  return(list(
    reduced_data = X_reduced,
    components = top_vectors,
    eigenvalues = eig_values
  ))
}

# Example usage (algorithm only)
# set.seed(42)
# X <- matrix(rnorm(50 * 5), nrow = 50, ncol = 5)
# pca_result <- pca_algorithm(X, k = 2)
# head(pca_result$reduced_data)

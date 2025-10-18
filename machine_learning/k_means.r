# k_means.r
# K-Means Clustering Algorithm Implementation in R
# A centroid-based clustering algorithm that partitions data into k clusters
# Each cluster is represented by its centroid (mean of all points in the cluster)
#
# Algorithm details:
# - Iteratively assigns points to nearest centroid and updates centroids
# - Minimizes within-cluster sum of squared distances (inertia)
# - Fast and efficient for large datasets
# - Works best with spherical, well-separated clusters
# - Time complexity: O(n * k * d * iterations)
# - Space complexity: O(n * d + k * d)

library(R6)

#' K-Means Clustering Algorithm
#' 
#' Implementation of the K-Means clustering algorithm with multiple initialization methods
#' 
#' @description
#' K-Means is a centroid-based clustering algorithm that partitions n observations into k clusters.
#' Each observation belongs to the cluster with the nearest centroid (cluster center).
#' The algorithm iteratively updates cluster assignments and centroids until convergence.
#'
#' @examples
#' # Create sample data
#' set.seed(42)
#' X <- rbind(
#'   matrix(rnorm(100, mean = 0, sd = 1), ncol = 2),
#'   matrix(rnorm(100, mean = 5, sd = 1), ncol = 2),
#'   matrix(rnorm(100, mean = 10, sd = 1), ncol = 2)
#' )
#' 
#' # Fit k-means
#' kmeans_model <- KMeans$new(n_clusters = 3, init = "k-means++", max_iter = 300)
#' kmeans_model$fit(X)
#' labels <- kmeans_model$predict(X)
#' 
#' # Get cluster centers
#' centers <- kmeans_model$get_centroids()
#' 
#' # Plot results
#' plot(X, col = labels + 1, pch = 19, main = "K-Means Clustering")
#' points(centers, col = 1:3, pch = 8, cex = 2, lwd = 2)
#' legend("topright", legend = paste("Cluster", 1:3), col = 2:4, pch = 19)

KMeans <- R6Class(
  "KMeans",
  
  public = list(
    #' @field n_clusters Number of clusters to form
    n_clusters = NULL,
    
    #' @field init Initialization method: "random", "k-means++", or matrix of initial centroids
    init = NULL,
    
    #' @field max_iter Maximum number of iterations
    max_iter = NULL,
    
    #' @field tol Tolerance for convergence (change in inertia)
    tol = NULL,
    
    #' @field random_state Random seed for reproducibility
    random_state = NULL,
    
    #' @field n_init Number of times to run algorithm with different initializations
    n_init = NULL,
    
    #' @field centroids Cluster centroids
    centroids = NULL,
    
    #' @field labels Cluster labels for training data
    labels = NULL,
    
    #' @field inertia Within-cluster sum of squared distances
    inertia = NULL,
    
    #' @field n_iter Number of iterations performed
    n_iter = NULL,
    
    #' @field cluster_sizes Number of points in each cluster
    cluster_sizes = NULL,
    
    #' @field training_data Stored training data
    training_data = NULL,
    
    #' @description
    #' Initialize a new K-Means instance
    #' 
    #' @param n_clusters Number of clusters (default: 3)
    #' @param init Initialization method: "random", "k-means++" (default: "k-means++")
    #' @param max_iter Maximum iterations (default: 300)
    #' @param tol Tolerance for convergence (default: 1e-4)
    #' @param random_state Random seed (default: NULL)
    #' @param n_init Number of initializations to try (default: 10)
    initialize = function(n_clusters = 3, init = "k-means++", max_iter = 300, 
                         tol = 1e-4, random_state = NULL, n_init = 10) {
      self$n_clusters <- n_clusters
      self$init <- init
      self$max_iter <- max_iter
      self$tol <- tol
      self$random_state <- random_state
      self$n_init <- n_init
      
      if (!is.null(random_state)) {
        set.seed(random_state)
      }
    },
    
    #' @description
    #' Compute Euclidean distance between two points
    #' 
    #' @param x1 First point (vector)
    #' @param x2 Second point (vector)
    #' @return Euclidean distance
    euclidean_distance = function(x1, x2) {
      return(sqrt(sum((x1 - x2)^2)))
    },
    
    #' @description
    #' Compute distances from all points to all centroids
    #' 
    #' @param X Data matrix (n_samples x n_features)
    #' @param centroids Centroid matrix (n_clusters x n_features)
    #' @return Distance matrix (n_samples x n_clusters)
    compute_distances = function(X, centroids) {
      n_samples <- nrow(X)
      n_clusters <- nrow(centroids)
      distances <- matrix(0, nrow = n_samples, ncol = n_clusters)
      
      for (i in 1:n_samples) {
        for (k in 1:n_clusters) {
          distances[i, k] <- self$euclidean_distance(X[i, ], centroids[k, ])
        }
      }
      
      return(distances)
    },
    
    #' @description
    #' Initialize centroids randomly
    #' 
    #' @param X Data matrix
    #' @return Initial centroids
    initialize_random = function(X) {
      n_samples <- nrow(X)
      indices <- sample(1:n_samples, self$n_clusters, replace = FALSE)
      centroids <- X[indices, , drop = FALSE]
      return(centroids)
    },
    
    #' @description
    #' Initialize centroids using k-means++ algorithm
    #' Selects centroids that are far apart, leading to better convergence
    #' 
    #' @param X Data matrix
    #' @return Initial centroids
    initialize_kmeans_plusplus = function(X) {
      n_samples <- nrow(X)
      n_features <- ncol(X)
      centroids <- matrix(0, nrow = self$n_clusters, ncol = n_features)
      
      # 1. Choose first centroid randomly
      first_idx <- sample(1:n_samples, 1)
      centroids[1, ] <- X[first_idx, ]
      
      # 2. Choose remaining centroids
      for (k in 2:self$n_clusters) {
        # Compute distances from each point to nearest existing centroid
        min_distances <- rep(Inf, n_samples)
        
        for (i in 1:n_samples) {
          for (j in 1:(k-1)) {
            dist <- self$euclidean_distance(X[i, ], centroids[j, ])
            if (dist < min_distances[i]) {
              min_distances[i] <- dist
            }
          }
        }
        
        # Square the distances
        squared_distances <- min_distances^2
        
        # Choose next centroid with probability proportional to squared distance
        probabilities <- squared_distances / sum(squared_distances)
        next_idx <- sample(1:n_samples, 1, prob = probabilities)
        centroids[k, ] <- X[next_idx, ]
      }
      
      return(centroids)
    },
    
    #' @description
    #' Initialize centroids based on selected method
    #' 
    #' @param X Data matrix
    #' @return Initial centroids
    initialize_centroids = function(X) {
      if (is.matrix(self$init)) {
        # User-provided initial centroids
        if (nrow(self$init) != self$n_clusters || ncol(self$init) != ncol(X)) {
          stop("Initial centroids dimensions don't match")
        }
        return(self$init)
      } else if (self$init == "random") {
        return(self$initialize_random(X))
      } else if (self$init == "k-means++") {
        return(self$initialize_kmeans_plusplus(X))
      } else {
        stop("Unknown initialization method. Use 'random' or 'k-means++'")
      }
    },
    
    #' @description
    #' Assign points to nearest centroid
    #' 
    #' @param X Data matrix
    #' @param centroids Current centroids
    #' @return List with labels and inertia
    assign_clusters = function(X, centroids) {
      n_samples <- nrow(X)
      labels <- integer(n_samples)
      inertia <- 0
      
      # Compute distances to all centroids
      distances <- self$compute_distances(X, centroids)
      
      # Assign to nearest centroid
      for (i in 1:n_samples) {
        nearest_cluster <- which.min(distances[i, ])
        labels[i] <- nearest_cluster
        inertia <- inertia + distances[i, nearest_cluster]^2
      }
      
      return(list(labels = labels, inertia = inertia))
    },
    
    #' @description
    #' Update centroids as mean of assigned points
    #' 
    #' @param X Data matrix
    #' @param labels Current cluster labels
    #' @return Updated centroids
    update_centroids = function(X, labels) {
      n_features <- ncol(X)
      new_centroids <- matrix(0, nrow = self$n_clusters, ncol = n_features)
      
      for (k in 1:self$n_clusters) {
        cluster_points <- X[labels == k, , drop = FALSE]
        
        if (nrow(cluster_points) == 0) {
          # If cluster is empty, reinitialize randomly
          warning(sprintf("Cluster %d is empty, reinitializing randomly", k))
          random_idx <- sample(1:nrow(X), 1)
          new_centroids[k, ] <- X[random_idx, ]
        } else {
          # Compute mean of cluster points
          new_centroids[k, ] <- colMeans(cluster_points)
        }
      }
      
      return(new_centroids)
    },
    
    #' @description
    #' Perform one complete run of k-means
    #' 
    #' @param X Data matrix
    #' @return List with centroids, labels, inertia, and iterations
    single_run = function(X) {
      # Initialize centroids
      centroids <- self$initialize_centroids(X)
      prev_inertia <- Inf
      
      for (iter in 1:self$max_iter) {
        # Assignment step
        assignment <- self$assign_clusters(X, centroids)
        labels <- assignment$labels
        inertia <- assignment$inertia
        
        # Check convergence
        if (abs(prev_inertia - inertia) < self$tol) {
          return(list(
            centroids = centroids,
            labels = labels,
            inertia = inertia,
            n_iter = iter
          ))
        }
        
        # Update step
        centroids <- self$update_centroids(X, labels)
        prev_inertia <- inertia
      }
      
      # Max iterations reached
      return(list(
        centroids = centroids,
        labels = labels,
        inertia = inertia,
        n_iter = self$max_iter
      ))
    },
    
    #' @description
    #' Fit the k-means model to the data
    #' 
    #' @param X Data matrix (n_samples x n_features)
    #' @return Self (for method chaining)
    fit = function(X) {
      # Validate input
      if (!is.matrix(X)) {
        X <- as.matrix(X)
      }
      
      n_samples <- nrow(X)
      
      if (self$n_clusters > n_samples) {
        stop("Number of clusters cannot exceed number of samples")
      }
      
      # Store training data
      self$training_data <- X
      
      # Run k-means multiple times and keep best result
      best_inertia <- Inf
      best_result <- NULL
      
      cat(sprintf("Running K-Means with %d initialization(s)...\n", self$n_init))
      
      for (run in 1:self$n_init) {
        result <- self$single_run(X)
        
        if (result$inertia < best_inertia) {
          best_inertia <- result$inertia
          best_result <- result
        }
        
        if (self$n_init > 1) {
          cat(sprintf("  Run %d/%d: inertia = %.4f\n", run, self$n_init, result$inertia))
        }
      }
      
      # Store best result
      self$centroids <- best_result$centroids
      self$labels <- best_result$labels
      self$inertia <- best_result$inertia
      self$n_iter <- best_result$n_iter
      
      # Compute cluster sizes
      self$cluster_sizes <- integer(self$n_clusters)
      for (k in 1:self$n_clusters) {
        self$cluster_sizes[k] <- sum(self$labels == k)
      }
      
      cat(sprintf("\nBest result: inertia = %.4f, iterations = %d\n", 
                  self$inertia, self$n_iter))
      
      return(invisible(self))
    },
    
    #' @description
    #' Predict cluster labels for new data
    #' 
    #' @param X Data matrix to predict (n_samples x n_features)
    #' @return Vector of cluster labels
    predict = function(X) {
      if (is.null(self$centroids)) {
        stop("Model has not been fitted yet. Call fit() first.")
      }
      
      if (!is.matrix(X)) {
        X <- as.matrix(X)
      }
      
      n_samples <- nrow(X)
      labels <- integer(n_samples)
      
      # Compute distances and assign to nearest centroid
      distances <- self$compute_distances(X, self$centroids)
      
      for (i in 1:n_samples) {
        labels[i] <- which.min(distances[i, ])
      }
      
      return(labels)
    },
    
    #' @description
    #' Fit the model and return cluster labels
    #' 
    #' @param X Data matrix (n_samples x n_features)
    #' @return Vector of cluster labels
    fit_predict = function(X) {
      self$fit(X)
      return(self$labels)
    },
    
    #' @description
    #' Transform data to cluster-distance space
    #' 
    #' @param X Data matrix
    #' @return Matrix of distances to each cluster centroid
    transform = function(X) {
      if (is.null(self$centroids)) {
        stop("Model has not been fitted yet. Call fit() first.")
      }
      
      if (!is.matrix(X)) {
        X <- as.matrix(X)
      }
      
      return(self$compute_distances(X, self$centroids))
    },
    
    #' @description
    #' Fit and transform in one step
    #' 
    #' @param X Data matrix
    #' @return Matrix of distances to cluster centroids
    fit_transform = function(X) {
      self$fit(X)
      return(self$transform(X))
    },
    
    #' @description
    #' Get the cluster centroids
    #' 
    #' @return Matrix of centroids
    get_centroids = function() {
      if (is.null(self$centroids)) {
        stop("Model has not been fitted yet. Call fit() first.")
      }
      return(self$centroids)
    },
    
    #' @description
    #' Calculate silhouette score for clustering quality
    #' 
    #' @return Vector of silhouette scores for each point
    silhouette_score = function() {
      if (is.null(self$labels) || is.null(self$training_data)) {
        stop("Model has not been fitted yet. Call fit() first.")
      }
      
      n_samples <- length(self$labels)
      silhouette_scores <- numeric(n_samples)
      X <- self$training_data
      
      for (i in 1:n_samples) {
        cluster_i <- self$labels[i]
        
        # Calculate a(i): mean distance to points in same cluster
        same_cluster_indices <- which(self$labels == cluster_i)
        same_cluster_indices <- same_cluster_indices[same_cluster_indices != i]
        
        if (length(same_cluster_indices) == 0) {
          a_i <- 0
        } else {
          distances_same <- numeric(length(same_cluster_indices))
          for (j in seq_along(same_cluster_indices)) {
            distances_same[j] <- self$euclidean_distance(
              X[i, ], 
              X[same_cluster_indices[j], ]
            )
          }
          a_i <- mean(distances_same)
        }
        
        # Calculate b(i): mean distance to points in nearest cluster
        b_i <- Inf
        for (k in 1:self$n_clusters) {
          if (k == cluster_i) next
          
          other_cluster_indices <- which(self$labels == k)
          if (length(other_cluster_indices) > 0) {
            distances_other <- numeric(length(other_cluster_indices))
            for (j in seq_along(other_cluster_indices)) {
              distances_other[j] <- self$euclidean_distance(
                X[i, ], 
                X[other_cluster_indices[j], ]
              )
            }
            mean_dist <- mean(distances_other)
            if (mean_dist < b_i) {
              b_i <- mean_dist
            }
          }
        }
        
        # Calculate silhouette score
        if (a_i == 0 && b_i == 0) {
          silhouette_scores[i] <- 0
        } else {
          silhouette_scores[i] <- (b_i - a_i) / max(a_i, b_i)
        }
      }
      
      return(silhouette_scores)
    },
    
    #' @description
    #' Calculate Davies-Bouldin Index for clustering quality
    #' Lower values indicate better clustering
    #' 
    #' @return Davies-Bouldin Index score
    davies_bouldin_score = function() {
      if (is.null(self$labels) || is.null(self$centroids)) {
        stop("Model has not been fitted yet. Call fit() first.")
      }
      
      X <- self$training_data
      
      # Calculate average distance within each cluster
      avg_distances <- numeric(self$n_clusters)
      for (k in 1:self$n_clusters) {
        cluster_points <- X[self$labels == k, , drop = FALSE]
        if (nrow(cluster_points) > 0) {
          distances <- numeric(nrow(cluster_points))
          for (i in 1:nrow(cluster_points)) {
            distances[i] <- self$euclidean_distance(
              cluster_points[i, ], 
              self$centroids[k, ]
            )
          }
          avg_distances[k] <- mean(distances)
        }
      }
      
      # Calculate Davies-Bouldin Index
      db_index <- 0
      for (i in 1:self$n_clusters) {
        max_ratio <- 0
        for (j in 1:self$n_clusters) {
          if (i != j) {
            centroid_distance <- self$euclidean_distance(
              self$centroids[i, ], 
              self$centroids[j, ]
            )
            ratio <- (avg_distances[i] + avg_distances[j]) / centroid_distance
            if (ratio > max_ratio) {
              max_ratio <- ratio
            }
          }
        }
        db_index <- db_index + max_ratio
      }
      
      db_index <- db_index / self$n_clusters
      return(db_index)
    },
    
    #' @description
    #' Calculate Calinski-Harabasz Index (Variance Ratio Criterion)
    #' Higher values indicate better clustering
    #' 
    #' @return Calinski-Harabasz Index score
    calinski_harabasz_score = function() {
      if (is.null(self$labels) || is.null(self$centroids)) {
        stop("Model has not been fitted yet. Call fit() first.")
      }
      
      X <- self$training_data
      n_samples <- nrow(X)
      n_features <- ncol(X)
      
      # Overall centroid
      overall_centroid <- colMeans(X)
      
      # Between-cluster dispersion
      between_dispersion <- 0
      for (k in 1:self$n_clusters) {
        n_k <- self$cluster_sizes[k]
        if (n_k > 0) {
          dist_sq <- sum((self$centroids[k, ] - overall_centroid)^2)
          between_dispersion <- between_dispersion + n_k * dist_sq
        }
      }
      
      # Within-cluster dispersion
      within_dispersion <- self$inertia
      
      # Calinski-Harabasz Index
      if (within_dispersion == 0) {
        return(Inf)
      }
      
      ch_score <- (between_dispersion / (self$n_clusters - 1)) / 
                  (within_dispersion / (n_samples - self$n_clusters))
      
      return(ch_score)
    },
    
    #' @description
    #' Print summary of the model
    print = function() {
      cat("K-Means Clustering Model\n")
      cat("========================\n")
      cat(sprintf("Number of clusters: %d\n", self$n_clusters))
      cat(sprintf("Initialization method: %s\n", 
                  ifelse(is.matrix(self$init), "custom", self$init)))
      cat(sprintf("Maximum iterations: %d\n", self$max_iter))
      cat(sprintf("Tolerance: %.2e\n", self$tol))
      cat(sprintf("Number of initializations: %d\n", self$n_init))
      
      if (!is.null(self$n_iter)) {
        cat(sprintf("\nIterations performed: %d\n", self$n_iter))
        cat(sprintf("Final inertia: %.4f\n", self$inertia))
        
        if (!is.null(self$cluster_sizes)) {
          cat("\nCluster sizes:\n")
          for (k in 1:self$n_clusters) {
            cat(sprintf("  Cluster %d: %d points (%.1f%%)\n", 
                        k, 
                        self$cluster_sizes[k],
                        100 * self$cluster_sizes[k] / length(self$labels)))
          }
        }
        
        if (!is.null(self$centroids)) {
          cat("\nCentroid coordinates:\n")
          print(self$centroids)
        }
      } else {
        cat("\nModel not fitted yet.\n")
      }
      
      invisible(self)
    }
  )
)

#' Helper function to create and fit k-means model
#' 
#' @param X Data matrix (n_samples x n_features)
#' @param n_clusters Number of clusters
#' @param init Initialization method
#' @param max_iter Maximum iterations
#' @param tol Convergence tolerance
#' @param random_state Random seed
#' @param n_init Number of initializations
#' @return Fitted KMeans object
#' @export
k_means <- function(X, n_clusters = 3, init = "k-means++", max_iter = 300, 
                    tol = 1e-4, random_state = NULL, n_init = 10) {
  model <- KMeans$new(
    n_clusters = n_clusters,
    init = init,
    max_iter = max_iter,
    tol = tol,
    random_state = random_state,
    n_init = n_init
  )
  model$fit(X)
  return(model)
}

# Example usage and demonstrations
if (FALSE) {
  # Example 1: Basic clustering with 2D data
  cat("\n=== Example 1: Basic 2D Clustering ===\n")
  set.seed(42)
  
  # Create 3 clusters of data
  cluster1 <- matrix(rnorm(60, mean = 0, sd = 1), ncol = 2)
  cluster2 <- matrix(rnorm(60, mean = 5, sd = 1), ncol = 2)
  cluster3 <- matrix(rnorm(60, mean = c(2, 8), sd = 1), ncol = 2)
  X <- rbind(cluster1, cluster2, cluster3)
  
  # Fit k-means with k-means++ initialization
  kmeans_model <- KMeans$new(n_clusters = 3, init = "k-means++", 
                             max_iter = 300, n_init = 10, random_state = 42)
  kmeans_model$fit(X)
  
  # Get predictions
  labels <- kmeans_model$predict(X)
  
  # Print model summary
  kmeans_model$print()
  
  # Get centroids
  centroids <- kmeans_model$get_centroids()
  
  # Calculate quality metrics
  sil_scores <- kmeans_model$silhouette_score()
  cat(sprintf("\nMean silhouette score: %.4f\n", mean(sil_scores)))
  
  db_score <- kmeans_model$davies_bouldin_score()
  cat(sprintf("Davies-Bouldin Index: %.4f (lower is better)\n", db_score))
  
  ch_score <- kmeans_model$calinski_harabasz_score()
  cat(sprintf("Calinski-Harabasz Index: %.4f (higher is better)\n", ch_score))
  
  # Plot results
  plot(X, col = labels + 1, pch = 19, 
       main = "K-Means Clustering (k-means++)",
       xlab = "Feature 1", ylab = "Feature 2")
  points(centroids, col = 1:3, pch = 8, cex = 3, lwd = 3)
  legend("topright", legend = c(paste("Cluster", 1:3), "Centroids"), 
         col = c(2:4, 1), pch = c(19, 19, 19, 8), pt.cex = c(1, 1, 1, 2))
  
  # Example 2: Random initialization comparison
  cat("\n=== Example 2: Random Initialization ===\n")
  kmeans_random <- KMeans$new(n_clusters = 3, init = "random", 
                              max_iter = 300, n_init = 10, random_state = 42)
  kmeans_random$fit(X)
  
  cat(sprintf("\nK-means++ inertia: %.4f\n", kmeans_model$inertia))
  cat(sprintf("Random init inertia: %.4f\n", kmeans_random$inertia))
  cat(sprintf("Improvement: %.2f%%\n", 
              100 * (kmeans_random$inertia - kmeans_model$inertia) / kmeans_random$inertia))
  
  # Example 3: Iris dataset
  cat("\n=== Example 3: Iris Dataset ===\n")
  data(iris)
  X_iris <- as.matrix(iris[, 1:4])
  
  kmeans_iris <- KMeans$new(n_clusters = 3, init = "k-means++", 
                            max_iter = 300, n_init = 10, random_state = 42)
  kmeans_iris$fit(X_iris)
  labels_iris <- kmeans_iris$predict(X_iris)
  
  kmeans_iris$print()
  
  # Calculate quality metrics
  sil_scores_iris <- kmeans_iris$silhouette_score()
  cat(sprintf("\nMean silhouette score: %.4f\n", mean(sil_scores_iris)))
  
  # Visualize on first two dimensions
  plot(X_iris[, 1:2], col = labels_iris + 1, pch = 19,
       main = "K-Means on Iris Dataset",
       xlab = "Sepal Length", ylab = "Sepal Width")
  centroids_iris <- kmeans_iris$get_centroids()
  points(centroids_iris[, 1:2], col = 1:3, pch = 8, cex = 3, lwd = 3)
  
  # Compare with true species
  true_labels <- as.numeric(iris$Species)
  confusion_matrix <- table(Predicted = labels_iris, Actual = true_labels)
  cat("\nConfusion Matrix:\n")
  print(confusion_matrix)
  
  # Example 4: Elbow method for optimal k
  cat("\n=== Example 4: Elbow Method ===\n")
  inertias <- numeric(10)
  silhouette_scores <- numeric(10)
  
  for (k in 1:10) {
    if (k == 1) {
      # Silhouette not defined for k=1
      model <- KMeans$new(n_clusters = k, max_iter = 100, 
                         n_init = 3, random_state = 42)
      model$fit(X)
      inertias[k] <- model$inertia
      silhouette_scores[k] <- NA
    } else {
      model <- KMeans$new(n_clusters = k, max_iter = 100, 
                         n_init = 3, random_state = 42)
      model$fit(X)
      inertias[k] <- model$inertia
      silhouette_scores[k] <- mean(model$silhouette_score())
    }
  }
  
  # Plot elbow curve
  par(mfrow = c(1, 2))
  
  plot(1:10, inertias, type = "b", pch = 19, col = "blue",
       main = "Elbow Method",
       xlab = "Number of Clusters (k)", ylab = "Inertia")
  grid()
  
  plot(2:10, silhouette_scores[2:10], type = "b", pch = 19, col = "red",
       main = "Silhouette Score",
       xlab = "Number of Clusters (k)", ylab = "Mean Silhouette Score")
  grid()
  
  par(mfrow = c(1, 1))
  
  # Example 5: Transform to cluster-distance space
  cat("\n=== Example 5: Transform to Cluster-Distance Space ===\n")
  distances <- kmeans_model$transform(X[1:5, ])
  cat("\nDistances from first 5 points to each centroid:\n")
  print(distances)
  
  # Example 6: Custom initial centroids
  cat("\n=== Example 6: Custom Initial Centroids ===\n")
  custom_centroids <- matrix(c(0, 0, 5, 5, 2, 8), nrow = 3, byrow = TRUE)
  kmeans_custom <- KMeans$new(n_clusters = 3, init = custom_centroids, 
                              max_iter = 300, n_init = 1)
  kmeans_custom$fit(X)
  kmeans_custom$print()
  
  # Example 7: Quality metrics comparison
  cat("\n=== Example 7: Clustering Quality Metrics ===\n")
  
  metrics_df <- data.frame(
    k = 2:6,
    inertia = numeric(5),
    silhouette = numeric(5),
    davies_bouldin = numeric(5),
    calinski_harabasz = numeric(5)
  )
  
  for (i in 1:5) {
    k <- i + 1
    model <- KMeans$new(n_clusters = k, max_iter = 100, 
                       n_init = 5, random_state = 42)
    model$fit(X)
    
    metrics_df$inertia[i] <- model$inertia
    metrics_df$silhouette[i] <- mean(model$silhouette_score())
    metrics_df$davies_bouldin[i] <- model$davies_bouldin_score()
    metrics_df$calinski_harabasz[i] <- model$calinski_harabasz_score()
  }
  
  cat("\nClustering Quality Metrics:\n")
  print(metrics_df)
  
  cat("\nOptimal k based on:\n")
  cat(sprintf("  Silhouette Score: k = %d (%.4f)\n", 
              metrics_df$k[which.max(metrics_df$silhouette)],
              max(metrics_df$silhouette)))
  cat(sprintf("  Davies-Bouldin Index: k = %d (%.4f)\n", 
              metrics_df$k[which.min(metrics_df$davies_bouldin)],
              min(metrics_df$davies_bouldin)))
  cat(sprintf("  Calinski-Harabasz Index: k = %d (%.4f)\n", 
              metrics_df$k[which.max(metrics_df$calinski_harabasz)],
              max(metrics_df$calinski_harabasz)))
}

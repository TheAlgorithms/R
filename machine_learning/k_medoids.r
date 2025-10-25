# k_medoids.r
# K-Medoids (PAM - Partitioning Around Medoids) Algorithm Implementation in R
# A clustering algorithm similar to k-means but uses actual data points as cluster centers
# More robust to outliers and noise compared to k-means
#
# Algorithm details:
# - Uses actual data points (medoids) as cluster centers instead of centroids
# - Minimizes sum of dissimilarities between points and their medoids
# - More robust to outliers than k-means
# - Can work with any distance metric
# - Time complexity: O(k * (n-k)^2 * iterations)
# - Space complexity: O(n^2) for distance matrix

library(R6)

#' K-Medoids Clustering Algorithm
#' 
#' Implementation of the Partitioning Around Medoids (PAM) algorithm
#' 
#' @description
#' K-Medoids is a clustering algorithm that partitions n observations into k clusters.
#' Unlike k-means which uses centroids, k-medoids uses actual data points as cluster centers.
#' This makes it more robust to outliers and allows it to work with arbitrary distance metrics.
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
#' # Fit k-medoids
#' kmedoids <- KMedoids$new(n_clusters = 3, max_iter = 100)
#' kmedoids$fit(X)
#' labels <- kmedoids$predict(X)
#' 
#' # Get cluster centers (medoids)
#' centers <- kmedoids$get_medoids()
#' 
#' # Plot results
#' plot(X, col = labels + 1, pch = 19, main = "K-Medoids Clustering")
#' points(centers, col = 1:3, pch = 8, cex = 2, lwd = 2)
#' legend("topright", legend = paste("Cluster", 1:3), col = 2:4, pch = 19)

KMedoids <- R6Class(
  "KMedoids",
  
  public = list(
    #' @field n_clusters Number of clusters to form
    n_clusters = NULL,
    
    #' @field max_iter Maximum number of iterations
    max_iter = NULL,
    
    #' @field random_state Random seed for reproducibility
    random_state = NULL,
    
    #' @field metric Distance metric to use
    metric = NULL,
    
    #' @field medoid_indices Indices of the medoid points
    medoid_indices = NULL,
    
    #' @field medoids The actual medoid points
    medoids = NULL,
    
    #' @field labels Cluster labels for training data
    labels = NULL,
    
    #' @field inertia Sum of distances from points to their medoids
    inertia = NULL,
    
    #' @field n_iter Number of iterations performed
    n_iter = NULL,
    
    #' @field distance_matrix Pairwise distance matrix
    distance_matrix = NULL,
    
    #' @field training_data Stored training data
    training_data = NULL,
    
    #' @description
    #' Initialize a new K-Medoids instance
    #' 
    #' @param n_clusters Number of clusters (default: 3)
    #' @param max_iter Maximum iterations (default: 300)
    #' @param random_state Random seed (default: NULL)
    #' @param metric Distance metric: "euclidean" or "manhattan" (default: "euclidean")
    initialize = function(n_clusters = 3, max_iter = 300, 
                         random_state = NULL, metric = "euclidean") {
      self$n_clusters <- n_clusters
      self$max_iter <- max_iter
      self$random_state <- random_state
      self$metric <- metric
      
      if (!is.null(random_state)) {
        set.seed(random_state)
      }
    },
    
    #' @description
    #' Compute distance matrix between all points
    #' 
    #' @param X Data matrix (n_samples x n_features)
    #' @return Distance matrix (n_samples x n_samples)
    compute_distance_matrix = function(X) {
      n <- nrow(X)
      dist_matrix <- matrix(0, nrow = n, ncol = n)
      
      if (self$metric == "euclidean") {
        for (i in 1:n) {
          for (j in i:n) {
            if (i == j) {
              dist_matrix[i, j] <- 0
            } else {
              dist <- sqrt(sum((X[i, ] - X[j, ])^2))
              dist_matrix[i, j] <- dist
              dist_matrix[j, i] <- dist
            }
          }
        }
      } else if (self$metric == "manhattan") {
        for (i in 1:n) {
          for (j in i:n) {
            if (i == j) {
              dist_matrix[i, j] <- 0
            } else {
              dist <- sum(abs(X[i, ] - X[j, ]))
              dist_matrix[i, j] <- dist
              dist_matrix[j, i] <- dist
            }
          }
        }
      } else {
        stop("Unsupported metric. Use 'euclidean' or 'manhattan'")
      }
      
      return(dist_matrix)
    },
    
    #' @description
    #' Initialize medoids randomly
    #' 
    #' @param n_samples Number of samples in dataset
    #' @return Vector of initial medoid indices
    initialize_medoids = function(n_samples) {
      # Randomly select k points as initial medoids
      medoid_indices <- sample(1:n_samples, self$n_clusters, replace = FALSE)
      return(medoid_indices)
    },
    
    #' @description
    #' Assign each point to nearest medoid
    #' 
    #' @param dist_matrix Distance matrix
    #' @param medoid_indices Current medoid indices
    #' @return List with cluster labels and total cost
    assign_clusters = function(dist_matrix, medoid_indices) {
      n_samples <- nrow(dist_matrix)
      labels <- integer(n_samples)
      total_cost <- 0
      
      for (i in 1:n_samples) {
        # Find nearest medoid
        distances_to_medoids <- dist_matrix[i, medoid_indices]
        nearest_medoid_idx <- which.min(distances_to_medoids)
        labels[i] <- nearest_medoid_idx
        total_cost <- total_cost + distances_to_medoids[nearest_medoid_idx]
      }
      
      return(list(labels = labels, cost = total_cost))
    },
    
    #' @description
    #' Update medoids by finding best representative in each cluster
    #' 
    #' @param dist_matrix Distance matrix
    #' @param labels Current cluster labels
    #' @param old_medoid_indices Current medoid indices
    #' @return List with new medoid indices and whether they changed
    update_medoids = function(dist_matrix, labels, old_medoid_indices) {
      new_medoid_indices <- integer(self$n_clusters)
      changed <- FALSE
      
      for (k in 1:self$n_clusters) {
        # Get all points in cluster k
        cluster_points <- which(labels == k)
        
        if (length(cluster_points) == 0) {
          # If cluster is empty, keep old medoid
          new_medoid_indices[k] <- old_medoid_indices[k]
          next
        }
        
        if (length(cluster_points) == 1) {
          # If only one point, it's the medoid
          new_medoid_indices[k] <- cluster_points[1]
        } else {
          # Find point that minimizes sum of distances to all other points in cluster
          min_cost <- Inf
          best_medoid <- cluster_points[1]
          
          for (candidate in cluster_points) {
            cost <- sum(dist_matrix[candidate, cluster_points])
            if (cost < min_cost) {
              min_cost <- cost
              best_medoid <- candidate
            }
          }
          
          new_medoid_indices[k] <- best_medoid
        }
        
        # Check if medoid changed
        if (new_medoid_indices[k] != old_medoid_indices[k]) {
          changed <- TRUE
        }
      }
      
      return(list(medoid_indices = new_medoid_indices, changed = changed))
    },
    
    #' @description
    #' Fit the k-medoids model to the data
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
      
      # Compute distance matrix
      cat("Computing distance matrix...\n")
      self$distance_matrix <- self$compute_distance_matrix(X)
      
      # Initialize medoids
      cat("Initializing medoids...\n")
      self$medoid_indices <- self$initialize_medoids(n_samples)
      
      # Iterative optimization
      cat("Starting optimization...\n")
      for (iter in 1:self$max_iter) {
        # Assign points to clusters
        assignment <- self$assign_clusters(self$distance_matrix, self$medoid_indices)
        self$labels <- assignment$labels
        self$inertia <- assignment$cost
        
        # Update medoids
        update_result <- self$update_medoids(
          self$distance_matrix, 
          self$labels, 
          self$medoid_indices
        )
        
        # Check convergence
        if (!update_result$changed) {
          self$n_iter <- iter
          cat(sprintf("Converged after %d iterations\n", iter))
          break
        }
        
        self$medoid_indices <- update_result$medoid_indices
        
        if (iter == self$max_iter) {
          self$n_iter <- iter
          cat(sprintf("Reached maximum iterations (%d)\n", iter))
        }
        
        # Print progress every 10 iterations
        if (iter %% 10 == 0) {
          cat(sprintf("Iteration %d: cost = %.4f\n", iter, self$inertia))
        }
      }
      
      # Store final medoids
      self$medoids <- X[self$medoid_indices, , drop = FALSE]
      
      cat(sprintf("Final cost: %.4f\n", self$inertia))
      
      return(invisible(self))
    },
    
    #' @description
    #' Predict cluster labels for new data
    #' 
    #' @param X Data matrix to predict (n_samples x n_features)
    #' @return Vector of cluster labels
    predict = function(X) {
      if (is.null(self$medoids)) {
        stop("Model has not been fitted yet. Call fit() first.")
      }
      
      if (!is.matrix(X)) {
        X <- as.matrix(X)
      }
      
      n_samples <- nrow(X)
      labels <- integer(n_samples)
      
      # For each point, find nearest medoid
      for (i in 1:n_samples) {
        min_dist <- Inf
        best_cluster <- 1
        
        for (k in 1:self$n_clusters) {
          if (self$metric == "euclidean") {
            dist <- sqrt(sum((X[i, ] - self$medoids[k, ])^2))
          } else if (self$metric == "manhattan") {
            dist <- sum(abs(X[i, ] - self$medoids[k, ]))
          }
          
          if (dist < min_dist) {
            min_dist <- dist
            best_cluster <- k
          }
        }
        
        labels[i] <- best_cluster
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
    #' Get the medoid points
    #' 
    #' @return Matrix of medoid points
    get_medoids = function() {
      if (is.null(self$medoids)) {
        stop("Model has not been fitted yet. Call fit() first.")
      }
      return(self$medoids)
    },
    
    #' @description
    #' Get the indices of medoid points in training data
    #' 
    #' @return Vector of medoid indices
    get_medoid_indices = function() {
      if (is.null(self$medoid_indices)) {
        stop("Model has not been fitted yet. Call fit() first.")
      }
      return(self$medoid_indices)
    },
    
    #' @description
    #' Calculate silhouette score for clustering quality
    #' 
    #' @return Vector of silhouette scores for each point
    silhouette_score = function() {
      if (is.null(self$labels) || is.null(self$distance_matrix)) {
        stop("Model has not been fitted yet. Call fit() first.")
      }
      
      n_samples <- length(self$labels)
      silhouette_scores <- numeric(n_samples)
      
      for (i in 1:n_samples) {
        cluster_i <- self$labels[i]
        
        # Calculate a(i): mean distance to points in same cluster
        same_cluster <- which(self$labels == cluster_i)
        same_cluster <- same_cluster[same_cluster != i]
        
        if (length(same_cluster) == 0) {
          a_i <- 0
        } else {
          a_i <- mean(self$distance_matrix[i, same_cluster])
        }
        
        # Calculate b(i): mean distance to points in nearest cluster
        b_i <- Inf
        for (k in 1:self$n_clusters) {
          if (k == cluster_i) next
          
          other_cluster <- which(self$labels == k)
          if (length(other_cluster) > 0) {
            mean_dist <- mean(self$distance_matrix[i, other_cluster])
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
    #' Print summary of the model
    print = function() {
      cat("K-Medoids Clustering Model\n")
      cat("==========================\n")
      cat(sprintf("Number of clusters: %d\n", self$n_clusters))
      cat(sprintf("Distance metric: %s\n", self$metric))
      cat(sprintf("Maximum iterations: %d\n", self$max_iter))
      
      if (!is.null(self$n_iter)) {
        cat(sprintf("Iterations performed: %d\n", self$n_iter))
        cat(sprintf("Final inertia: %.4f\n", self$inertia))
        
        if (!is.null(self$labels)) {
          cat("\nCluster sizes:\n")
          for (k in 1:self$n_clusters) {
            size <- sum(self$labels == k)
            cat(sprintf("  Cluster %d: %d points\n", k, size))
          }
        }
      } else {
        cat("Model not fitted yet.\n")
      }
      
      invisible(self)
    }
  )
)

#' Helper function to create and fit k-medoids model
#' 
#' @param X Data matrix (n_samples x n_features)
#' @param n_clusters Number of clusters
#' @param max_iter Maximum iterations
#' @param random_state Random seed
#' @param metric Distance metric
#' @return Fitted KMedoids object
#' @export
k_medoids <- function(X, n_clusters = 3, max_iter = 300, 
                      random_state = NULL, metric = "euclidean") {
  model <- KMedoids$new(
    n_clusters = n_clusters,
    max_iter = max_iter,
    random_state = random_state,
    metric = metric
  )
  model$fit(X)
  return(model)
}

# Example usage and demonstration
if (FALSE) {
  # Example 1: Basic clustering with 2D data
  cat("\n=== Example 1: Basic 2D Clustering ===\n")
  set.seed(42)
  
  # Create 3 clusters of data
  cluster1 <- matrix(rnorm(60, mean = 0, sd = 1), ncol = 2)
  cluster2 <- matrix(rnorm(60, mean = 5, sd = 1), ncol = 2)
  cluster3 <- matrix(rnorm(60, mean = c(2, 8), sd = 1), ncol = 2)
  X <- rbind(cluster1, cluster2, cluster3)
  
  # Fit k-medoids
  kmedoids <- KMedoids$new(n_clusters = 3, max_iter = 100, random_state = 42)
  kmedoids$fit(X)
  
  # Get predictions
  labels <- kmedoids$predict(X)
  
  # Print model summary
  kmedoids$print()
  
  # Get medoids
  medoids <- kmedoids$get_medoids()
  cat("\nMedoid coordinates:\n")
  print(medoids)
  
  # Calculate silhouette scores
  sil_scores <- kmedoids$silhouette_score()
  cat(sprintf("\nMean silhouette score: %.4f\n", mean(sil_scores)))
  
  # Plot results
  plot(X, col = labels + 1, pch = 19, 
       main = "K-Medoids Clustering (Euclidean)",
       xlab = "Feature 1", ylab = "Feature 2")
  points(medoids, col = 1:3, pch = 8, cex = 2, lwd = 2)
  legend("topright", legend = c(paste("Cluster", 1:3), "Medoids"), 
         col = c(2:4, 1), pch = c(19, 19, 19, 8))
  
  # Example 2: Manhattan distance metric
  cat("\n=== Example 2: Manhattan Distance ===\n")
  kmedoids_manhattan <- KMedoids$new(
    n_clusters = 3, 
    max_iter = 100, 
    random_state = 42,
    metric = "manhattan"
  )
  kmedoids_manhattan$fit(X)
  labels_manhattan <- kmedoids_manhattan$predict(X)
  
  kmedoids_manhattan$print()
  
  # Example 3: Iris dataset
  cat("\n=== Example 3: Iris Dataset ===\n")
  data(iris)
  X_iris <- as.matrix(iris[, 1:4])
  
  kmedoids_iris <- KMedoids$new(n_clusters = 3, max_iter = 100, random_state = 42)
  kmedoids_iris$fit(X_iris)
  labels_iris <- kmedoids_iris$predict(X_iris)
  
  # Compare with true labels
  true_labels <- as.numeric(iris$Species)
  accuracy <- sum(labels_iris == true_labels) / length(true_labels)
  cat(sprintf("\nClustering accuracy: %.2f%%\n", accuracy * 100))
  
  # Visualize on first two dimensions
  plot(X_iris[, 1:2], col = labels_iris + 1, pch = 19,
       main = "K-Medoids on Iris Dataset",
       xlab = "Sepal Length", ylab = "Sepal Width")
  medoids_iris <- kmedoids_iris$get_medoids()
  points(medoids_iris[, 1:2], col = 1:3, pch = 8, cex = 2, lwd = 2)
  
  # Example 4: Finding optimal number of clusters
  cat("\n=== Example 4: Elbow Method ===\n")
  inertias <- numeric(10)
  
  for (k in 1:10) {
    model <- KMedoids$new(n_clusters = k, max_iter = 50, random_state = 42)
    model$fit(X)
    inertias[k] <- model$inertia
  }
  
  plot(1:10, inertias, type = "b", pch = 19,
       main = "Elbow Method for Optimal K",
       xlab = "Number of Clusters", ylab = "Inertia")
  grid()
}

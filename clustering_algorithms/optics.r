# OPTICS (Ordering Points To Identify the Clustering Structure)
#
# OPTICS is a density-based clustering algorithm that extends DBSCAN by creating
# an augmented ordering of the database representing its density-based clustering
# structure. Unlike DBSCAN, it produces a reachability plot that can be used to
# extract clusters at different density thresholds.
#
# Time Complexity: O(n^2) for distance matrix, O(n log n) with spatial indexing
# Space Complexity: O(n^2) for distance matrix
#
# Input: data matrix/dataframe, eps (maximum radius), minPts (minimum neighbors)
# Output: ordered points with reachability distances and core distances

optics <- function(data, eps = 0.5, minPts = 5) {
  # Convert to matrix
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }
  
  n <- nrow(data)
  
  # Calculate pairwise distances
  dist_matrix <- as.matrix(dist(data, method = "euclidean"))
  
  # Initialize result vectors
  reachability <- rep(Inf, n)
  core_dist <- rep(Inf, n)
  processed <- rep(FALSE, n)
  ordered_points <- integer(0)
  cluster_id <- rep(0, n)
  
  # Calculate core distance for each point
  for (i in 1:n) {
    neighbors <- which(dist_matrix[i, ] <= eps)
    neighbors <- neighbors[neighbors != i]
    
    if (length(neighbors) >= minPts) {
      neighbor_dists <- sort(dist_matrix[i, neighbors])
      core_dist[i] <- neighbor_dists[minPts]
    }
  }
  
  # Priority queue implementation using simple sorting
  update_seeds <- function(seeds, neighbors, center_idx) {
    c_dist <- core_dist[center_idx]
    
    for (neighbor in neighbors) {
      if (processed[neighbor]) next
      
      new_reach_dist <- max(c_dist, dist_matrix[center_idx, neighbor])
      
      if (reachability[neighbor] == Inf) {
        reachability[neighbor] <<- new_reach_dist
        seeds <- c(seeds, neighbor)
      } else if (new_reach_dist < reachability[neighbor]) {
        reachability[neighbor] <<- new_reach_dist
      }
    }
    
    # Sort seeds by reachability distance
    if (length(seeds) > 0) {
      seeds <- seeds[order(reachability[seeds])]
    }
    
    return(seeds)
  }
  
  # Main OPTICS algorithm
  for (i in 1:n) {
    if (processed[i]) next
    
    # Get neighbors within eps
    neighbors <- which(dist_matrix[i, ] <= eps)
    neighbors <- neighbors[neighbors != i]
    
    processed[i] <- TRUE
    ordered_points <- c(ordered_points, i)
    
    if (core_dist[i] != Inf) {
      seeds <- integer(0)
      seeds <- update_seeds(seeds, neighbors, i)
      
      while (length(seeds) > 0) {
        current <- seeds[1]
        seeds <- seeds[-1]
        
        if (processed[current]) next
        
        neighbors_current <- which(dist_matrix[current, ] <= eps)
        neighbors_current <- neighbors_current[neighbors_current != current]
        
        processed[current] <- TRUE
        ordered_points <- c(ordered_points, current)
        
        if (core_dist[current] != Inf) {
          seeds <- update_seeds(seeds, neighbors_current, current)
        }
      }
    }
  }
  
  return(list(
    order = ordered_points,
    reachability = reachability[ordered_points],
    core_distance = core_dist[ordered_points],
    eps = eps,
    minPts = minPts
  ))
}

# Extract DBSCAN-like clusters from OPTICS result
extract_dbscan_clusters <- function(optics_result, eps_cluster) {
  n <- length(optics_result$order)
  cluster_id <- rep(0, n)
  current_cluster <- 0
  
  for (i in 1:n) {
    if (optics_result$reachability[i] > eps_cluster) {
      if (optics_result$core_distance[i] <= eps_cluster) {
        current_cluster <- current_cluster + 1
        cluster_id[i] <- current_cluster
      } else {
        cluster_id[i] <- 0
      }
    } else {
      if (current_cluster == 0) {
        current_cluster <- current_cluster + 1
      }
      cluster_id[i] <- current_cluster
    }
  }
  
  # Reorder to original indices
  result <- rep(0, n)
  result[optics_result$order] <- cluster_id
  
  return(result)
}

# Extract clusters using xi method (steep areas in reachability plot)
extract_xi_clusters <- function(optics_result, xi = 0.05, min_cluster_size = 5) {
  n <- length(optics_result$order)
  reach <- optics_result$reachability
  
  # Replace Inf with max finite value
  max_reach <- max(reach[is.finite(reach)])
  reach[is.infinite(reach)] <- max_reach * 2
  
  # Find steep up and down areas
  steep_up <- c()
  steep_down <- c()
  
  for (i in 2:n) {
    if (reach[i] > reach[i-1] * (1 + xi)) {
      steep_up <- c(steep_up, i)
    }
    if (reach[i] < reach[i-1] * (1 - xi)) {
      steep_down <- c(steep_down, i)
    }
  }
  
  # Match steep areas to form clusters
  cluster_id <- rep(0, n)
  current_cluster <- 0
  
  if (length(steep_down) > 0 && length(steep_up) > 0) {
    for (sd in steep_down) {
      matching_up <- steep_up[steep_up > sd]
      if (length(matching_up) > 0) {
        su <- matching_up[1]
        if (su - sd >= min_cluster_size) {
          current_cluster <- current_cluster + 1
          cluster_id[sd:su] <- current_cluster
        }
      }
    }
  }
  
  # Reorder to original indices
  result <- rep(0, n)
  result[optics_result$order] <- cluster_id
  
  return(result)
}

# Plot reachability plot
plot_reachability <- function(optics_result, main = "OPTICS Reachability Plot") {
  reach <- optics_result$reachability
  max_reach <- max(reach[is.finite(reach)])
  reach[is.infinite(reach)] <- max_reach * 1.2
  
  plot(1:length(reach), reach, type = "h", lwd = 2,
       xlab = "Cluster Order", ylab = "Reachability Distance",
       main = main, col = "steelblue")
  grid()
}

# Calculate silhouette score for clusters
calculate_silhouette <- function(data, clusters) {
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }
  
  unique_clusters <- unique(clusters[clusters != 0])
  if (length(unique_clusters) < 2) {
    return(NA)
  }
  
  dist_matrix <- as.matrix(dist(data))
  n <- nrow(data)
  silhouette_scores <- rep(0, n)
  
  for (i in 1:n) {
    if (clusters[i] == 0) next
    
    same_cluster <- which(clusters == clusters[i])
    same_cluster <- same_cluster[same_cluster != i]
    
    if (length(same_cluster) == 0) {
      silhouette_scores[i] <- 0
      next
    }
    
    a <- mean(dist_matrix[i, same_cluster])
    
    b <- Inf
    for (other_cluster in unique_clusters) {
      if (other_cluster == clusters[i]) next
      other_points <- which(clusters == other_cluster)
      if (length(other_points) > 0) {
        b <- min(b, mean(dist_matrix[i, other_points]))
      }
    }
    
    silhouette_scores[i] <- (b - a) / max(a, b)
  }
  
  return(mean(silhouette_scores[clusters != 0]))
}

# Example usage and tests
cat("=== OPTICS Clustering Algorithm ===\n\n")

# Example 1: Simple 2D dataset
cat("Example 1: Simple 2D Dataset\n")
set.seed(42)
cluster1 <- matrix(rnorm(100, mean = 0, sd = 0.5), ncol = 2)
cluster2 <- matrix(rnorm(100, mean = 3, sd = 0.5), ncol = 2)
cluster3 <- matrix(rnorm(100, mean = c(1.5, 3), sd = 0.5), ncol = 2)
data1 <- rbind(cluster1, cluster2, cluster3)

cat("Running OPTICS...\n")
result1 <- optics(data1, eps = 1.0, minPts = 5)
cat("Processed", length(result1$order), "points\n")
cat("First 10 reachability distances:", head(result1$reachability, 10), "\n")

# Extract clusters
clusters1 <- extract_dbscan_clusters(result1, eps_cluster = 0.8)
cat("Clusters found:", length(unique(clusters1[clusters1 != 0])), "\n")
cat("Noise points:", sum(clusters1 == 0), "\n")

if (length(unique(clusters1[clusters1 != 0])) >= 2) {
  silhouette1 <- calculate_silhouette(data1, clusters1)
  cat("Silhouette score:", round(silhouette1, 4), "\n")
}

# Example 2: Dataset with varying densities
cat("\n\nExample 2: Varying Density Dataset\n")
set.seed(123)
dense_cluster <- matrix(rnorm(200, mean = 0, sd = 0.3), ncol = 2)
sparse_cluster <- matrix(rnorm(100, mean = 4, sd = 1.0), ncol = 2)
data2 <- rbind(dense_cluster, sparse_cluster)

result2 <- optics(data2, eps = 2.0, minPts = 5)
cat("Processed", length(result2$order), "points\n")

clusters2_dbscan <- extract_dbscan_clusters(result2, eps_cluster = 1.0)
cat("DBSCAN-style extraction - Clusters:", length(unique(clusters2_dbscan[clusters2_dbscan != 0])), "\n")

clusters2_xi <- extract_xi_clusters(result2, xi = 0.05, min_cluster_size = 10)
cat("Xi-method extraction - Clusters:", length(unique(clusters2_xi[clusters2_xi != 0])), "\n")

# Example 3: Iris dataset
cat("\n\nExample 3: Iris Dataset\n")
data(iris)
iris_data <- iris[, 1:4]

result3 <- optics(iris_data, eps = 2.0, minPts = 5)
cat("Processed", length(result3$order), "points\n")

clusters3 <- extract_dbscan_clusters(result3, eps_cluster = 0.5)
cat("Clusters found:", length(unique(clusters3[clusters3 != 0])), "\n")
cat("Noise points:", sum(clusters3 == 0), "\n")

# Compare with actual species
if (length(unique(clusters3[clusters3 != 0])) >= 2) {
  silhouette3 <- calculate_silhouette(iris_data, clusters3)
  cat("Silhouette score:", round(silhouette3, 4), "\n")
}

# Example 4: Moons dataset (non-convex clusters)
cat("\n\nExample 4: Two Moons Dataset (Non-convex)\n")
set.seed(456)
n_points <- 100
t1 <- seq(0, pi, length.out = n_points)
moon1 <- cbind(cos(t1), sin(t1)) + matrix(rnorm(n_points * 2, sd = 0.1), ncol = 2)
moon2 <- cbind(1 - cos(t1), 1 - sin(t1) - 0.5) + matrix(rnorm(n_points * 2, sd = 0.1), ncol = 2)
moons_data <- rbind(moon1, moon2)

result4 <- optics(moons_data, eps = 0.5, minPts = 5)
clusters4 <- extract_dbscan_clusters(result4, eps_cluster = 0.3)
cat("Clusters found:", length(unique(clusters4[clusters4 != 0])), "\n")
cat("Noise points:", sum(clusters4 == 0), "\n")

if (length(unique(clusters4[clusters4 != 0])) >= 2) {
  silhouette4 <- calculate_silhouette(moons_data, clusters4)
  cat("Silhouette score:", round(silhouette4, 4), "\n")
}

# Example 5: Performance comparison
cat("\n\nExample 5: Performance Test\n")
sizes <- c(100, 200, 500)
cat("Dataset size vs execution time:\n")

for (size in sizes) {
  test_data <- matrix(rnorm(size * 2), ncol = 2)
  start_time <- Sys.time()
  test_result <- optics(test_data, eps = 1.0, minPts = 5)
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  cat(sprintf("  %d points: %.3f seconds\n", size, elapsed))
}

cat("\n=== All tests completed successfully ===\n")

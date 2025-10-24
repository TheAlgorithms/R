# Traveling Salesman Problem (TSP) using Bitmask Dynamic Programming
#
# The Traveling Salesman Problem finds the shortest possible route that visits 
# each city exactly once and returns to the starting city. This implementation 
# uses bitmask DP to efficiently track visited cities.
#
# Time Complexity: O(n² * 2^n) where n = number of cities
# Space Complexity: O(n * 2^n) for memoization table
#
# Applications:
# - Route optimization and logistics
# - Circuit board drilling and manufacturing
# - DNA sequencing and genome mapping
# - Network design and optimization
# - Scheduling and planning problems
# - Microchip design and fabrication

# Main TSP function using bitmask DP
tsp_bitmask_dp <- function(dist) {
  #' Solve the Traveling Salesman Problem using Bitmask Dynamic Programming
  #' @param dist: 2D matrix where dist[i, j] is the distance from city i to city j
  #' @return: Minimum cost to visit all cities and return to starting city
  
  n <- nrow(dist)
  
  # Bitmask when all cities are visited
  ALL_VISITED <- bitwShiftL(1, n) - 1
  
  # Initialize memoization table
  # memo[pos, mask] = minimum cost starting from pos with visited cities in mask
  memo <- matrix(NA, nrow = n, ncol = bitwShiftL(1, n))
  
  # Recursive DP helper function
  dp <- function(mask, pos) {
    #' Recursive DP function to compute minimum travel cost
    #' @param mask: Bitmask representing visited cities
    #' @param pos: Current city position (0-indexed)
    #' @return: Minimum travel cost from current state
    
    # Base case: all cities visited, return to starting city (city 0)
    if (mask == ALL_VISITED) {
      return(dist[pos + 1, 1])
    }
    
    # Check memoization table
    if (!is.na(memo[pos + 1, mask + 1])) {
      return(memo[pos + 1, mask + 1])
    }
    
    # Initialize minimum cost as infinity
    min_cost <- Inf
    
    # Try visiting each unvisited city
    for (city in 0:(n - 1)) {
      # Check if city is not visited (bit is 0)
      if (bitwAnd(mask, bitwShiftL(1, city)) == 0) {
        # Mark city as visited
        new_mask <- bitwOr(mask, bitwShiftL(1, city))
        # Calculate cost: distance to city + cost from city
        cost <- dist[pos + 1, city + 1] + dp(new_mask, city)
        min_cost <- min(min_cost, cost)
      }
    }
    
    # Store result in memo table
    memo[pos + 1, mask + 1] <<- min_cost
    return(min_cost)
  }
  
  # Start from city 0 with only city 0 visited (mask = 1)
  result <- dp(1, 0)
  return(result)
}

# Function to get the optimal path (with path reconstruction)
tsp_bitmask_with_path <- function(dist) {
  #' Solve TSP and return both minimum cost and the optimal path
  #' @param dist: 2D distance matrix
  #' @return: List containing minimum cost and optimal path
  
  n <- nrow(dist)
  ALL_VISITED <- bitwShiftL(1, n) - 1
  
  # Memoization tables
  memo <- matrix(NA, nrow = n, ncol = bitwShiftL(1, n))
  parent <- matrix(NA, nrow = n, ncol = bitwShiftL(1, n))
  
  # DP function with path tracking
  dp <- function(mask, pos) {
    if (mask == ALL_VISITED) {
      return(dist[pos + 1, 1])
    }
    
    if (!is.na(memo[pos + 1, mask + 1])) {
      return(memo[pos + 1, mask + 1])
    }
    
    min_cost <- Inf
    best_city <- -1
    
    for (city in 0:(n - 1)) {
      if (bitwAnd(mask, bitwShiftL(1, city)) == 0) {
        new_mask <- bitwOr(mask, bitwShiftL(1, city))
        cost <- dist[pos + 1, city + 1] + dp(new_mask, city)
        if (cost < min_cost) {
          min_cost <- cost
          best_city <- city
        }
      }
    }
    
    memo[pos + 1, mask + 1] <<- min_cost
    parent[pos + 1, mask + 1] <<- best_city
    return(min_cost)
  }
  
  # Get minimum cost
  min_cost <- dp(1, 0)
  
  # Reconstruct path
  path <- c(0)  # Start at city 0
  mask <- 1
  pos <- 0
  
  while (mask != ALL_VISITED) {
    next_city <- parent[pos + 1, mask + 1]
    path <- c(path, next_city)
    mask <- bitwOr(mask, bitwShiftL(1, next_city))
    pos <- next_city
  }
  
  path <- c(path, 0)  # Return to starting city
  
  return(list(
    min_cost = min_cost,
    path = path,
    path_cities = path + 1  # Convert to 1-indexed for display
  ))
}

# Helper function to print distance matrix
print_distance_matrix <- function(dist) {
  #' Print a formatted distance matrix
  #' @param dist: Distance matrix to print
  
  n <- nrow(dist)
  cat("Distance Matrix:\n")
  cat("     ")
  for (j in 1:n) {
    cat(sprintf("C%-3d ", j))
  }
  cat("\n")
  
  for (i in 1:n) {
    cat(sprintf("C%-3d ", i))
    for (j in 1:n) {
      cat(sprintf("%-4d ", dist[i, j]))
    }
    cat("\n")
  }
  cat("\n")
}

# ========== Example Usage ==========

# Example 1: Small 4-city problem
cat("========== Example 1: 4 Cities ==========\n\n")
dist_matrix_1 <- matrix(c(
  0, 10, 15, 20,
  10, 0, 35, 25,
  15, 35, 0, 30,
  20, 25, 30, 0
), nrow = 4, byrow = TRUE)

print_distance_matrix(dist_matrix_1)

min_cost_1 <- tsp_bitmask_dp(dist_matrix_1)
cat(sprintf("Minimum cost to visit all cities: %d\n\n", min_cost_1))

# Get path as well
result_1 <- tsp_bitmask_with_path(dist_matrix_1)
cat(sprintf("Optimal path: %s\n", paste(result_1$path_cities, collapse = " -> ")))
cat(sprintf("Total cost: %d\n\n", result_1$min_cost))

# Example 2: Another 4-city problem
cat("========== Example 2: Another 4-City Problem ==========\n\n")
dist_matrix_2 <- matrix(c(
  0, 20, 42, 35,
  20, 0, 30, 34,
  42, 30, 0, 12,
  35, 34, 12, 0
), nrow = 4, byrow = TRUE)

print_distance_matrix(dist_matrix_2)

result_2 <- tsp_bitmask_with_path(dist_matrix_2)
cat(sprintf("Optimal path: %s\n", paste(result_2$path_cities, collapse = " -> ")))
cat(sprintf("Total cost: %d\n\n", result_2$min_cost))

# Example 3: Small 5-city problem
cat("========== Example 3: 5 Cities ==========\n\n")
dist_matrix_3 <- matrix(c(
  0, 12, 10, 19, 8,
  12, 0, 3, 7, 6,
  10, 3, 0, 2, 20,
  19, 7, 2, 0, 4,
  8, 6, 20, 4, 0
), nrow = 5, byrow = TRUE)

print_distance_matrix(dist_matrix_3)

result_3 <- tsp_bitmask_with_path(dist_matrix_3)
cat(sprintf("Optimal path: %s\n", paste(result_3$path_cities, collapse = " -> ")))
cat(sprintf("Total cost: %d\n\n", result_3$min_cost))

# Performance note
cat("========== Performance Note ==========\n")
cat("This algorithm works well for small n (typically n <= 20).\n")
cat("For larger instances, consider:\n")
cat("  - Heuristic approaches (Nearest Neighbor, 2-opt)\n")
cat("  - Approximation algorithms (Christofides algorithm)\n")
cat("  - Metaheuristics (Genetic Algorithms, Simulated Annealing)\n")

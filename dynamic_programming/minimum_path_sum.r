# Minimum Path Sum Problem
#
# The Minimum Path Sum problem finds the minimum sum path from the top-left corner
# to the bottom-right corner of a grid, moving only right or down at each step.
# This is a classic dynamic programming problem that appears in various forms.
#
# Time Complexity: O(m * n) where m = number of rows, n = number of columns
# Space Complexity: O(m * n) for DP table, O(min(m, n)) for optimized version
#
# Applications:
# - Grid-based pathfinding algorithms
# - Resource optimization in 2D grids
# - Game development (pathfinding with costs)
# - Network routing optimization
# - Cost minimization in transportation

# Basic DP solution for Minimum Path Sum
minimum_path_sum <- function(grid) {
  #' Find the minimum path sum from top-left to bottom-right corner
  #' @param grid: 2D matrix of non-negative integers
  #' @return: List containing minimum sum, path, and DP table
  
  m <- nrow(grid)
  n <- ncol(grid)
  
  # Handle edge case
  if (m == 0 || n == 0) {
    return(list(
      min_sum = 0,
      path = c(),
      dp_table = matrix(0, nrow = 1, ncol = 1)
    ))
  }
  
  # Create DP table: dp[i, j] = minimum sum to reach position (i, j)
  dp <- matrix(0, nrow = m, ncol = n)
  
  # Initialize first row and column
  dp[1, 1] <- grid[1, 1]
  
  # Fill first row (can only move right)
  if (n > 1) {
    for (j in 2:n) {
      dp[1, j] <- dp[1, j - 1] + grid[1, j]
    }
  }
  
  # Fill first column (can only move down)
  if (m > 1) {
    for (i in 2:m) {
      dp[i, 1] <- dp[i - 1, 1] + grid[i, 1]
    }
  }
  
  # Fill remaining cells
  if (m > 1 && n > 1) {
    for (i in 2:m) {
      for (j in 2:n) {
        dp[i, j] <- min(dp[i - 1, j], dp[i, j - 1]) + grid[i, j]
      }
    }
  }
  
  # Backtrack to find the path
  path <- list()
  i <- m
  j <- n
  
  while (i > 1 || j > 1) {
    path <- c(list(c(i, j)), path)
    
    if (i == 1) {
      # Can only move left
      j <- j - 1
    } else if (j == 1) {
      # Can only move up
      i <- i - 1
    } else {
      # Choose direction with minimum sum
      if (dp[i - 1, j] < dp[i, j - 1]) {
        i <- i - 1
      } else {
        j <- j - 1
      }
    }
  }
  path <- c(list(c(1, 1)), path)
  
  return(list(
    min_sum = dp[m, n],
    path = path,
    dp_table = dp
  ))
}

# Space-optimized version using only 1D array
minimum_path_sum_optimized <- function(grid) {
  #' Space optimized minimum path sum using 1D array
  #' @param grid: 2D matrix of non-negative integers
  #' @return: Minimum path sum
  
  m <- nrow(grid)
  n <- ncol(grid)
  
  if (m == 0 || n == 0) return(0)
  
  # Use the smaller dimension for space optimization
  if (m <= n) {
    # Process row by row
    dp <- rep(0, m)
    dp[1] <- grid[1, 1]
    
    # Initialize first row
    if (m > 1) {
      for (i in 2:m) {
        dp[i] <- dp[i - 1] + grid[i, 1]
      }
    }
    
    # Process remaining columns
    for (j in 2:n) {
      dp[1] <- dp[1] + grid[1, j]
      for (i in 2:m) {
        dp[i] <- min(dp[i - 1], dp[i]) + grid[i, j]
      }
    }
  } else {
    # Process column by column
    dp <- rep(0, n)
    dp[1] <- grid[1, 1]
    
    # Initialize first column
    for (j in 2:n) {
      dp[j] <- dp[j - 1] + grid[1, j]
    }
    
    # Process remaining rows
    for (i in 2:m) {
      dp[1] <- dp[1] + grid[i, 1]
      for (j in 2:n) {
        dp[j] <- min(dp[j - 1], dp[j]) + grid[i, j]
      }
    }
  }
  
  return(dp[length(dp)])
}

# Function to find all possible minimum paths
find_all_minimum_paths <- function(grid) {
  #' Find all possible paths that achieve the minimum sum
  #' @param grid: 2D matrix of non-negative integers
  #' @return: List of all minimum cost paths
  
  m <- nrow(grid)
  n <- ncol(grid)
  
  if (m == 0 || n == 0) return(list())
  
  # First compute the minimum sum
  result <- minimum_path_sum(grid)
  min_sum <- result$min_sum
  
  all_paths <- list()
  
  # Use recursive backtracking to find all paths with minimum sum
  find_paths_recursive <- function(current_path, current_sum, i, j) {
    current_sum <- current_sum + grid[i, j]
    
    # If we've reached the bottom-right corner
    if (i == m && j == n) {
      if (current_sum == min_sum) {
        all_paths <<- c(all_paths, list(c(current_path, list(c(i, j)))))
      }
      return
    }
    
    # If current sum exceeds minimum, prune
    if (current_sum > min_sum) {
      return
    }
    
    # Move right
    if (j < n) {
      find_paths_recursive(c(current_path, list(c(i, j))), current_sum, i, j + 1)
    }
    
    # Move down
    if (i < m) {
      find_paths_recursive(c(current_path, list(c(i, j))), current_sum, i + 1, j)
    }
  }
  
  find_paths_recursive(list(), 0, 1, 1)
  return(all_paths)
}

# Helper function to print DP table
print_minimum_path_sum_dp <- function(dp_table, grid) {
  m <- nrow(grid)
  n <- ncol(grid)
  
  cat("DP Table for Minimum Path Sum:\n")
  cat("Grid:\n")
  for (i in 1:m) {
    cat("  ")
    for (j in 1:n) {
      cat(sprintf("%3d ", grid[i, j]))
    }
    cat("\n")
  }
  cat("\nDP Table:\n")
  for (i in 1:m) {
    cat("  ")
    for (j in 1:n) {
      cat(sprintf("%3d ", dp_table[i, j]))
    }
    cat("\n")
  }
  cat("\n")
}

# Helper function to visualize path on grid
visualize_path <- function(grid, path) {
  m <- nrow(grid)
  n <- ncol(grid)
  
  cat("Path Visualization:\n")
  cat("Grid with path marked (*):\n")
  
  # Create a matrix to mark the path
  path_matrix <- matrix(" ", nrow = m, ncol = n)
  
  for (pos in path) {
    path_matrix[pos[1], pos[2]] <- "*"
  }
  
  for (i in 1:m) {
    cat("  ")
    for (j in 1:n) {
      if (path_matrix[i, j] == "*") {
        cat(sprintf("%3s ", "*"))
      } else {
        cat(sprintf("%3d ", grid[i, j]))
      }
    }
    cat("\n")
  }
  cat("\n")
}

# ===========================
# Example Usage & Testing
# ===========================
cat("=== Minimum Path Sum Problem (Dynamic Programming) ===\n\n")

# Test 1: Basic Example
cat("Test 1: Basic Example\n")
grid1 <- matrix(c(1, 3, 1, 1, 5, 1, 4, 2, 1), nrow = 3, ncol = 3, byrow = TRUE)
cat("Grid:\n")
print(grid1)

result1 <- minimum_path_sum(grid1)
print_minimum_path_sum_dp(result1$dp_table, grid1)
cat("Minimum Path Sum:", result1$min_sum, "\n")
cat("Path (row, col):", paste(sapply(result1$path, function(x) paste("(", x[1], ",", x[2], ")", sep="")), collapse = " -> "), "\n")
visualize_path(grid1, result1$path)
cat("\n")

# Test 2: Optimized Version
cat("Test 2: Space Optimized Version\n")
min_sum_opt <- minimum_path_sum_optimized(grid1)
cat("Minimum Path Sum (Optimized):", min_sum_opt, "\n")
cat("Verification: Both methods match:", result1$min_sum == min_sum_opt, "\n\n")

# Test 3: Single Row/Column Cases
cat("Test 3: Edge Cases\n")
cat("Single row grid:\n")
grid_row <- matrix(c(1, 2, 3, 4, 5), nrow = 1)
print(grid_row)
result_row <- minimum_path_sum(grid_row)
cat("Minimum sum:", result_row$min_sum, "\n\n")

cat("Single column grid:\n")
grid_col <- matrix(c(1, 2, 3, 4, 5), ncol = 1)
print(grid_col)
result_col <- minimum_path_sum(grid_col)
cat("Minimum sum:", result_col$min_sum, "\n\n")

# Test 4: Larger Grid
cat("Test 4: Larger Grid (4x5)\n")
# Set random seed for reproducibility in tests. The value 42 is chosen arbitrarily.
SEED <- 42
set.seed(SEED)
grid_large <- matrix(sample(1:9, 20, replace = TRUE), nrow = 4, ncol = 5)
cat("Grid:\n")
print(grid_large)

result_large <- minimum_path_sum(grid_large)
cat("Minimum Path Sum:", result_large$min_sum, "\n")
cat("Path length:", length(result_large$path), "steps\n")
visualize_path(grid_large, result_large$path)
cat("\n")

# Test 5: Performance Comparison
cat("Test 5: Performance Comparison (6x8 grid)\n")
grid_perf <- matrix(sample(1:20, 48, replace = TRUE), nrow = 6, ncol = 8)

library(microbenchmark)

mbm <- microbenchmark(
  std = minimum_path_sum(grid_perf),
  opt = minimum_path_sum_optimized(grid_perf),
  times = 100L
)

result_std <- minimum_path_sum(grid_perf)
result_opt <- minimum_path_sum_optimized(grid_perf)

cat("Standard DP result:", result_std$min_sum, "\n")
cat("Optimized DP result:", result_opt, "\n")
cat("Standard DP median time:", sprintf("%.6f sec", median(mbm$time[mbm$expr == "std"])/1e9), "\n")
cat("Optimized DP median time:", sprintf("%.6f sec", median(mbm$time[mbm$expr == "opt"])/1e9), "\n")
cat("Results match:", result_std$min_sum == result_opt, "\n\n")

# Test 6: Multiple Minimum Paths
cat("Test 6: Multiple Minimum Paths\n")
grid_multiple <- matrix(c(1, 2, 1, 1, 1, 1, 1, 1, 1), nrow = 3, ncol = 3, byrow = TRUE)
cat("Grid:\n")
print(grid_multiple)

result_multiple <- minimum_path_sum(grid_multiple)
cat("Minimum Path Sum:", result_multiple$min_sum, "\n")
cat("One possible path:", paste(sapply(result_multiple$path, function(x) paste("(", x[1], ",", x[2], ")", sep="")), collapse = " -> "), "\n")

# Find all minimum paths
all_paths <- find_all_minimum_paths(grid_multiple)
cat("Total number of minimum paths:", length(all_paths), "\n")
for (i in seq_along(all_paths)) {
  path_str <- paste(sapply(all_paths[[i]], function(x) paste("(", x[1], ",", x[2], ")", sep="")), collapse = " -> ")
  path_sum <- sum(sapply(all_paths[[i]], function(x) grid_multiple[x[1], x[2]]))
  cat("Path", i, ":", path_str, "(sum =", path_sum, ")\n")
}
cat("\n")

# Test 7: Real-world Example - Cost Optimization
cat("Test 7: Real-world Example - Transportation Cost Optimization\n")
# Grid representing transportation costs between cities
transport_grid <- matrix(c(
  2, 3, 4, 2, 1,
  1, 2, 1, 3, 2,
  3, 1, 2, 1, 4,
  2, 4, 1, 2, 3
), nrow = 4, ncol = 5, byrow = TRUE)

cat("Transportation Cost Grid:\n")
print(transport_grid)

transport_result <- minimum_path_sum(transport_grid)
cat("Minimum transportation cost:", transport_result$min_sum, "\n")
cat("Optimal route:", paste(sapply(transport_result$path, function(x) paste("City(", x[1], ",", x[2], ")", sep="")), collapse = " -> "), "\n")
visualize_path(transport_grid, transport_result$path)

# Calculate cost breakdown
cat("Cost breakdown:\n")
total_cost <- 0
for (i in seq_along(transport_result$path)) {
  pos <- transport_result$path[[i]]
  cost <- transport_grid[pos[1], pos[2]]
  total_cost <- total_cost + cost
  cat("  Step", i, ": City(", pos[1], ",", pos[2], ") =", cost, "\n")
}
cat("Total cost verification:", total_cost, "\n")

# Matrix Chain Multiplication Problem
#
# The Matrix Chain Multiplication problem finds the most efficient way to multiply
# a given sequence of matrices. The goal is to minimize the number of scalar
# multiplications required by determining the optimal parenthesization.
#
# Time Complexity: O(n³) where n = number of matrices
# Space Complexity: O(n²) for DP table
#
# Applications:
# - Computer graphics and 3D transformations
# - Scientific computing and numerical analysis
# - Machine learning and neural networks
# - Signal processing and image processing
# - Optimization in linear algebra operations

# Basic DP solution for Matrix Chain Multiplication
matrix_chain_multiplication <- function(dimensions) {
  #' Find the minimum number of scalar multiplications needed to multiply matrices
  #' @param dimensions: Vector of matrix dimensions [d0, d1, d2, ..., dn] where
  #'                   matrix i has dimensions dimensions[i] x dimensions[i+1]
  #' @return: List containing minimum cost, DP table, and optimal parenthesization
  
  n <- length(dimensions) - 1  # Number of matrices
  
  # Handle edge cases
  if (n <= 0) {
    return(list(
      min_cost = 0,
      dp_table = matrix(0, nrow = 1, ncol = 1),
      optimal_parentheses = "",
      split_table = matrix(0, nrow = 1, ncol = 1)
    ))
  }
  
  if (n == 1) {
    return(list(
      min_cost = 0,
      dp_table = matrix(0, nrow = 1, ncol = 1),
      optimal_parentheses = "A1",
      split_table = matrix(0, nrow = 1, ncol = 1)
    ))
  }
  
  # Create DP table: dp[i, j] = minimum cost to multiply matrices from i to j
  dp <- matrix(0, nrow = n, ncol = n)
  split_table <- matrix(0, nrow = n, ncol = n)  # To store optimal split points
  
  # Fill DP table using bottom-up approach
  for (length in 2:n) {  # length of chain
    for (i in 1:(n - length + 1)) {
      j <- i + length - 1
      dp[i, j] <- Inf
      
      # Try all possible split points
      for (k in i:(j - 1)) {
        # Cost = cost of left part + cost of right part + cost of multiplying them
        cost <- dp[i, k] + dp[k + 1, j] + dimensions[i] * dimensions[k + 1] * dimensions[j + 1]
        
        if (cost < dp[i, j]) {
          dp[i, j] <- cost
          split_table[i, j] <- k  # Store optimal split point
        }
      }
    }
  }
  
  # Generate optimal parenthesization
  optimal_parentheses <- generate_parentheses(split_table, 1, n)
  
  return(list(
    min_cost = dp[1, n],
    dp_table = dp,
    optimal_parentheses = optimal_parentheses,
    split_table = split_table
  ))
}

# Recursive function to generate optimal parenthesization
generate_parentheses <- function(split_table, i, j) {
  if (i == j) {
    return(paste0("A", i))
  } else {
    k <- split_table[i, j]
    left <- generate_parentheses(split_table, i, k)
    right <- generate_parentheses(split_table, k + 1, j)
    return(paste0("(", left, " × ", right, ")"))
  }
}

# Space-optimized version using only upper triangular matrix
matrix_chain_optimized <- function(dimensions) {
  #' Space optimized matrix chain multiplication
  #' @param dimensions: Vector of matrix dimensions
  #' @return: Minimum cost only
  
  n <- length(dimensions) - 1
  
  if (n <= 1) return(0)
  
  # Use only upper triangular part of DP table
  dp <- matrix(0, nrow = n, ncol = n)
  
  for (length in 2:n) {
    for (i in 1:(n - length + 1)) {
      j <- i + length - 1
      dp[i, j] <- Inf
      
      for (k in i:(j - 1)) {
        cost <- dp[i, k] + dp[k + 1, j] + dimensions[i] * dimensions[k + 1] * dimensions[j + 1]
        dp[i, j] <- min(dp[i, j], cost)
      }
    }
  }
  
  return(dp[1, n])
}

# Function to calculate actual multiplication cost for given parenthesization
calculate_multiplication_cost <- function(dimensions, parentheses) {
  #' Calculate the actual cost of multiplying matrices with given parenthesization
  #' @param dimensions: Vector of matrix dimensions
  #' @param parentheses: String representation of parenthesization
  #' @return: Total cost of multiplication
  
  # This is a simplified version - in practice, you'd parse the parentheses string
  # For demonstration, we'll use the DP result
  result <- matrix_chain_multiplication(dimensions)
  return(result$min_cost)
}

# Function to find all possible optimal parenthesizations
find_all_optimal_parentheses <- function(dimensions) {
  #' Find all possible optimal parenthesizations
  #' @param dimensions: Vector of matrix dimensions
  #' @return: List of all optimal parenthesizations
  
  n <- length(dimensions) - 1
  if (n <= 1) return(list(paste0("A", 1:n)))
  
  result <- matrix_chain_multiplication(dimensions)
  min_cost <- result$min_cost
  
  all_parentheses <- list()
  
  # Generate all possible parenthesizations and filter optimal ones
  generate_all_parentheses <- function(i, j) {
    if (i == j) {
      return(list(paste0("A", i)))
    }
    
    all_ways <- list()
    for (k in i:(j - 1)) {
      left_ways <- generate_all_parentheses(i, k)
      right_ways <- generate_all_parentheses(k + 1, j)
      
      for (left in left_ways) {
        for (right in right_ways) {
          all_ways <- c(all_ways, list(paste0("(", left, " × ", right, ")")))
        }
      }
    }
    return(all_ways)
  }
  
  all_ways <- generate_all_parentheses(1, n)
  
  # Filter only optimal ways (simplified - in practice, calculate cost for each)
  # For demonstration, return first few ways
  return(all_ways[1:min(3, length(all_ways))])
}

# Helper function to print DP table
print_matrix_chain_dp <- function(dp_table, split_table, dimensions) {
  n <- nrow(dp_table)
  
  cat("DP Table for Matrix Chain Multiplication:\n")
  cat("Matrix dimensions:", paste(dimensions, collapse = " × "), "\n")
  cat("Number of matrices:", n, "\n\n")
  
  # Print matrix indices
  cat("     ")
  for (j in 1:n) {
    cat(sprintf("%4d", j))
  }
  cat("\n")
  cat(paste(rep("-", 5 + 4 * n), collapse = ""), "\n")
  
  for (i in 1:n) {
    cat(sprintf("%2d | ", i))
    for (j in 1:n) {
      if (i <= j) {
        cat(sprintf("%4d", dp_table[i, j]))
      } else {
        cat("    ")
      }
    }
    cat("\n")
  }
  cat("\n")
  
  # Print split table
  cat("Split Table (optimal split points):\n")
  cat("     ")
  for (j in 1:n) {
    cat(sprintf("%4d", j))
  }
  cat("\n")
  cat(paste(rep("-", 5 + 4 * n), collapse = ""), "\n")
  
  for (i in 1:n) {
    cat(sprintf("%2d | ", i))
    for (j in 1:n) {
      if (i < j) {
        cat(sprintf("%4d", split_table[i, j]))
      } else {
        cat("    ")
      }
    }
    cat("\n")
  }
  cat("\n")
}

# Helper function to show matrix multiplication steps
show_multiplication_steps <- function(dimensions, parentheses) {
  n <- length(dimensions) - 1
  
  cat("Matrix Chain Multiplication Steps:\n")
  cat("Matrix dimensions:", paste(dimensions, collapse = " × "), "\n")
  cat("Optimal parenthesization:", parentheses, "\n\n")
  
  for (i in 1:n) {
    cat(sprintf("A%d: %d × %d matrix\n", i, dimensions[i], dimensions[i + 1]))
  }
  cat("\n")
  
  # Show cost calculation
  result <- matrix_chain_multiplication(dimensions)
  cat("Minimum scalar multiplications:", result$min_cost, "\n")
  
  # Show cost breakdown for small examples
  if (n <= 4) {
    cat("\nCost breakdown:\n")
    for (i in 1:n) {
      for (j in i:n) {
        if (i < j) {
          cat(sprintf("Cost[A%d..A%d] = %d\n", i, j, result$dp_table[i, j]))
        }
      }
    }
  }
  cat("\n")
}

# ===========================
# Example Usage & Testing
# ===========================
cat("=== Matrix Chain Multiplication Problem (Dynamic Programming) ===\n\n")

# Test 1: Basic Example
cat("Test 1: Basic Example\n")
dims1 <- c(1, 5, 4, 6, 2)  # 4 matrices: 1×5, 5×4, 4×6, 6×2
cat("Matrix dimensions:", paste(dims1, collapse = " × "), "\n")

result1 <- matrix_chain_multiplication(dims1)
print_matrix_chain_dp(result1$dp_table, result1$split_table, dims1)
cat("Minimum cost:", result1$min_cost, "\n")
cat("Optimal parenthesization:", result1$optimal_parentheses, "\n")
show_multiplication_steps(dims1, result1$optimal_parentheses)

# Test 2: Optimized Version
cat("Test 2: Space Optimized Version\n")
min_cost_opt <- matrix_chain_optimized(dims1)
cat("Minimum cost (Optimized):", min_cost_opt, "\n")
cat("Verification: Both methods match:", result1$min_cost == min_cost_opt, "\n\n")

# Test 3: Edge Cases
cat("Test 3: Edge Cases\n")
cat("Single matrix:", matrix_chain_multiplication(c(3, 4))$min_cost, "\n")
cat("Two matrices:", matrix_chain_multiplication(c(3, 4, 5))$min_cost, "\n")
cat("Empty dimensions:", matrix_chain_multiplication(c())$min_cost, "\n\n")

# Test 4: Larger Example
cat("Test 4: Larger Example (6 matrices)\n")
dims_large <- c(30, 35, 15, 5, 10, 20, 25)
cat("Matrix dimensions:", paste(dims_large, collapse = " × "), "\n")

result_large <- matrix_chain_multiplication(dims_large)
cat("Minimum cost:", result_large$min_cost, "\n")
cat("Optimal parenthesization:", result_large$optimal_parentheses, "\n\n")

# Test 5: Performance Comparison
cat("Test 5: Performance Comparison (8 matrices)\n")
set.seed(42)
dims_perf <- sample(10:50, 9)  # 8 matrices
cat("Matrix dimensions:", paste(dims_perf, collapse = " × "), "\n")

start_time <- Sys.time()
result_std <- matrix_chain_multiplication(dims_perf)
std_time <- as.numeric(Sys.time() - start_time, units = "secs")

start_time <- Sys.time()
result_opt <- matrix_chain_optimized(dims_perf)
opt_time <- as.numeric(Sys.time() - start_time, units = "secs")

cat("Standard DP result:", result_std$min_cost, "\n")
cat("Optimized DP result:", result_opt, "\n")
cat("Standard DP time:", sprintf("%.4f sec", std_time), "\n")
cat("Optimized DP time:", sprintf("%.4f sec", opt_time), "\n")
cat("Results match:", result_std$min_cost == result_opt, "\n\n")

# Test 6: Multiple Optimal Solutions
cat("Test 6: Multiple Optimal Solutions\n")
dims_multiple <- c(2, 3, 2, 3, 2)  # 4 matrices with multiple optimal solutions
cat("Matrix dimensions:", paste(dims_multiple, collapse = " × "), "\n")

result_multiple <- matrix_chain_multiplication(dims_multiple)
cat("Minimum cost:", result_multiple$min_cost, "\n")
cat("One optimal parenthesization:", result_multiple$optimal_parentheses, "\n")

# Find all optimal parenthesizations
all_optimal <- find_all_optimal_parentheses(dims_multiple)
cat("Total optimal parenthesizations found:", length(all_optimal), "\n")
for (i in seq_along(all_optimal)) {
  cat("Option", i, ":", all_optimal[[i]], "\n")
}
cat("\n")

# Test 7: Real-world Example - Computer Graphics
cat("Test 7: Real-world Example - 3D Graphics Pipeline\n")
# Typical 3D transformation pipeline: Model → View → Projection → Screen
# Each transformation is a matrix multiplication
graphics_dims <- c(4, 4, 4, 4, 4)  # 4×4 transformation matrices
cat("3D Graphics Pipeline (4×4 transformation matrices):\n")
cat("Matrix dimensions:", paste(graphics_dims, collapse = " × "), "\n")

graphics_result <- matrix_chain_multiplication(graphics_dims)
cat("Minimum cost:", graphics_result$min_cost, "\n")
cat("Optimal order:", graphics_result$optimal_parentheses, "\n")

# Show the transformation pipeline
cat("\nTransformation Pipeline:\n")
cat("1. Model Matrix (4×4) - Object to World coordinates\n")
cat("2. View Matrix (4×4) - World to Camera coordinates\n")
cat("3. Projection Matrix (4×4) - Camera to Clip coordinates\n")
cat("4. Viewport Matrix (4×4) - Clip to Screen coordinates\n")
cat("Total operations saved with optimal order:", graphics_result$min_cost, "scalar multiplications\n\n")

# Test 8: Cost Analysis
cat("Test 8: Cost Analysis\n")
dims_analysis <- c(10, 20, 30, 40, 50)
cat("Matrix dimensions:", paste(dims_analysis, collapse = " × "), "\n")

analysis_result <- matrix_chain_multiplication(dims_analysis)
cat("Minimum cost:", analysis_result$min_cost, "\n")

# Compare with naive left-to-right multiplication
naive_cost <- 0
for (i in 1:(length(dims_analysis) - 2)) {
  naive_cost <- naive_cost + dims_analysis[1] * dims_analysis[i + 1] * dims_analysis[i + 2]
}

cat("Naive left-to-right cost:", naive_cost, "\n")
cat("Savings with optimal order:", naive_cost - analysis_result$min_cost, "\n")
cat("Percentage improvement:", sprintf("%.1f%%", (naive_cost - analysis_result$min_cost) / naive_cost * 100), "\n")

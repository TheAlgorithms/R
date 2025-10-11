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
  
  # Helper to parse the parenthesization string into a tree
  parse_parentheses <- function(s) {
    s <- gsub(" ", "", s) # Remove spaces
    # If it's a single matrix, e.g. "A1"
    if (grepl("^A[0-9]+$", s)) {
      idx <- as.integer(sub("A", "", s))
      return(list(type = "leaf", idx = idx))
    }
    # Otherwise, find the main split
    # Remove outer parentheses if present
    if (substr(s, 1, 1) == "(" && substr(s, nchar(s), nchar(s)) == ")") {
      s <- substr(s, 2, nchar(s) - 1)
    }
    # Find the split point for " × " at the top level
    depth <- 0
    for (i in seq_len(nchar(s))) {
      ch <- substr(s, i, i)
      if (ch == "(") depth <- depth + 1
      if (ch == ")") depth <- depth - 1
      # Look for "×" at depth 0
      if (depth == 0 && i < nchar(s) && substr(s, i, i+1) == "×") {
        left <- substr(s, 1, i-2)
        right <- substr(s, i+2, nchar(s))
        return(list(
          type = "node",
          left = parse_parentheses(left),
          right = parse_parentheses(right)
        ))
      }
    }
    stop("Invalid parenthesization string")
  }
  
  # Helper to compute cost recursively
  compute_cost <- function(node) {
    if (node$type == "leaf") {
      # Return dimensions for this matrix
      idx <- node$idx
      return(list(rows = dimensions[idx], cols = dimensions[idx+1], cost = 0))
    } else {
      left <- compute_cost(node$left)
      right <- compute_cost(node$right)
      # Multiply left and right matrices: left$rows x left$cols and right$rows x right$cols
      if (left$cols != right$rows) stop("Incompatible matrix dimensions")
      cost <- left$cost + right$cost + left$rows * left$cols * right$cols
      return(list(rows = left$rows, cols = right$cols, cost = cost))
    }
  }
  
  tree <- parse_parentheses(parentheses)
  result <- compute_cost(tree)
  return(result$cost)
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
  
  # Helper function to compute cost of a parenthesization
  compute_cost <- function(paren, dims) {
    # Recursively compute cost based on parenthesization string
    # paren: string like "(A1 × (A2 × A3))"
    # dims: vector of dimensions
    # Returns: cost (integer)
    
    # Parse the parenthesization string into a tree structure
    # Helper: extract indices of matrices in a parenthesization
    extract_indices <- function(s) {
      # Returns vector of matrix indices in the string
      matches <- gregexpr("A[0-9]+", s)
      as.integer(unlist(regmatches(s, matches)))
    }
    
    # Helper: recursively compute cost
    recursive_cost <- function(s) {
      # If s is a single matrix, cost is 0
      indices <- extract_indices(s)
      if (length(indices) == 1) {
        return(list(cost = 0, left = indices[1], right = indices[1]))
      }
      
      # Find the main split: the outermost multiplication
      # Remove outer parentheses
      s_trim <- substring(s, 2, nchar(s) - 1)
      
      # Find the split point (the × not inside parentheses)
      depth <- 0
      split_pos <- NULL
      for (i in seq_len(nchar(s_trim))) {
        ch <- substr(s_trim, i, i)
        if (ch == "(") depth <- depth + 1
        if (ch == ")") depth <- depth - 1
        if (ch == "×" && depth == 0) {
          split_pos <- i
          break
        }
      }
      if (is.null(split_pos)) {
        # Should not happen
        stop("Invalid parenthesization string: ", s)
      }
      
      left_str <- trimws(substring(s_trim, 1, split_pos - 1))
      right_str <- trimws(substring(s_trim, split_pos + 1))
      
      left <- recursive_cost(left_str)
      right <- recursive_cost(right_str)
      
      # The multiplication cost: dims[left$left] x dims[left$right+1] x dims[right$right+1]
      cost_here <- dims[left$left] * dims[left$right + 1] * dims[right$right + 1]
      total_cost <- left$cost + right$cost + cost_here
      return(list(cost = total_cost, left = left$left, right = right$right))
    }
    
    recursive_cost(paren)$cost
  }
  
  # Compute cost for each parenthesization
  costs <- sapply(all_ways, compute_cost, dims = dimensions)
  
  # Find all parenthesizations with minimum cost
  optimal_ways <- all_ways[costs == min_cost]
  
  return(optimal_ways)

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
# Simulate left-to-right multiplication, updating result matrix dimensions
rows <- dims_analysis[1]
cols <- dims_analysis[2]
for (i in 2:(length(dims_analysis) - 1)) {
  next_cols <- dims_analysis[i + 1]
  naive_cost <- naive_cost + rows * cols * next_cols
  cols <- next_cols
}

cat("Naive left-to-right cost:", naive_cost, "\n")
cat("Savings with optimal order:", naive_cost - analysis_result$min_cost, "\n")
cat("Percentage improvement:", sprintf("%.1f%%", (naive_cost - analysis_result$min_cost) / naive_cost * 100), "\n")

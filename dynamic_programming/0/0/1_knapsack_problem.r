# 0/1 Knapsack Problem (Dynamic Programming)
#
# The 0/1 Knapsack problem is one of the most classic problems in dynamic programming.
# Given a set of items, each with a weight and a value, determine the maximum total value
# you can obtain by putting items in a knapsack with a fixed capacity. Each item can
# either be included (1) or excluded (0) — hence the name "0/1".
#
# Time Complexity: O(n * W)  where n = number of items, W = knapsack capacity
# Space Complexity: O(n * W)  for full DP table, O(W) for optimized version
#
# Applications:
# - Budget allocation problems
# - Resource optimization (CPU scheduling, project selection)
# - Portfolio selection in finance
# - Cargo loading and packing
# - Subset optimization in AI planning

# Classic DP solution for 0/1 Knapsack
knapsack_01 <- function(weights, values, capacity) {
  #' Solve 0/1 Knapsack Problem using Dynamic Programming
  #' @param weights: Numeric vector of item weights
  #' @param values: Numeric vector of item values
  #' @param capacity: Maximum weight capacity of the knapsack
  #' @return: List containing max value, selected items, and DP table
  
  n <- length(values)
  
  # Handle edge case
  if (n == 0 || capacity == 0) {
    return(list(
      max_value = 0,
      selected_items = c(),
      dp_table = matrix(0, nrow = n + 1, ncol = capacity + 1)
    ))
  }
  
  # Create DP table: dp[i, w] = max value using first i items with capacity w
  dp <- matrix(0, nrow = n + 1, ncol = capacity + 1)
  
  # Fill DP table
  for (i in 1:n) {
    for (w in 0:capacity) {
      # Don't include item i
      dp[i + 1, w + 1] <- dp[i, w + 1]
      
      # Include item i (if it fits)
      if (weights[i] <= w) {
        include_value <- values[i] + dp[i, w - weights[i] + 1]
        dp[i + 1, w + 1] <- max(dp[i + 1, w + 1], include_value)
      }
    }
  }
  
  # Backtrack to find selected items
  selected <- c()
  i <- n
  w <- capacity
  
  while (i > 0 && w > 0) {
    # If value came from including item i
    if (dp[i + 1, w + 1] != dp[i, w + 1]) {
      selected <- c(i, selected)
      w <- w - weights[i]
    }
    i <- i - 1
  }
  
  return(list(
    max_value = dp[n + 1, capacity + 1],
    selected_items = selected,
    dp_table = dp
  ))
}

# Space-optimized version using only 1D array
knapsack_01_optimized <- function(weights, values, capacity) {
  #' Space optimized 0/1 Knapsack using 1D array
  #' @return: Maximum total value
  
  n <- length(values)
  
  if (n == 0 || capacity == 0) {
    return(0)
  }
  
  dp <- rep(0, capacity + 1)
  
  # Process each item
  for (i in 1:n) {
    # Traverse from right to left to avoid overwriting needed values
    for (w in capacity:weights[i]) {
      if (weights[i] <= w) {
        dp[w + 1] <- max(dp[w + 1], values[i] + dp[w - weights[i] + 1])
      }
    }
  }
  
  return(dp[capacity + 1])
}

# Helper function to print DP table
print_knapsack_dp <- function(dp_table, weights, values, capacity) {
  cat("DP Table for 0/1 Knapsack:\n")
  cat("Weights:", paste(weights, collapse = ", "), "\n")
  cat("Values :", paste(values, collapse = ", "), "\n")
  cat("Capacity:", capacity, "\n\n")
  
  # Print capacity headers
  cat("        ")
  cat(paste(sprintf("%3d", 0:capacity), collapse = " "))
  cat("\n")
  cat(paste(rep("-", 8 + 4 * (capacity + 1)), collapse = ""), "\n")
  
  for (i in 1:nrow(dp_table)) {
    cat(sprintf("Item %2d | ", i - 1))
    cat(paste(sprintf("%3d", dp_table[i, ]), collapse = " "))
    cat("\n")
  }
  cat("\n")
}

# ===========================
# Example Usage & Testing
# ===========================
cat("=== 0/1 Knapsack Problem (Dynamic Programming) ===\n\n")

# Test 1: Basic Example
weights <- c(1, 3, 4, 5)
values <- c(1, 4, 5, 7)
capacity <- 7

cat("Test 1: Basic Example\n")
cat("Weights:", paste(weights, collapse = ", "), "\n")
cat("Values :", paste(values, collapse = ", "), "\n")
cat("Capacity:", capacity, "\n\n")

result <- knapsack_01(weights, values, capacity)
print_knapsack_dp(result$dp_table, weights, values, capacity)
cat("Maximum Value:", result$max_value, "\n")
cat("Selected Item Indices:", paste(result$selected_items, collapse = ", "), "\n")
cat("Total Weight:", sum(weights[result$selected_items]), "\n")
cat("Total Value:", sum(values[result$selected_items]), "\n\n")

# Test 2: Space Optimized Example
cat("Test 2: Space Optimized Version\n")
max_val_opt <- knapsack_01_optimized(weights, values, capacity)
cat("Maximum Value (Optimized):", max_val_opt, "\n")
cat("Verification: Both methods match:", result$max_value == max_val_opt, "\n\n")

# Test 3: Larger Dataset
cat("Test 3: Larger Dataset\n")
set.seed(42)
weights <- sample(1:15, 10)
values <- sample(10:100, 10)
capacity <- 35

cat("Weights:", paste(weights, collapse = ", "), "\n")
cat("Values :", paste(values, collapse = ", "), "\n")
cat("Capacity:", capacity, "\n\n")

large_result <- knapsack_01(weights, values, capacity)
cat("Maximum Value:", large_result$max_value, "\n")
cat("Selected Items:", paste(large_result$selected_items, collapse = ", "), "\n")
cat("Total Weight:", sum(weights[large_result$selected_items]), "\n\n")

# Test 4: Edge Cases
cat("Test 4: Edge Cases\n")
cat("Empty items:", knapsack_01(c(), c(), 10)$max_value, "\n")
cat("Zero capacity:", knapsack_01(weights, values, 0)$max_value, "\n")
cat("Single item fits:", knapsack_01(c(5), c(10), 10)$max_value, "\n")
cat("Single item doesn't fit:", knapsack_01(c(10), c(10), 5)$max_value, "\n\n")

# Test 5: Performance Check
cat("Test 5: Performance Comparison (n=100)\n")
n <- 100
weights <- sample(1:15, n, replace = TRUE)
values <- sample(10:100, n, replace = TRUE)
capacity <- 200

start_time <- Sys.time()
res_std <- knapsack_01_optimized(weights, values, capacity)
std_time <- as.numeric(Sys.time() - start_time, units = "secs")

cat("Optimized DP result:", res_std, "\n")
cat("Time taken:", sprintf("%.4f sec", std_time), "\n")

# Verify correctness
cat("\nVerifying correctness with full DP:\n")
start_time <- Sys.time()
res_full <- knapsack_01(weights, values, capacity)
full_time <- as.numeric(Sys.time() - start_time, units = "secs")
cat("Full DP result:", res_full$max_value, "\n")
cat("Time taken:", sprintf("%.4f sec", full_time), "\n")
cat("Results match:", res_std == res_full$max_value, "\n")
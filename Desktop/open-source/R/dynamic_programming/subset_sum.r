# Subset Sum Problem
#
# The Subset Sum problem determines whether there exists a subset of a given set
# of positive integers that sums to a target value. This is a classic NP-complete
# problem solved using dynamic programming.
#
# Time Complexity: O(n * sum) where n = number of elements, sum = target sum
# Space Complexity: O(n * sum) for DP table, O(sum) for optimized version
#
# Applications:
# - Partition problems in computer science
# - Knapsack problem variations
# - Resource allocation and optimization
# - Cryptography and number theory
# - Game theory and decision making

# Basic DP solution for Subset Sum Problem
subset_sum <- function(arr, target) {
  #' Check if there exists a subset that sums to the target value
  #' @param arr: Numeric vector of positive integers
  #' @param target: Target sum value
  #' @return: Boolean indicating if subset exists, along with DP table
  
  n <- length(arr)
  
  # Handle edge cases
  if (n == 0) {
    return(list(
      exists = (target == 0),
      dp_table = matrix(FALSE, nrow = 1, ncol = 1),
      subset = c()
    ))
  }
  
  if (target == 0) {
    return(list(
      exists = TRUE,
      dp_table = matrix(TRUE, nrow = n + 1, ncol = 1),
      subset = c()
    ))
  }
  
  # Create DP table: dp[i, j] = TRUE if sum j can be achieved using first i elements
  dp <- matrix(FALSE, nrow = n + 1, ncol = target + 1)
  
  # Base case: sum 0 can always be achieved with empty subset
  for (i in 1:(n + 1)) {
    dp[i, 1] <- TRUE
  }
  
  # Fill DP table
  for (i in 2:(n + 1)) {
    for (j in 1:(target + 1)) {
      current_sum <- j - 1  # Convert to 0-based indexing
      
      # Don't include current element
      dp[i, j] <- dp[i - 1, j]
      
      # Include current element (if it doesn't exceed current sum)
      if (arr[i - 1] <= current_sum) {
        dp[i, j] <- dp[i, j] || dp[i - 1, j - arr[i - 1]]
      }
    }
  }
  
  # Backtrack to find one possible subset
  subset <- c()
  if (dp[n + 1, target + 1]) {
    i <- n + 1
    j <- target + 1
    
    while (i > 1 && j > 1) {
      # If current sum was achieved without including arr[i-1]
      if (dp[i - 1, j]) {
        i <- i - 1
      } else {
        # Current element was included
        subset <- c(arr[i - 1], subset)
        j <- j - arr[i - 1]
        i <- i - 1
      }
    }
  }
  
  return(list(
    exists = dp[n + 1, target + 1],
    dp_table = dp,
    subset = subset
  ))
}

# Space-optimized version using only 1D array
subset_sum_optimized <- function(arr, target) {
  #' Space optimized subset sum using 1D array
  #' @param arr: Numeric vector of positive integers
  #' @param target: Target sum value
  #' @return: Boolean indicating if subset exists
  
  n <- length(arr)
  
  if (n == 0) return(target == 0)
  if (target == 0) return(TRUE)
  
  dp <- rep(FALSE, target + 1)
  dp[1] <- TRUE  # sum 0 is always possible
  
  for (i in 1:n) {
    # Traverse from right to left to avoid overwriting needed values
    for (j in target:1) {
      if (arr[i] <= j) {
        dp[j + 1] <- dp[j + 1] || dp[j - arr[i] + 1]
      }
    }
  }
  
  return(dp[target + 1])
}

# Function to find all subsets that sum to target
find_all_subsets <- function(arr, target) {
  #' Find all subsets that sum to the target value
  #' @param arr: Numeric vector of positive integers
  #' @param target: Target sum value
  #' @return: List of subsets (each subset is a numeric vector) that sum to target
  
  n <- length(arr)
  results <- list()
  
  # Helper recursive function
  find_subsets_rec <- function(idx, current_subset, current_sum) {
    if (current_sum == target) {
      results <<- c(results, list(current_subset))
      return()
    }
    if (idx > n || current_sum > target) {
      return()
    }
    # Include arr[idx]
    find_subsets_rec(idx + 1, c(current_subset, arr[idx]), current_sum + arr[idx])
    # Exclude arr[idx]
    find_subsets_rec(idx + 1, current_subset, current_sum)
  }
  
  find_subsets_rec(1, c(), 0)
  return(results)
}

# Helper function to print DP table
print_subset_sum_dp <- function(dp_table, arr, target) {
  cat("DP Table for Subset Sum Problem:\n")
  cat("Array:", paste(arr, collapse = ", "), "\n")
  cat("Target Sum:", target, "\n\n")
  
  # Print column headers (sums)
  cat("        ")
  cat(paste(sprintf("%4d", 0:target), collapse = " "))
  cat("\n")
  cat(paste(rep("-", 8 + 5 * (target + 1)), collapse = ""), "\n")
  
  for (i in 1:nrow(dp_table)) {
    if (i == 1) {
      cat("Empty  | ")
    } else {
      cat(sprintf("Elem%2d| ", i - 1))
    }
    
    for (j in 1:ncol(dp_table)) {
      cat(sprintf("%4s", ifelse(dp_table[i, j], " T", " F")))
    }
    cat("\n")
  }
  cat("\n")
}

# ===========================
# Example Usage & Testing
# ===========================
cat("=== Subset Sum Problem (Dynamic Programming) ===\n\n")

# Test 1: Basic Example
arr1 <- c(3, 34, 4, 12, 5, 2)
target1 <- 9
cat("Test 1: Basic Example\n")
cat("Array:", paste(arr1, collapse = ", "), "\n")
cat("Target Sum:", target1, "\n\n")

result1 <- subset_sum(arr1, target1)
print_subset_sum_dp(result1$dp_table, arr1, target1)
cat("Subset exists:", result1$exists, "\n")
if (result1$exists) {
  cat("One possible subset:", paste(result1$subset, collapse = ", "), "\n")
  cat("Sum verification:", sum(result1$subset), "\n")
}
cat("\n")

# Test 2: Optimized Version
cat("Test 2: Space Optimized Version\n")
exists_opt <- subset_sum_optimized(arr1, target1)
cat("Subset exists (Optimized):", exists_opt, "\n")
cat("Verification: Both methods match:", result1$exists == exists_opt, "\n\n")

# Test 3: No Solution Case
cat("Test 3: No Solution Case\n")
arr3 <- c(3, 34, 4, 12, 5, 2)
target3 <- 30
cat("Array:", paste(arr3, collapse = ", "), "\n")
cat("Target Sum:", target3, "\n")

result3 <- subset_sum(arr3, target3)
cat("Subset exists:", result3$exists, "\n\n")

# Test 4: Multiple Solutions
cat("Test 4: Multiple Solutions\n")
arr4 <- c(1, 2, 3, 4, 5)
target4 <- 6
cat("Array:", paste(arr4, collapse = ", "), "\n")
cat("Target Sum:", target4, "\n")

result4 <- subset_sum(arr4, target4)
cat("Subset exists:", result4$exists, "\n")
if (result4$exists) {
  cat("One possible subset:", paste(result4$subset, collapse = ", "), "\n")
  
  # Find all possible subsets
  all_subsets <- find_all_subsets(arr4, target4)
  cat("Total number of subsets:", length(all_subsets), "\n")
  for (i in seq_along(all_subsets)) {
    cat("Subset", i, ":", paste(all_subsets[[i]], collapse = ", "), 
        "(sum =", sum(all_subsets[[i]]), ")\n")
  }
}
cat("\n")

# Test 5: Edge Cases
cat("Test 5: Edge Cases\n")
cat("Empty array, target 0:", subset_sum(c(), 0)$exists, "\n")
cat("Empty array, target 5:", subset_sum(c(), 5)$exists, "\n")
cat("Array [1,2,3], target 0:", subset_sum(c(1, 2, 3), 0)$exists, "\n")
cat("Array [5], target 5:", subset_sum(c(5), 5)$exists, "\n")
cat("Array [5], target 3:", subset_sum(c(5), 3)$exists, "\n\n")

# Test 6: Larger Dataset
cat("Test 6: Larger Dataset (n=15)\n")
set.seed(42)
arr_large <- sample(1:20, 15)
target_large <- 50
cat("Array:", paste(arr_large, collapse = ", "), "\n")
cat("Target Sum:", target_large, "\n")

start_time <- Sys.time()
result_large <- subset_sum(arr_large, target_large)
dp_time <- as.numeric(Sys.time() - start_time, units = "secs")

start_time <- Sys.time()
exists_large_opt <- subset_sum_optimized(arr_large, target_large)
opt_time <- as.numeric(Sys.time() - start_time, units = "secs")

cat("Subset exists:", result_large$exists, "\n")
cat("DP method time:", sprintf("%.4f sec", dp_time), "\n")
cat("Optimized method time:", sprintf("%.4f sec", opt_time), "\n")
cat("Results match:", result_large$exists == exists_large_opt, "\n")

if (result_large$exists) {
  cat("One possible subset:", paste(result_large$subset, collapse = ", "), "\n")
  cat("Sum verification:", sum(result_large$subset), "\n")
}
cat("\n")

# Test 7: Real-world Example - Budget Allocation
cat("Test 7: Real-world Example - Budget Allocation\n")
project_costs <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
budget <- 150
cat("Project costs:", paste(project_costs, collapse = ", "), "\n")
cat("Available budget:", budget, "\n")

budget_result <- subset_sum(project_costs, budget)
cat("Exact budget allocation possible:", budget_result$exists, "\n")

if (budget_result$exists) {
  selected_projects <- budget_result$subset
  cat("Selected projects (costs):", paste(selected_projects, collapse = ", "), "\n")
  cat("Total cost:", sum(selected_projects), "\n")
  cat("Remaining budget:", budget - sum(selected_projects), "\n")
} else {
  # Find closest possible sum (≤ budget) in a single pass
  closest_sum <- max_subset_sum_leq(project_costs, budget)
  cat("Closest possible sum:", closest_sum, "\n")
}

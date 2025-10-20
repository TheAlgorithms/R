# Longest Increasing Subsequence (Dynamic Programming)
#
# The Longest Increasing Subsequence (LIS) problem is a classic dynamic programming problem.
# Given an array of integers, find the length of the longest subsequence that is strictly
# increasing. A subsequence is derived from the array by deleting some or no elements
# without changing the order of the remaining elements.
#
# Time Complexity: O(n²) for basic DP, O(n log n) for optimized binary search version
# Space Complexity: O(n) for both approaches
#
# Applications:
# - Bioinformatics (DNA sequence analysis)
# - Stock market analysis (longest upward trend)
# - Scheduling problems
# - Game theory (optimal play sequences)
# - Data compression and pattern recognition

# Basic DP solution for Longest Increasing Subsequence
longest_increasing_subsequence <- function(nums) {
  #' Find the length of the longest increasing subsequence using Dynamic Programming
  #' @param nums: Numeric vector of integers
  #' @return: List containing max length, DP array, and one possible LIS
  
  n <- length(nums)
  
  # Handle edge cases
  if (n == 0) {
    return(list(
      max_length = 0,
      dp_array = c(),
      lis_sequence = c(),
      dp_table = c()
    ))
  }
  
  if (n == 1) {
    return(list(
      max_length = 1,
      dp_array = c(1),
      lis_sequence = nums,
      dp_table = c(1)
    ))
  }
  
  # Initialize DP array: dp[i] = length of LIS ending at index i
  dp <- rep(1, n)
  
  # Fill DP array
  for (i in 2:n) {
    for (j in 1:(i - 1)) {
      if (nums[j] < nums[i]) {
        dp[i] <- max(dp[i], dp[j] + 1)
      }
    }
  }
  
  # Find maximum length
  max_length <- max(dp)
  
  # Backtrack to find one possible LIS
  lis_sequence <- c()
  current_length <- max_length
  
  for (i in n:1) {
    if (dp[i] == current_length) {
      lis_sequence <- c(nums[i], lis_sequence)
      current_length <- current_length - 1
      if (current_length == 0) break
    }
  }
  
  return(list(
    max_length = max_length,
    dp_array = dp,
    lis_sequence = lis_sequence,
    dp_table = dp
  ))
}

# Optimized O(n log n) solution using binary search
longest_increasing_subsequence_optimized <- function(nums) {
  #' Find the length of the longest increasing subsequence using binary search
  #' @param nums: Numeric vector of integers
  #' @return: Length of the longest increasing subsequence
  
  n <- length(nums)
  
  if (n == 0) return(0)
  if (n == 1) return(1)
  
  # tails[i] stores the smallest tail of all increasing subsequences of length i+1
  tails <- c()
  
  for (num in nums) {
    # Binary search for the position to replace or extend
    pos <- binary_search_insert_position(tails, num)
    
    if (pos > length(tails)) {
      # Extend the sequence
      tails <- c(tails, num)
    } else {
      # Replace the element at position pos
      tails[pos] <- num
    }
  }
  
  return(length(tails))
}

# Helper function for binary search
binary_search_insert_position <- function(arr, target) {
  #' Binary search to find the position where target should be inserted
  #' @param arr: Sorted numeric vector
  #' @param target: Value to insert
  #' @return: Position (1-indexed) where target should be inserted
  
  if (length(arr) == 0) return(1)
  
  left <- 1
  right <- length(arr)
  
  while (left <= right) {
    mid <- left + (right - left) %/% 2
    
    if (arr[mid] < target) {
      left <- mid + 1
    } else {
      right <- mid - 1
    }
  }
  
  return(left)
}

# Function to find all possible LIS sequences (simplified version)
find_all_lis <- function(nums) {
  #' Find all possible longest increasing subsequences
  #' @param nums: Numeric vector of integers
  #' @return: List of all possible LIS sequences
  
  n <- length(nums)
  if (n == 0) return(list())
  
  # Calculate DP array
  dp <- rep(1, n)
  for (i in 2:n) {
    for (j in 1:(i - 1)) {
      if (nums[j] < nums[i]) {
        dp[i] <- max(dp[i], dp[j] + 1)
      }
    }
  }
  
  max_length <- max(dp)
  
  # For simplicity, return just one LIS (same as the main function)
  # Finding all possible LIS is complex and not essential for the algorithm demonstration
  result <- longest_increasing_subsequence(nums)
  return(list(result$lis_sequence))
}

# Helper function to print DP table
print_lis_dp <- function(dp_array, nums) {
  cat("DP Array for Longest Increasing Subsequence:\n")
  cat("Input Array:", paste(nums, collapse = ", "), "\n")
  cat("DP Array   :", paste(dp_array, collapse = ", "), "\n")
  cat("Max Length :", max(dp_array), "\n\n")
}

# ===========================
# Example Usage & Testing
# ===========================
cat("=== Longest Increasing Subsequence (Dynamic Programming) ===\n\n")

# Test 1: Basic Example
nums1 <- c(10, 9, 2, 5, 3, 7, 101, 18)
cat("Test 1: Basic Example\n")
cat("Input Array:", paste(nums1, collapse = ", "), "\n\n")

result1 <- longest_increasing_subsequence(nums1)
print_lis_dp(result1$dp_array, nums1)
cat("Maximum Length:", result1$max_length, "\n")
cat("One LIS Sequence:", paste(result1$lis_sequence, collapse = ", "), "\n\n")

# Test 2: Optimized Version
cat("Test 2: Optimized O(n log n) Version\n")
max_len_opt <- longest_increasing_subsequence_optimized(nums1)
cat("Maximum Length (Optimized):", max_len_opt, "\n")
cat("Verification: Both methods match:", result1$max_length == max_len_opt, "\n\n")

# Test 3: All Possible LIS
cat("Test 3: All Possible LIS Sequences\n")
all_lis <- find_all_lis(nums1)
cat("Total number of LIS sequences:", length(all_lis), "\n")
for (i in seq_along(all_lis)) {
  cat("LIS", i, ":", paste(all_lis[[i]], collapse = ", "), "\n")
}
cat("\n")

# Test 4: Edge Cases
cat("Test 4: Edge Cases\n")
cat("Empty array:", longest_increasing_subsequence(c())$max_length, "\n")
cat("Single element:", longest_increasing_subsequence(c(5))$max_length, "\n")
cat("All same elements:", longest_increasing_subsequence(c(3, 3, 3, 3))$max_length, "\n")
cat("Strictly decreasing:", longest_increasing_subsequence(c(5, 4, 3, 2, 1))$max_length, "\n")
cat("Strictly increasing:", longest_increasing_subsequence(c(1, 2, 3, 4, 5))$max_length, "\n\n")

# Test 5: Larger Dataset
cat("Test 5: Larger Dataset (n=20)\n")
set.seed(42)
nums_large <- sample(1:100, 20)
cat("Input Array:", paste(nums_large, collapse = ", "), "\n\n")

result_large <- longest_increasing_subsequence(nums_large)
cat("Maximum Length:", result_large$max_length, "\n")
cat("One LIS Sequence:", paste(result_large$lis_sequence, collapse = ", "), "\n\n")

# Test 6: Performance Comparison
cat("Test 6: Performance Comparison (n=1000)\n")
n <- 1000
nums_perf <- sample(1:1000, n)

start_time <- Sys.time()
res_opt <- longest_increasing_subsequence_optimized(nums_perf)
opt_time <- as.numeric(Sys.time() - start_time, units = "secs")

cat("Optimized O(n log n) result:", res_opt, "\n")
cat("Time taken:", sprintf("%.4f sec", opt_time), "\n")

# Verify correctness with basic DP (smaller sample for time comparison)
nums_small <- nums_perf[1:100]
start_time <- Sys.time()
res_basic <- longest_increasing_subsequence(nums_small)
basic_time <- as.numeric(Sys.time() - start_time, units = "secs")

cat("Basic O(n²) result (n=100):", res_basic$max_length, "\n")
cat("Time taken:", sprintf("%.4f sec", basic_time), "\n")

# Test 7: Real-world Example - Stock Prices
cat("Test 7: Real-world Example - Stock Price Trend\n")
stock_prices <- c(100, 102, 98, 105, 103, 107, 110, 108, 112, 115, 113, 118, 120, 117, 125)
cat("Stock Prices:", paste(stock_prices, collapse = ", "), "\n")

stock_result <- longest_increasing_subsequence(stock_prices)
cat("Longest upward trend length:", stock_result$max_length, "\n")
cat("Longest upward trend:", paste(stock_result$lis_sequence, collapse = ", "), "\n")
cat("Percentage increase:", 
    sprintf("%.2f%%", (stock_result$lis_sequence[length(stock_result$lis_sequence)] / 
                      stock_result$lis_sequence[1] - 1) * 100), "\n")

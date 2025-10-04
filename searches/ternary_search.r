# Ternary Search Algorithm
#
# Ternary search is a divide-and-conquer algorithm used to find the maximum or minimum
# of a unimodal function, or to search for a value in a sorted array.
# It divides the search space into three parts instead of two (like binary search).
#
# Time Complexity: O(log₃ n) ≈ O(log n) 
# Space Complexity: O(1) for iterative, O(log n) for recursive
#
# Applications:
# - Finding maximum/minimum of unimodal functions
# - Optimization problems in competitive programming
# - Searching in arrays (alternative to binary search)

# Ternary search for finding maximum of a unimodal function
ternary_search_maximum <- function(func, left, right, precision = 1e-9) {
  #' Find the maximum of a unimodal function using ternary search
  #' @param func: Function to find maximum of (must be unimodal)
  #' @param left: Left boundary of search interval
  #' @param right: Right boundary of search interval  
  #' @param precision: Desired precision for the result
  #' @return: x-coordinate where function achieves maximum
  
  while (right - left > precision) {
    # Divide interval into three parts
    mid1 <- left + (right - left) / 3
    mid2 <- right - (right - left) / 3
    
    # Compare function values at the two middle points
    if (func(mid1) < func(mid2)) {
      # Maximum is in the right part [mid1, right]
      left <- mid1
    } else {
      # Maximum is in the left part [left, mid2]
      right <- mid2
    }
  }
  
  # Return the midpoint of final interval
  return((left + right) / 2)
}

# Ternary search for finding minimum of a unimodal function
ternary_search_minimum <- function(func, left, right, precision = 1e-9) {
  #' Find the minimum of a unimodal function using ternary search
  #' @param func: Function to find minimum of (must be unimodal)
  #' @param left: Left boundary of search interval
  #' @param right: Right boundary of search interval
  #' @param precision: Desired precision for the result
  #' @return: x-coordinate where function achieves minimum
  
  while (right - left > precision) {
    # Divide interval into three parts
    mid1 <- left + (right - left) / 3
    mid2 <- right - (right - left) / 3
    
    # Compare function values at the two middle points
    if (func(mid1) > func(mid2)) {
      # Minimum is in the right part [mid1, right]
      left <- mid1
    } else {
      # Minimum is in the left part [left, mid2]
      right <- mid2
    }
  }
  
  # Return the midpoint of final interval
  return((left + right) / 2)
}

# Ternary search on sorted arrays (recursive implementation)
ternary_search_array_recursive <- function(arr, target, left = 1, right = length(arr)) {
  #' Search for target in sorted array using recursive ternary search
  #' @param arr: Sorted array to search in
  #' @param target: Value to search for
  #' @param left: Left index of search range
  #' @param right: Right index of search range
  #' @return: Index of target if found, -1 otherwise
  
  if (left > right) {
    return(-1)  # Target not found
  }
  
  # Calculate two middle points
  mid1 <- left + floor((right - left) / 3)
  mid2 <- right - floor((right - left) / 3)
  
  # Check if target is at either middle point
  if (arr[mid1] == target) {
    return(mid1)
  }
  if (arr[mid2] == target) {
    return(mid2)
  }
  
  # Determine which third to search
  if (target < arr[mid1]) {
    # Search in first third
    return(ternary_search_array_recursive(arr, target, left, mid1 - 1))
  } else if (target > arr[mid2]) {
    # Search in last third
    return(ternary_search_array_recursive(arr, target, mid2 + 1, right))
  } else {
    # Search in middle third
    return(ternary_search_array_recursive(arr, target, mid1 + 1, mid2 - 1))
  }
}

# Ternary search on sorted arrays (iterative implementation)
ternary_search_array_iterative <- function(arr, target) {
  #' Search for target in sorted array using iterative ternary search
  #' @param arr: Sorted array to search in
  #' @param target: Value to search for
  #' @return: Index of target if found, -1 otherwise
  
  left <- 1
  right <- length(arr)
  
  while (left <= right) {
    # Calculate two middle points
    mid1 <- left + floor((right - left) / 3)
    mid2 <- right - floor((right - left) / 3)
    
    # Check if target is at either middle point
    if (arr[mid1] == target) {
      return(mid1)
    }
    if (arr[mid2] == target) {
      return(mid2)
    }
    
    # Determine which third to search
    if (target < arr[mid1]) {
      # Search in first third
      right <- mid1 - 1
    } else if (target > arr[mid2]) {
      # Search in last third
      left <- mid2 + 1
    } else {
      # Search in middle third
      left <- mid1 + 1
      right <- mid2 - 1
    }
  }
  
  return(-1)  # Target not found
}

# Helper function to compare ternary search with binary search
binary_search_comparison <- function(arr, target) {
  #' Binary search implementation for comparison
  left <- 1
  right <- length(arr)
  comparisons <- 0
  
  while (left <= right) {
    comparisons <- comparisons + 1
    mid <- left + floor((right - left) / 2)
    
    if (arr[mid] == target) {
      return(list(index = mid, comparisons = comparisons))
    } else if (arr[mid] < target) {
      left <- mid + 1
    } else {
      right <- mid - 1
    }
  }
  
  return(list(index = -1, comparisons = comparisons))
}

# Function to count comparisons in ternary search
ternary_search_with_count <- function(arr, target) {
  #' Ternary search with comparison counting
  left <- 1
  right <- length(arr)
  comparisons <- 0
  
  while (left <= right) {
    comparisons <- comparisons + 2  # Two comparisons per iteration
    
    mid1 <- left + floor((right - left) / 3)
    mid2 <- right - floor((right - left) / 3)
    
    if (arr[mid1] == target) {
      return(list(index = mid1, comparisons = comparisons))
    }
    if (arr[mid2] == target) {
      return(list(index = mid2, comparisons = comparisons))
    }
    
    if (target < arr[mid1]) {
      right <- mid1 - 1
    } else if (target > arr[mid2]) {
      left <- mid2 + 1
    } else {
      left <- mid1 + 1
      right <- mid2 - 1
    }
  }
  
  return(list(index = -1, comparisons = comparisons))
}

# Example usage and testing
cat("=== Ternary Search Algorithm ===\n")

# Test 1: Finding maximum of a quadratic function
cat("1. Finding Maximum of Unimodal Function\n")
quadratic_func <- function(x) -(x - 3)^2 + 10  # Maximum at x = 3
max_x <- ternary_search_maximum(quadratic_func, 0, 6)
cat("Function: f(x) = -(x-3)² + 10\n")
cat("Maximum found at x =", round(max_x, 6), "\n")
cat("Function value at maximum:", quadratic_func(max_x), "\n")
cat("Expected maximum at x = 3\n\n")

# Test 2: Finding minimum of a quadratic function  
cat("2. Finding Minimum of Unimodal Function\n")
quadratic_min <- function(x) (x - 2)^2 + 5  # Minimum at x = 2
min_x <- ternary_search_minimum(quadratic_min, -1, 5)
cat("Function: f(x) = (x-2)² + 5\n")
cat("Minimum found at x =", round(min_x, 6), "\n")
cat("Function value at minimum:", quadratic_min(min_x), "\n")
cat("Expected minimum at x = 2\n\n")

# Test 3: Ternary search on sorted array
cat("3. Ternary Search on Sorted Array\n")
sorted_array <- c(1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25)
target <- 13

cat("Array:", paste(sorted_array, collapse = ", "), "\n")
cat("Searching for:", target, "\n")

# Recursive version
result_recursive <- ternary_search_array_recursive(sorted_array, target)
cat("Recursive result: Index", result_recursive, "\n")

# Iterative version
result_iterative <- ternary_search_array_iterative(sorted_array, target)
cat("Iterative result: Index", result_iterative, "\n")

# Verify the result
if (result_iterative != -1) {
  cat("Value at found index:", sorted_array[result_iterative], "\n")
}

# Test 4: Performance comparison
cat("\n4. Performance Comparison: Ternary vs Binary Search\n")
large_array <- seq(1, 1000, by = 2)  # Odd numbers from 1 to 999
search_target <- 501

# Ternary search
ternary_result <- ternary_search_with_count(large_array, search_target)
# Binary search  
binary_result <- binary_search_comparison(large_array, search_target)

cat("Array size:", length(large_array), "\n")
cat("Searching for:", search_target, "\n")
cat("Ternary search: Found at index", ternary_result$index, 
    "with", ternary_result$comparisons, "comparisons\n")
cat("Binary search:  Found at index", binary_result$index, 
    "with", binary_result$comparisons, "comparisons\n")

# Test 5: Edge cases
cat("\n5. Edge Cases\n")
small_array <- c(10, 20, 30)

cat("Small array:", paste(small_array, collapse = ", "), "\n")
cat("Search for 20:", ternary_search_array_iterative(small_array, 20), "\n")
cat("Search for 15 (not present):", ternary_search_array_iterative(small_array, 15), "\n")
cat("Search for 5 (smaller than all):", ternary_search_array_iterative(small_array, 5), "\n")
cat("Search for 35 (larger than all):", ternary_search_array_iterative(small_array, 35), "\n")

# Test 6: Real optimization example
cat("\n6. Real Optimization Example\n")
cat("Finding optimal production quantity to minimize cost\n")
# Cost function: fixed cost + variable cost - economies of scale
cost_function <- function(x) 100 + 2*x + 0.001*x^2 - 0.1*x*log(x + 1)
optimal_quantity <- ternary_search_minimum(cost_function, 1, 1000)
cat("Optimal quantity:", round(optimal_quantity, 2), "\n")
cat("Minimum cost:", round(cost_function(optimal_quantity), 2), "\n")
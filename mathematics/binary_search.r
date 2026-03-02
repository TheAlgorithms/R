# Description:
# Binary Search is an efficient searching algorithm that works
# on sorted arrays. It repeatedly divides the search interval
# in half and compares the middle element with the target value.
#
# If the target is found, its index is returned.
# If the target does not exist, the function returns -1.
#
# Time Complexity:
#   Best Case: O(1)
#   Average Case: O(log n)
#   Worst Case: O(log n)
#
# Space Complexity:
#   O(1) - iterative approach
#
# Requirements:
#   The input vector MUST be sorted in ascending order.
# -------------------------------------------------------------


binary_search <- function(arr, target) {
  
  # -------------------------------
  # Input validation
  # -------------------------------
  if (!is.numeric(arr)) {
    stop("Error: Input array must be numeric.")
  }
  
  if (length(arr) == 0) {
    return(-1)
  }
  
  # -------------------------------
  # Initialize search boundaries
  # -------------------------------
  left <- 1
  right <- length(arr)
  
  # -------------------------------
  # Iterative Binary Search
  # -------------------------------
  while (left <= right) {
    
    # Calculate midpoint safely
    mid <- floor((left + right) / 2)
    
    # Check if target found
    if (arr[mid] == target) {
      return(mid)
    }
    
    # If target is greater, ignore left half
    if (arr[mid] < target) {
      left <- mid + 1
    }
    
    # If target is smaller, ignore right half
    else {
      right <- mid - 1
    }
  }
  
  # Target not found
  return(-1)
}


# -------------------------------------------------------------
# Example Usage
# -------------------------------------------------------------

# Sorted numeric vector
numbers <- c(2, 5, 8, 12, 16, 23, 38, 56, 72, 91)

# Case 1: Target exists
target1 <- 23
result1 <- binary_search(numbers, target1)
cat("Index of", target1, ":", result1, "\n")

# Case 2: Target does NOT exist
target2 <- 50
result2 <- binary_search(numbers, target2)
cat("Index of", target2, ":", result2, "\n")


# -------------------------------------------------------------
# Additional Test Cases
# -------------------------------------------------------------

# Single element array
cat("Test single element:", binary_search(c(10), 10), "\n")

# Empty array
cat("Test empty array:", binary_search(c(), 5), "\n")

# Large array test
large_arr <- seq(1, 1000, by = 2)
cat("Large test:", binary_search(large_arr, 501), "\n")


# -------------------------------------------------------------
# Notes:
# - Binary search only works correctly on sorted arrays.
# - For unsorted data, sorting must be performed first.
# -------------------------------------------------------------

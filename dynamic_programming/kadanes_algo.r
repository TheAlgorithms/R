# Kadane's Algorithm in R
#
# Finds the contiguous subarray with the largest sum.
# Time Complexity: O(n)
# Space Complexity: O(1) (not counting output subarray)
#
# Applications:
# - Financial time series (max profit window)
# - Signal processing (max energy segment)
# - Pattern detection in sequences
# - As a subroutine in more complex DP/optimization tasks

kadane <- function(arr) {
  #' Kadane's algorithm to find maximum subarray sum and its indices
  #' @param arr: Numeric vector (can include negatives and positives)
  #' @return: A list with fields:
  #'         max_sum - numeric: maximum subarray sum
  #'         start   - integer: start index of the subarray (1-based), NA if empty input
  #'         end     - integer: end index of the subarray (1-based), NA if empty input
  #'         subarray- numeric vector: the subarray that gives max_sum (empty if input empty)
  
  n <- length(arr)
  
  # Edge cases
  if (n == 0) {
    return(list(
      max_sum = -Inf,
      start = NA_integer_,
      end = NA_integer_,
      subarray = numeric(0)
    ))
  }
  
  # Initialize with first element (handles all-negative arrays correctly)
  max_ending_here <- arr[1]
  max_so_far <- arr[1]
  s <- 1
  start <- 1
  end <- 1
  
  if (n >= 2) {
    for (i in 2:n) {
      # If adding arr[i] to current segment is worse than starting new at arr[i]
      if (max_ending_here + arr[i] < arr[i]) {
        max_ending_here <- arr[i]
        s <- i
      } else {
        max_ending_here <- max_ending_here + arr[i]
      }
      
      # Update best segment if needed
      if (max_ending_here > max_so_far) {
        max_so_far <- max_ending_here
        start <- s
        end <- i
      }
    }
  }
  
  return(list(
    max_sum = max_so_far,
    start = as.integer(start),
    end = as.integer(end),
    subarray = arr[start:end]
  ))
}

# Helper to pretty-print results
print_kadane_result <- function(res, arr_name="Array") {
  cat("Input:", arr_name, "\n")
  if (is.na(res$start)) {
    cat("Result: empty input\n\n")
    return(invisible(NULL))
  }
  cat("Max Subarray Sum:", res$max_sum, "\n")
  cat("Start Index:", res$start, " End Index:", res$end, "\n")
  cat("Subarray:", paste(res$subarray, collapse = ", "), "\n\n")
}

# ===========================
# Example Usage & Testing
# ===========================
cat("=== Kadane's Algorithm Tests ===\n\n")

# Test 1: Mixed positive and negative
arr1 <- c(-2, 1, -3, 4, -1, 2, 1, -5, 4)
res1 <- kadane(arr1)
print_kadane_result(res1, "arr1 (mixed)")

# Test 2: All positive
arr2 <- c(2, 3, 1, 4)
res2 <- kadane(arr2)
print_kadane_result(res2, "arr2 (all positive)")

# Test 3: All negative
arr3 <- c(-8, -3, -6, -2, -5, -4)
res3 <- kadane(arr3)
print_kadane_result(res3, "arr3 (all negative)")

# Test 4: Single element
arr4 <- c(5)
res4 <- kadane(arr4)
print_kadane_result(res4, "arr4 (single element)")

# Test 5: Empty array
arr5 <- numeric(0)
res5 <- kadane(arr5)
print_kadane_result(res5, "arr5 (empty)")

# Test 6: Random large array - timing example
set.seed(123)
arr6 <- sample(-100:100, 100000, replace = TRUE)
start_time <- Sys.time()
res6 <- kadane(arr6)
end_time <- Sys.time()
print_kadane_result(res6, "arr6 (large random)")
cat("Elapsed time (seconds):", as.numeric(end_time - start_time, units = "secs"), "\n\n")

# Optional: function to get maximum circular subarray (Kadane + total sum trick)
kadane_circular <- function(arr) {
  #' Finds max subarray sum for circular arrays (wrap-around allowed)
  #' If all elements are negative, returns max element (non-wrap).
  n <- length(arr)
  if (n == 0) return(list(max_sum = -Inf, start = NA, end = NA, subarray = numeric(0)))
  
  # Standard Kadane for non-circular max
  normal <- kadane(arr)$max_sum
  
  # If all negative, normal already is max element; circular logic would fail
  if (all(arr <= 0)) {
    return(list(max_sum = normal, start = which.max(arr), end = which.max(arr), subarray = arr[which.max(arr)]))
  }
  
  # Max wrap = total_sum - min_subarray_sum
  total_sum <- sum(arr)
  
  # Find minimum subarray using Kadane on inverted array
  inverted <- -arr
  min_sub_sum <- kadane(inverted)$max_sum  # this is -min_subarray_sum
  max_wrap <- total_sum + min_sub_sum      # because min_sub_sum is negative of min subarray
  
  if (max_wrap > normal) {
    return(list(max_sum = max_wrap, start = NA, end = NA, subarray = NA)) # indices for wrap-around not computed here
  } else {
    normal_result <- kadane(arr)
    return(list(max_sum = normal, start = normal_result$start, end = normal_result$end, subarray = normal_result$subarray))
  }
}

# Example for circular
cat("=== Circular Kadane Example ===\n")
arrc <- c(8, -1, 3, 4)
res_circ <- kadane_circular(arrc)
cat("Input:", paste(arrc, collapse = ", "), "\n")
cat("Max circular subarray sum:", res_circ$max_sum, "\n\n")

# End of script

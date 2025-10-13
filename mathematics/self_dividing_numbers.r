# Self-Dividing Numbers
#
# A self-dividing number is a number that is divisible by every digit it contains.
# Additionally, self-dividing numbers do not contain the digit 0 (since division by zero is undefined).
#
# Time Complexity: O(m * log₁₀(n)) where m = right - left + 1, and log₁₀(n) is the number of digits
# Space Complexity: O(m) for storing the result list
#
# Algorithm:
# 1. Iterate through all numbers in the given range [left, right]
# 2. For each number, check if it's self-dividing by:
#    - Extract each digit using modulo (%) and division (/)
#    - Check if digit is 0 or if the original number is not divisible by the digit
#    - If all digits divide the number, it's self-dividing
# 3. Collect all self-dividing numbers and return them

is_self_dividing_number <- function(num) {
  #' Check if a number is self-dividing
  #'
  #' @description Determines if a number is divisible by all of its digits.
  #' A self-dividing number cannot contain the digit 0.
  #' @param num A positive integer to check
  #' @return TRUE if the number is self-dividing, FALSE otherwise
  #' @usage is_self_dividing_number(128)
  #' @details A self-dividing number is divisible by every digit it contains.
  #' For example: 128 is self-dividing because 128 % 1 == 0, 128 % 2 == 0, 128 % 8 == 0
  #' @examples
  #' is_self_dividing_number(1)    # TRUE (trivial case)
  #' is_self_dividing_number(128)  # TRUE (1, 2, 8 all divide 128)
  #' is_self_dividing_number(102)  # FALSE (contains 0)
  #' is_self_dividing_number(12)   # TRUE (1, 2 both divide 12)
  
  # Input validation
  if (!is.numeric(num) || length(num) != 1 || num != as.integer(num) || num < 1) {
    stop("Input must be a positive integer")
  }
  
  original <- num
  
  # Check each digit
  while (num > 0) {
    digit <- num %% 10
    
    # If digit is 0 or doesn't divide the original number, it's not self-dividing
    if (digit == 0 || original %% digit != 0) {
      return(FALSE)
    }
    
    num <- floor(num / 10)
  }
  
  return(TRUE)
}

self_dividing_numbers <- function(left, right) {
  #' Find all self-dividing numbers in a given range
  #'
  #' @description Returns a vector of all self-dividing numbers between left and right (inclusive).
  #' @param left The lower bound of the range (inclusive)
  #' @param right The upper bound of the range (inclusive)
  #' @return A vector of integers containing all self-dividing numbers in the range
  #' @usage self_dividing_numbers(1, 22)
  #' @details Iterates through the range and checks each number for the self-dividing property.
  #' @examples
  #' self_dividing_numbers(1, 22)    # returns c(1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 15, 22)
  #' self_dividing_numbers(47, 85)   # returns c(48, 55, 66, 77)
  
  # Input validation
  if (!is.numeric(left) || !is.numeric(right) || length(left) != 1 || length(right) != 1) {
    stop("left and right must be single numeric values")
  }
  
  if (left != as.integer(left) || right != as.integer(right)) {
    stop("left and right must be integers")
  }
  
  if (left < 1 || right < 1) {
    stop("left and right must be positive integers")
  }
  
  if (left > right) {
    stop("left must be less than or equal to right")
  }
  
  result <- c()
  
  # Check each number in the range
  for (i in left:right) {
    if (is_self_dividing_number(i)) {
      result <- c(result, i)
    }
  }
  
  return(result)
}

# Alternative vectorized implementation for better performance
self_dividing_numbers_vectorized <- function(left, right) {
  #' Vectorized version to find self-dividing numbers
  #'
  #' @description Uses vectorized operations to find self-dividing numbers more efficiently
  #' @param left The lower bound of the range (inclusive)  
  #' @param right The upper bound of the range (inclusive)
  #' @return A vector of integers containing all self-dividing numbers in the range
  
  if (left > right || left < 1) {
    return(integer(0))
  }
  
  # Create range and filter using vectorized operations
  numbers <- left:right
  is_self_div <- sapply(numbers, is_self_dividing_number)
  
  return(numbers[is_self_div])
}

# Helper function to analyze digits of a number
analyze_number_digits <- function(num) {
  #' Analyze the digits of a number for debugging purposes
  #'
  #' @description Breaks down a number into its digits and shows divisibility
  #' @param num A positive integer to analyze
  #' @return A list with digits and divisibility information
  
  if (!is.numeric(num) || num < 1) {
    stop("Input must be a positive integer")
  }
  
  original <- num
  digits <- c()
  divisible <- c()
  
  # Extract digits and check divisibility
  while (num > 0) {
    digit <- num %% 10
    digits <- c(digit, digits)  # Prepend to maintain order
    
    if (digit == 0) {
      divisible <- c(FALSE, divisible)
    } else {
      divisible <- c((original %% digit == 0), divisible)
    }
    
    num <- floor(num / 10)
  }
  
  return(list(
    number = original,
    digits = digits,
    divisible = divisible,
    is_self_dividing = all(digits != 0) && all(divisible)
  ))
}

# -----------------------------
# Examples and Testing
# -----------------------------
cat("=== Self-Dividing Numbers ===\n")

# Test individual numbers
test_numbers <- c(1, 2, 12, 22, 48, 102, 128, 144)
cat("\nTesting individual numbers:\n")
for (num in test_numbers) {
  result <- is_self_dividing_number(num)
  cat(sprintf("Number %d is %sself-dividing\n", num, ifelse(result, "", "NOT ")))
}

# Test ranges as shown in examples
cat("\nTesting ranges:\n")

# Example 1: Range [1, 22]
result1 <- self_dividing_numbers(1, 22)
cat("Self-dividing numbers between 1 and 22:\n")
cat(paste(result1, collapse = ", "), "\n")
cat("Expected: 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 15, 22\n\n")

# Example 2: Range [47, 85]  
result2 <- self_dividing_numbers(47, 85)
cat("Self-dividing numbers between 47 and 85:\n")
cat(paste(result2, collapse = ", "), "\n")
cat("Expected: 48, 55, 66, 77\n\n")

# Compare vectorized vs regular implementation
cat("Comparing implementations:\n")
vec_result1 <- self_dividing_numbers_vectorized(1, 22)
vec_result2 <- self_dividing_numbers_vectorized(47, 85)

cat("Regular vs Vectorized for [1, 22]:")
cat(ifelse(identical(result1, vec_result1), " ✓ Match\n", " ✗ Different\n"))

cat("Regular vs Vectorized for [47, 85]:")
cat(ifelse(identical(result2, vec_result2), " ✓ Match\n", " ✗ Different\n"))

# Detailed analysis of specific numbers
cat("\nDetailed analysis of specific numbers:\n")
analysis_numbers <- c(128, 102, 48, 23)
for (num in analysis_numbers) {
  analysis <- analyze_number_digits(num)
  cat(sprintf("Number: %d\n", analysis$number))
  cat(sprintf("Digits: %s\n", paste(analysis$digits, collapse = ", ")))
  cat(sprintf("Divisible by each digit: %s\n", paste(analysis$divisible, collapse = ", ")))
  cat(sprintf("Is self-dividing: %s\n\n", analysis$is_self_dividing))
}

# Edge cases
cat("Testing edge cases:\n")
cat("Range [1, 1]:", paste(self_dividing_numbers(1, 1), collapse = ", "), "\n")
cat("Range [10, 10]:", paste(self_dividing_numbers(10, 10), collapse = ", "), "\n")
cat("Single digits [1, 9]:", paste(self_dividing_numbers(1, 9), collapse = ", "), "\n")

# Performance test (optional - only if microbenchmark is available)
if (requireNamespace("microbenchmark", quietly = TRUE)) {
  cat("\nPerformance comparison for large range [1, 1000]:\n")
  
  start_time <- Sys.time()
  regular_result <- self_dividing_numbers(1, 1000)
  regular_time <- Sys.time() - start_time
  
  start_time <- Sys.time()
  vectorized_result <- self_dividing_numbers_vectorized(1, 1000)
  vectorized_time <- Sys.time() - start_time
  
  cat("Regular implementation:", length(regular_result), "numbers in", regular_time, "seconds\n")
  cat("Vectorized implementation:", length(vectorized_result), "numbers in", vectorized_time, "seconds\n")
  cat("Results match:", identical(regular_result, vectorized_result), "\n")
}
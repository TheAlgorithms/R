# Karatsuba Multiplication Algorithm
# Multiply two integers using the Karatsuba multiplication algorithm
# This is a fast multiplication algorithm that uses divide-and-conquer
# Time Complexity: O(n^log2(3)) ≈ O(n^1.585)

karatsuba <- function(x, y) {
  # Base case for recursion
  if (x < 10 || y < 10) {
    return(x * y)
  }
  
  # Determine the number of digits in the largest number
  n <- max(nchar(as.character(x)), nchar(as.character(y)))
  half <- floor(n / 2)
  
  # Split x and y into high and low parts
  divisor <- 10^half
  high_x <- floor(x / divisor)
  low_x <- x %% divisor
  high_y <- floor(y / divisor)
  low_y <- y %% divisor
  
  # Recursive calls
  z0 <- karatsuba(low_x, low_y)
  z1 <- karatsuba((low_x + high_x), (low_y + high_y))
  z2 <- karatsuba(high_x, high_y)
  
  # Karatsuba formula: (z2 * 10^(2*half)) + ((z1 - z2 - z0) * 10^half) + z0
  result <- (z2 * 10^(2 * half)) + ((z1 - z2 - z0) * 10^half) + z0
  
  return(result)
}

# Example usage
a <- 12345678
b <- 87654321
result <- karatsuba(a, b)
cat(sprintf("Karatsuba(%d, %d) = %.0f\n", a, b, result))

# Verify with standard multiplication
standard_result <- a * b
cat(sprintf("Standard multiplication: %d * %d = %.0f\n", a, b, standard_result))
cat(sprintf("Results match: %s\n", result == standard_result))

# Additional test cases
cat("\nAdditional test cases:\n")
test_cases <- list(
  c(1234, 5678),
  c(999, 888),
  c(12, 34),
  c(123456, 789012)
)

for (test in test_cases) {
  x <- test[1]
  y <- test[2]
  karatsuba_result <- karatsuba(x, y)
  standard_result <- x * y
  cat(sprintf("karatsuba(%d, %d) = %.0f (Expected: %.0f, Match: %s)\n", 
              x, y, karatsuba_result, standard_result, 
              karatsuba_result == standard_result))
}

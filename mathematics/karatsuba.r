# Multiply two integers using the Karatsuba multiplication algorithm.
# This is a fast multiplication algorithm that uses divide-and-conquer.
#
# Expected inputs:
#   - x, y: integer values (positive or negative). Non-integer types are not supported.
#   - Negative values are supported.
#
# Output type:
#   - Returns a numeric value (double). For very large integers (> ~9e15), precision may be lost.
#
# Limits/assumptions:
#   - Uses R’s native double precision arithmetic; not suitable for arbitrary-precision operations.
#   - For very large integers, use `gmp::bigz` for exact results.
#
# Time Complexity: O(n^log2(3)) ≈ O(n^1.585)
# Space Complexity: O(n)
#
# Example:
#   karatsuba(1234, 5678)
#   # [1] 7006652

karatsuba <- function(x, y) {
  # ---- Input validation ----
  if (!is.numeric(x) || !is.numeric(y) || length(x) != 1 || length(y) != 1) {
    stop("Inputs x and y must be single numeric values.")
  }
  if (!is.finite(x) || !is.finite(y)) {
    stop("Inputs must be finite numbers.")
  }
  if (x != floor(x) || y != floor(y)) {
    stop("Inputs must be integers (no decimals).")
  }

  # Handle sign and absolute values
  sgn <- sign(x) * sign(y)
  x <- abs(x)
  y <- abs(y)

  # Base case
  if (x < 10 || y < 10) {
    return(sgn * (x * y))
  }

  # ---- Robust digit counting (avoids scientific notation issues) ----
  get_digits <- function(v) {
    if (v == 0) return(1L)
    floor(log10(abs(v))) + 1L
  }

  n <- max(get_digits(x), get_digits(y))
  half <- floor(n / 2)

  # ---- Split the numbers ----
  high_x <- floor(x / 10^half)
  low_x  <- x %% 10^half
  high_y <- floor(y / 10^half)
  low_y  <- y %% 10^half

  # ---- Recursive calls ----
  z0 <- karatsuba(low_x, low_y)
  z1 <- karatsuba((low_x + high_x), (low_y + high_y))
  z2 <- karatsuba(high_x, high_y)

  # ---- Combine results (Karatsuba formula) ----
  result <- (z2 * 10^(2 * half)) + ((z1 - z2 - z0) * 10^half) + z0

  return(sgn * result)
}

# ---- Example usage ----
# Only runs when script is executed directly, not when sourced/imported
if (sys.nframe() == 0) {
  a <- 12345678
  b <- 87654321
  result <- karatsuba(a, b)
  cat(sprintf("Karatsuba(%d, %d) = %.0f\n", a, b, result))

  standard_result <- a * b
  cat(sprintf("Standard multiplication: %d * %d = %.0f\n", a, b, standard_result))
  cat(sprintf("Results match: %s\n", result == standard_result))

  cat("\nAdditional test cases:\n")
  test_cases <- list(
    c(1234, 5678),
    c(999, 888),
    c(12, 34),
    c(123456, 789012),
    c(-1234, 5678),
    c(-999, -888)
  )

  for (test in test_cases) {
    x <- test[1]
    y <- test[2]
    karatsuba_result <- karatsuba(x, y)
    standard_result <- x * y
    cat(sprintf(
      "karatsuba(%d, %d) = %.0f (Expected: %.0f, Match: %s)\n",
      x, y, karatsuba_result, standard_result,
      karatsuba_result == standard_result
    ))
  }
}
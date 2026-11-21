# Fibonacci Search
#
# Fibonacci search finds a value in a sorted vector. It works like binary
# search but splits the range at Fibonacci numbers instead of the midpoint,
# using only addition and subtraction to move the split points.
#
# 1. Find the smallest Fibonacci number F(k) that is >= the vector length.
# 2. Compare the target with the element at offset + F(k-2).
# 3. If the target is larger, drop the left part and move to F(k-1).
#    If the target is smaller, drop the right part and move to F(k-2).
#    If equal, return the position.
#
# Time Complexity: O(log n)
# Space Complexity: O(1)
#
# Applications:
# - Searching sorted data where division is costly
# - Sorted data on media with expensive random access
#
# Requirement: the input vector must be sorted in ascending order.
#
# Example usage:
# fibonacci_search(c(10, 22, 35, 40, 45), 35)   # 3
# fibonacci_search(c(10, 22, 35, 40, 45), 7)    # -1

#' Search a sorted vector with Fibonacci search
#' @param arr Numeric vector sorted in ascending order
#' @param target Value to find
#' @return 1-based index of target, or -1 if not found
fibonacci_search <- function(arr, target) {
  if (!is.numeric(arr)) {
    stop("Error: arr must be a numeric vector")
  }
  if (!is.numeric(target) || length(target) != 1 || is.na(target)) {
    stop("Error: target must be a single number")
  }
  n <- length(arr)
  if (n == 0) {
    return(-1)
  }

  # Smallest Fibonacci number fib >= n, with the two before it
  fib_2 <- 0
  fib_1 <- 1
  fib <- fib_1 + fib_2
  while (fib < n) {
    fib_2 <- fib_1
    fib_1 <- fib
    fib <- fib_1 + fib_2
  }

  # Last index (0-based) known to be smaller than the target
  offset <- -1

  while (fib > 1) {
    i <- min(offset + fib_2, n - 1)
    if (arr[i + 1] < target) {
      # Drop the left part: move down one Fibonacci number
      fib <- fib_1
      fib_1 <- fib_2
      fib_2 <- fib - fib_1
      offset <- i
    } else if (arr[i + 1] > target) {
      # Drop the right part: move down two Fibonacci numbers
      fib <- fib_2
      fib_1 <- fib_1 - fib_2
      fib_2 <- fib - fib_1
    } else {
      return(i + 1)
    }
  }

  # One element may be left to compare
  if (fib_1 == 1 && offset + 1 < n && arr[offset + 2] == target) {
    return(offset + 2)
  }
  -1
}

# ===========================
# Example Usage & Testing
# ===========================
data <- c(10, 22, 35, 40, 45, 50, 80, 82, 85, 90, 100)

# Every element is found at its own position
for (i in seq_along(data)) {
  stopifnot(fibonacci_search(data, data[i]) == i)
}

# Missing values: below, above and between elements
stopifnot(fibonacci_search(data, 5) == -1)
stopifnot(fibonacci_search(data, 101) == -1)
stopifnot(fibonacci_search(data, 41) == -1)

# Edge cases
stopifnot(fibonacci_search(numeric(0), 3) == -1)
stopifnot(fibonacci_search(c(5), 5) == 1)
stopifnot(fibonacci_search(c(5), 4) == -1)
stopifnot(fibonacci_search(c(1, 2), 2) == 2)

# Agrees with a linear scan on vectors of many sizes
for (n in 1:40) {
  v <- seq(2, 2 * n, by = 2)
  for (t in 0:(2 * n + 1)) {
    expected <- if (t %in% v) match(t, v) else -1
    stopifnot(fibonacci_search(v, t) == expected)
  }
}

# Invalid input raises an error
stopifnot(inherits(try(fibonacci_search("a", 1), silent = TRUE), "try-error"))

cat("Index of 85:", fibonacci_search(data, 85), "\n")
cat("All Fibonacci search tests passed\n")

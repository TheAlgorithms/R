# Postman sort (LSD Radix sort) in R:

# This implements decimal LSD radix sort using a stable counting sort per digit.
# It mirrors the provided Java version: repeatedly apply counting sort by digit
# for exp = 1, 10, 100, ... up to the max number of digits.

postman.sort <- function(elements.vec) {
  if (!is.numeric(elements.vec)) stop("postman.sort: elements.vec must be numeric")
  n <- length(elements.vec)
  if (n <= 1) return(elements.vec)

  # Only supports non-negative integers in standard form (typical radix LSD assumption)
  if (any(elements.vec < 0)) stop("postman.sort: only non-negative values supported")

  # For safety, coerce to integer-like values
  if (any(elements.vec != floor(elements.vec))) stop("postman.sort: values must be integers")

  get.max <- function(a) max(a)

  counting.sort.by.digit <- function(a, exp) {
    n <- length(a)
    output <- numeric(n)
    count <- integer(10) # digits 0..9

    # Count occurrences
    for (i in seq_len(n)) {
      digit <- (a[i] %/% exp) %% 10
      count[digit + 1L] <- count[digit + 1L] + 1L
    }

    # Prefix sums for positions (1-based indexing in R)
    for (i in 2:10) {
      count[i] <- count[i] + count[i - 1L]
    }

    # Build output stable, iterate from end
    for (i in n:1) {
      digit <- (a[i] %/% exp) %% 10
      idx <- count[digit + 1L]
      output[idx] <- a[i]
      count[digit + 1L] <- idx - 1L
    }

    return(output)
  }

  max.val <- get.max(elements.vec)
  exp <- 1
  while ((max.val %/% exp) > 0) {
    elements.vec <- counting.sort.by.digit(elements.vec, exp)
    exp <- exp * 10
  }

  return(elements.vec)
}

# Example:
# postman.sort(c(170, 45, 75, 90, 802, 24, 2, 66))
# [1] 2 24 45 66 75 90 170 802

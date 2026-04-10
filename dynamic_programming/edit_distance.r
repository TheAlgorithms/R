# edit_distance.r
# Levenshtein edit distance algorithm in R
# Computes the minimum number of insertions, deletions, and substitutions
# required to transform one string into another.
# Time Complexity: O(m * n)
# Space Complexity: O(m * n)

# Compute the Levenshtein distance between two strings
edit_distance <- function(str1, str2) {
  #' @param str1: First string
  #' @param str2: Second string
  #' @return: Integer edit distance
  if (!is.character(str1) || !is.character(str2)) {
    stop("Both inputs must be character strings.")
  }
  if (length(str1) != 1 || length(str2) != 1) {
    stop("Each input must be a single string.")
  }

  m <- nchar(str1)
  n <- nchar(str2)
  dp <- matrix(0L, nrow = m + 1, ncol = n + 1)

  # base cases: transform empty prefix
  dp[, 1] <- seq(0L, m)
  dp[1, ] <- seq(0L, n)

  for (i in seq_len(m) + 1L) {
    for (j in seq_len(n) + 1L) {
      cost <- if (substr(str1, i - 1, i - 1) == substr(str2, j - 1, j - 1)) 0L else 1L
      dp[i, j] <- min(
        dp[i - 1, j] + 1L,      # deletion
        dp[i, j - 1] + 1L,      # insertion
        dp[i - 1, j - 1] + cost # substitution
      )
    }
  }

  return(dp[m + 1, n + 1])
}

# Compute the edit distance and reconstruct an optimal alignment path
edit_distance_with_path <- function(str1, str2) {
  #' @param str1: First string
  #' @param str2: Second string
  #' @return: List with distance, operations, and dp table
  if (!is.character(str1) || !is.character(str2)) {
    stop("Both inputs must be character strings.")
  }
  if (length(str1) != 1 || length(str2) != 1) {
    stop("Each input must be a single string.")
  }

  m <- nchar(str1)
  n <- nchar(str2)
  dp <- matrix(0L, nrow = m + 1, ncol = n + 1)
  dp[, 1] <- seq(0L, m)
  dp[1, ] <- seq(0L, n)

  for (i in seq_len(m) + 1L) {
    for (j in seq_len(n) + 1L) {
      cost <- if (substr(str1, i - 1, i - 1) == substr(str2, j - 1, j - 1)) 0L else 1L
      dp[i, j] <- min(
        dp[i - 1, j] + 1L,
        dp[i, j - 1] + 1L,
        dp[i - 1, j - 1] + cost
      )
    }
  }

  i <- m + 1
  j <- n + 1
  ops <- character()

  while (i > 1 || j > 1) {
    if (i > 1 && j > 1 && dp[i, j] == dp[i - 1, j - 1] +
        (substr(str1, i - 1, i - 1) != substr(str2, j - 1, j - 1))) {
      if (substr(str1, i - 1, i - 1) == substr(str2, j - 1, j - 1)) {
        ops <- c("match", ops)
      } else {
        ops <- c(sprintf("substitute '%s' -> '%s'", substr(str1, i - 1, i - 1), substr(str2, j - 1, j - 1)), ops)
      }
      i <- i - 1
      j <- j - 1
    } else if (i > 1 && dp[i, j] == dp[i - 1, j] + 1L) {
      ops <- c(sprintf("delete '%s'", substr(str1, i - 1, i - 1)), ops)
      i <- i - 1
    } else {
      ops <- c(sprintf("insert '%s'", substr(str2, j - 1, j - 1)), ops)
      j <- j - 1
    }
  }

  return(list(
    distance = dp[m + 1, n + 1],
    operations = ops,
    dp_table = dp
  ))
}

# Example usage:
# print(edit_distance("kitten", "sitting"))
# result <- edit_distance_with_path("kitten", "sitting")
# print(result$distance)
# print(result$operations)

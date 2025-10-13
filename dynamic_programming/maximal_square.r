# Maximal Square (Dynamic Programming)
#
# Given a binary matrix of '0' and '1' (character or numeric), find the area of
# the largest square containing only 1s.
#
# Idea
# Use DP with padding: dp[r+1][c+1] stores the side length of the largest square
# whose bottom-right corner is at matrix[r][c]. Transition when matrix[r][c] == 1:
#   dp[r+1][c+1] = 1 + min(dp[r][c+1], dp[r+1][c], dp[r][c])
# Track the maximum side while filling the table. Area = max_side^2.
#
# Complexity
# - Time:  O(m*n)
# - Space: O(m*n) for 2D DP, O(n) for space-optimized 1D DP

# Normalize matrix values to integers 0/1
normalize_binary_matrix <- function(mat) {
  if (is.character(mat)) {
    return(ifelse(mat == '1', 1L, 0L))
  } else if (is.numeric(mat) || is.integer(mat)) {
    return(ifelse(mat != 0, 1L, 0L))
  } else {
    stop("Unsupported matrix type; use character ('0'/'1') or numeric/integer 0/1")
  }
}

# 2D DP approach
maximal_square <- function(mat) {
  if (length(mat) == 0) return(0L)
  bin <- normalize_binary_matrix(mat)
  rows <- nrow(bin)
  cols <- ncol(bin)

  dp <- matrix(0L, nrow = rows + 1L, ncol = cols + 1L)
  max_side <- 0L

  for (r in 1:rows) {
    for (c in 1:cols) {
      if (bin[r, c] == 1L) {
        dp[r + 1L, c + 1L] <- 1L + min(dp[r, c + 1L], dp[r + 1L, c], dp[r, c])
        if (dp[r + 1L, c + 1L] > max_side) max_side <- dp[r + 1L, c + 1L]
      }
    }
  }

  as.integer(max_side * max_side)
}

# 1D space-optimized approach
maximal_square_optimized <- function(mat) {
  if (length(mat) == 0) return(0L)
  bin <- normalize_binary_matrix(mat)
  rows <- nrow(bin)
  cols <- ncol(bin)

  dp <- integer(cols + 1L)  # current row DP with padding
  max_side <- 0L

  for (r in 1:rows) {
    prev_diag <- 0L  # represents dp[c] from previous row and previous column (top-left)
    for (c in 1:cols) {
      temp <- dp[c + 1L]  # save current top (for next iteration's prev_diag)
      if (bin[r, c] == 1L) {
        dp[c + 1L] <- 1L + min(dp[c + 1L], dp[c], prev_diag)
        if (dp[c + 1L] > max_side) max_side <- dp[c + 1L]
      } else {
        dp[c + 1L] <- 0L
      }
      prev_diag <- temp
    }
  }

  as.integer(max_side * max_side)
}

# ------------------------------
# Examples / Demonstration
# ------------------------------
cat("=== Maximal Square (DP) ===\n\n")

# Example 1: basic
mat1 <- matrix(c('1','0','1','0','0',
                 '1','0','1','1','1',
                 '1','1','1','1','1',
                 '1','0','0','1','0'),
               nrow = 4, byrow = TRUE)
cat("Example 1 (2D DP): ", maximal_square(mat1), "\n")
cat("Example 1 (1D DP): ", maximal_square_optimized(mat1), "\n\n")

# Example 2: all zeros
mat2 <- matrix('0', nrow = 3, ncol = 3)
cat("Example 2 (all zeros): ", maximal_square(mat2), "\n\n")

# Example 3: numeric matrix
mat3 <- matrix(c(1,1,1, 1,1,1, 1,1,1), nrow = 3, byrow = TRUE)
cat("Example 3 (all ones 3x3): ", maximal_square(mat3), "\n\n")

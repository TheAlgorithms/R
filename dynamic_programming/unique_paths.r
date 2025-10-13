# Unique Paths Problem (Grid Paths)
#
# Count the number of unique paths from the top-left corner (1,1) to the
# bottom-right corner (m,n) in an m x n grid when you can only move either
# right or down at any point in time.
#
# This is a classic dynamic programming problem. Below are three approaches:
# 1) Tabulation DP (O(m*n) time, O(m*n) space)
# 2) Space-optimized DP using a 1D array (O(m*n) time, O(n) space)
# 3) Top-down recursion with memoization (O(m*n) time, O(m*n) space)
#
# Additionally, we provide a variant that supports obstacles on the grid.
#
# Complexity
# Time complexity: O(m * n)
# Space complexity: O(m * n) for the standard/memo DP; O(n) for the optimized version
#
# Applications
# - Robot/grid movement counting
# - Combinatorics and path counting
# - Subproblems in DP interviews and contests

# ------------------------------
# 1) Tabulation DP (returns DP table for illustration)
# ------------------------------
unique_paths_dp <- function(m, n) {
  #' Count unique paths using 2D DP table
  #' @param m: number of rows (positive integer)
  #' @param n: number of columns (positive integer)
  #' @return: list(count = paths count, dp_table = m x n integer matrix)

  if (m <= 0 || n <= 0) {
    return(list(count = 0L, dp_table = matrix(0L, nrow = max(1, m), ncol = max(1, n))))
  }

  dp <- matrix(0L, nrow = m, ncol = n)

  # Base cases: first row and first column have exactly one path
  dp[1, ] <- 1L
  dp[, 1] <- 1L

  # Fill the rest: dp[i,j] = dp[i-1,j] + dp[i,j-1]
  if (m > 1 && n > 1) {
    for (i in 2:m) {
      for (j in 2:n) {
        dp[i, j] <- dp[i - 1, j] + dp[i, j - 1]
      }
    }
  }

  list(count = dp[m, n], dp_table = dp)
}

# ------------------------------
# 2) Space-optimized DP using 1D array
# ------------------------------
unique_paths_optimized <- function(m, n) {
  #' Count unique paths using 1D DP array (O(n) space)
  #' @param m: number of rows
  #' @param n: number of columns
  #' @return: integer count of unique paths

  if (m <= 0 || n <= 0) return(0L)

  # Ensure we iterate over the longer dimension in the outer loop only if desired.
  # Here, we fix a 1D array of size n (columns) and iterate rows.
  dp <- rep.int(1L, n)
  if (m == 1L || n == 1L) return(1L)

  for (i in 2:m) {
    for (j in 2:n) {
      dp[j] <- dp[j] + dp[j - 1]
    }
  }
  dp[n]
}

# ------------------------------
# 3) Top-down recursion with memoization (mirrors the user's Java approach)
# ------------------------------
unique_paths_memo <- function(m, n) {
  #' Count unique paths using recursion + memoization
  #' @param m: number of rows
  #' @param n: number of columns
  #' @return: integer count of unique paths

  if (m <= 0 || n <= 0) return(0L)

  memo <- matrix(-1L, nrow = m, ncol = n)

  helper <- function(i, j) {
    # i, j are 1-based indices
    if (i > m || j > n) return(0L)
    if (i == m && j == n) return(1L)
    if (memo[i, j] != -1L) return(memo[i, j])

    memo[i, j] <<- helper(i, j + 1L) + helper(i + 1L, j)
    memo[i, j]
  }

  helper(1L, 1L)
}

# ------------------------------
# Variant: Unique Paths with Obstacles
# Grid values: 0 = free cell, 1 = obstacle
# ------------------------------
unique_paths_with_obstacles <- function(grid) {
  #' Count unique paths with obstacles using DP
  #' @param grid: m x n integer matrix (0 free, 1 obstacle)
  #' @return: list(count = paths count, dp_table = m x n integer matrix)

  m <- nrow(grid)
  n <- ncol(grid)
  if (length(grid) == 0 || m == 0 || n == 0) {
    return(list(count = 0L, dp_table = matrix(0L, nrow = 1, ncol = 1)))
  }

  dp <- matrix(0L, nrow = m, ncol = n)

  # If start or end is blocked, no paths
  if (grid[1, 1] == 1L || grid[m, n] == 1L) {
    return(list(count = 0L, dp_table = dp))
  }

  dp[1, 1] <- 1L

  # First row
  if (n > 1) {
    for (j in 2:n) {
      dp[1, j] <- if (grid[1, j] == 1L) 0L else dp[1, j - 1]
    }
  }

  # First column
  if (m > 1) {
    for (i in 2:m) {
      dp[i, 1] <- if (grid[i, 1] == 1L) 0L else dp[i - 1, 1]
    }
  }

  # Rest cells
  if (m > 1 && n > 1) {
    for (i in 2:m) {
      for (j in 2:n) {
        if (grid[i, j] == 1L) {
          dp[i, j] <- 0L
        } else {
          dp[i, j] <- dp[i - 1, j] + dp[i, j - 1]
        }
      }
    }
  }

  list(count = dp[m, n], dp_table = dp)
}

# ------------------------------
# Helpers for display
# ------------------------------
print_unique_paths_dp <- function(dp_table) {
  m <- nrow(dp_table)
  n <- ncol(dp_table)
  cat("DP Table for Unique Paths (values are number of paths to reach each cell):\n")
  for (i in 1:m) {
    cat("  ")
    for (j in 1:n) {
      cat(sprintf("%6d ", dp_table[i, j]))
    }
    cat("\n")
  }
  cat("\n")
}

# ===========================
# Example Usage & Testing
# ===========================
cat("=== Unique Paths (Dynamic Programming) ===\n\n")

# Test 1: Basic cases
cat("Test 1: Basic cases\n")
res_3x7 <- unique_paths_dp(3, 7)
cat("Grid 3x7 -> unique paths:", res_3x7$count, "\n")
print_unique_paths_dp(res_3x7$dp_table)

res_3x7_opt <- unique_paths_optimized(3, 7)
cat("Optimized 3x7 -> unique paths:", res_3x7_opt, " | match:", res_3x7_opt == res_3x7$count, "\n\n")

# Test 2: Edge cases
cat("Test 2: Edge cases\n")
cat("1x1 ->", unique_paths_optimized(1, 1), "\n")
cat("1x5 ->", unique_paths_optimized(1, 5), "\n")
cat("5x1 ->", unique_paths_optimized(5, 1), "\n\n")

# Test 3: Memoized version parity
cat("Test 3: Memoized version parity\n")
memo_3x7 <- unique_paths_memo(3, 7)
cat("Memo 3x7 ->", memo_3x7, " | match:", memo_3x7 == res_3x7$count, "\n\n")

# Test 4: Obstacles variant
cat("Test 4: Obstacles variant\n")
grid_obs <- matrix(c(
  0, 0, 0, 0,
  0, 1, 0, 0,
  0, 0, 0, 0
), nrow = 3, byrow = TRUE)

obs_res <- unique_paths_with_obstacles(grid_obs)
cat("Unique paths with obstacles:", obs_res$count, "\n")
print_unique_paths_dp(obs_res$dp_table)

# Test 5: Quick performance comparison (optional)
if (requireNamespace("microbenchmark", quietly = TRUE)) {
  cat("Test 5: Performance (microbenchmark)\n")
  library(microbenchmark)
  mb <- microbenchmark(
    dp = unique_paths_dp(10, 10)$count,
    opt = unique_paths_optimized(10, 10),
    memo = unique_paths_memo(10, 10),
    times = 50L
  )
  print(summary(mb))
  cat("Results agree:", unique_paths_dp(10, 10)$count == unique_paths_optimized(10, 10) &&
        unique_paths_optimized(10, 10) == unique_paths_memo(10, 10), "\n\n")
} else {
  cat("microbenchmark not installed; skipping performance test.\n\n")
}

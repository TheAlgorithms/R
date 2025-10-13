# Minimum Path Sum (Dynamic Programming)
#
# Intuition
# We need to move from the top-left corner to the bottom-right corner of a grid.
# At every step, we can only move down or right. The goal is to minimize the
# sum of values along the path.
#
# This hints that at each cell (i, j), the optimal path sum depends on the
# minimum of the optimal sums from the top cell (i-1, j) and the left cell (i, j-1).
#
# Approach
# - Top-down recursion with memoization (primary implementation)
#   fun(i, j) = grid[i, j] + min(fun(i-1, j), fun(i, j-1))
#   Base cases:
#     * fun(1, 1) = grid[1, 1]
#     * out-of-bounds indices return +Inf (invalid path)
#   Use a dp table initialized with NA to avoid recomputation.
# - Bottom-up tabulation (iterative) for comparison and path reconstruction.
#
# Complexity
# - Time:  O(m * n) — each cell computed once due to memoization.
# - Space: O(m * n) for the DP table; O(m + n) recursion stack (top-down).
#
# Applications
# - Robot path planning with costs
# - Image seam carving (as a building block)
# - Grid-based games and maze cost minimization
# - Teaching dynamic programming patterns

# -------------------------------
# Top-down (memoized) solution
# -------------------------------
min_path_sum_topdown <- function(grid) {
  #' Minimum Path Sum using Top-Down DP with Memoization
  #' @param grid Numeric matrix representing the grid costs (m x n)
  #' @return List with fields: min_sum (numeric), dp (matrix)
  
  # Validate input
  if (is.null(grid) || length(grid) == 0) {
    return(list(min_sum = 0, dp = matrix(numeric(0), nrow = 0, ncol = 0)))
  }
  if (!is.matrix(grid)) {
    stop("'grid' must be a numeric matrix")
  }
  m <- nrow(grid)
  n <- ncol(grid)
  if (m == 0 || n == 0) {
    return(list(min_sum = 0, dp = matrix(numeric(0), nrow = 0, ncol = 0)))
  }
  
  dp <- matrix(NA_real_, nrow = m, ncol = n)
  
  fun <- function(i, j) {
    # Out of bounds => invalid path
    if (i < 1 || j < 1) return(Inf)
    # Base case: start cell
    if (i == 1 && j == 1) return(grid[1, 1])
    # Memoized value
    if (!is.na(dp[i, j])) return(dp[i, j])
    
    up <- fun(i - 1, j)
    left <- fun(i, j - 1)
    dp[i, j] <<- grid[i, j] + min(up, left)
    return(dp[i, j])
  }
  
  min_sum <- fun(m, n)
  list(min_sum = min_sum, dp = dp)
}

# --------------------------------
# Bottom-up (iterative) solution
# --------------------------------
min_path_sum_bottomup <- function(grid) {
  #' Minimum Path Sum using Bottom-Up DP (Tabulation)
  #' @param grid Numeric matrix representing the grid costs (m x n)
  #' @return List with fields: min_sum (numeric), dp (matrix)
  
  if (is.null(grid) || length(grid) == 0) {
    return(list(min_sum = 0, dp = matrix(numeric(0), nrow = 0, ncol = 0)))
  }
  if (!is.matrix(grid)) {
    stop("'grid' must be a numeric matrix")
  }
  m <- nrow(grid)
  n <- ncol(grid)
  if (m == 0 || n == 0) {
    return(list(min_sum = 0, dp = matrix(numeric(0), nrow = 0, ncol = 0)))
  }
  
  dp <- matrix(0, nrow = m, ncol = n)
  dp[1, 1] <- grid[1, 1]
  
  # First row
  if (n >= 2) {
    for (j in 2:n) dp[1, j] <- grid[1, j] + dp[1, j - 1]
  }
  # First column
  if (m >= 2) {
    for (i in 2:m) dp[i, 1] <- grid[i, 1] + dp[i - 1, 1]
  }
  # Rest of the grid
  if (m >= 2 && n >= 2) {
    for (i in 2:m) {
      for (j in 2:n) {
        dp[i, j] <- grid[i, j] + min(dp[i - 1, j], dp[i, j - 1])
      }
    }
  }
  
  list(min_sum = dp[m, n], dp = dp)
}

# -------------------------------------------------
# Reconstruct one minimal-cost path from dp matrix
# -------------------------------------------------
reconstruct_min_path <- function(grid, dp) {
  #' Reconstruct one minimum-cost path from (1,1) to (m,n)
  #' @param grid Numeric matrix of costs
  #' @param dp DP matrix with minimal sums to each cell
  #' @return List with: path_coords (list of (i,j)), path_values, path_sum
  
  if (!is.matrix(grid) || !is.matrix(dp)) stop("'grid' and 'dp' must be matrices")
  m <- nrow(grid); n <- ncol(grid)
  if (nrow(dp) != m || ncol(dp) != n) stop("'dp' must match the dimensions of 'grid'")
  if (m == 0 || n == 0) return(list(path_coords = list(), path_values = c(), path_sum = 0))
  
  i <- m; j <- n
  coords <- list()
  values <- c()
  
  while (!(i == 1 && j == 1)) {
    coords <- c(list(c(i, j))), coords
    values <- c(grid[i, j], values)
    # Decide whether we came from up or left
    up <- if (i > 1) dp[i - 1, j] else Inf
    left <- if (j > 1) dp[i, j - 1] else Inf
    # move towards the predecessor whose dp + grid[i,j] == dp[i,j]
    # If tie, prefer left (arbitrary but deterministic)
    if (!is.infinite(left) && dp[i, j] == grid[i, j] + left) {
      j <- j - 1
    } else if (!is.infinite(up) && dp[i, j] == grid[i, j] + up) {
      i <- i - 1
    } else {
      # Fallback: choose the smaller predecessor
      if (left <= up) j <- j - 1 else i <- i - 1
    }
  }
  # Add the start cell
  coords <- c(list(c(1, 1)), coords)
  values <- c(grid[1, 1], values)
  
  list(path_coords = coords, path_values = values, path_sum = sum(values))
}

# --------------------------------------
# Pretty-print the DP table for the grid
# --------------------------------------
print_mps_dp <- function(grid, dp) {
  #' Print the DP table in a readable format for Minimum Path Sum
  #' @param grid Numeric matrix of costs
  #' @param dp DP matrix with minimal sums
  
  m <- nrow(grid); n <- ncol(grid)
  cat("DP Table (Minimum Path Sum):\n")
  for (i in 1:m) {
    cat(paste(sprintf("%6s", ifelse(is.infinite(dp[i, ]), "Inf", format(round(dp[i, ], 2), nsmall = 0))), collapse = " "), "\n")
  }
  cat("\n")
}

# ===========================
# Example Usage & Testing
# ===========================
cat("=== Minimum Path Sum (Dynamic Programming) ===\n\n")

# Test 1: Classic 3x3 grid example
grid1 <- matrix(c(
  1, 3, 1,
  1, 5, 1,
  4, 2, 1
), nrow = 3, byrow = TRUE)

cat("Test 1: Classic 3x3 Grid\n")
cat("Grid:\n")
print(grid1)

res_td_1 <- min_path_sum_topdown(grid1)
res_bu_1 <- min_path_sum_bottomup(grid1)
cat("Top-Down Min Sum:", res_td_1$min_sum, "\n")
cat("Bottom-Up Min Sum:", res_bu_1$min_sum, "\n")
cat("Match:", res_td_1$min_sum == res_bu_1$min_sum, "\n\n")

cat("DP (Bottom-Up):\n")
print_mps_dp(grid1, res_bu_1$dp)

path1 <- reconstruct_min_path(grid1, res_bu_1$dp)
cat("One minimum path (coords i,j):\n")
for (p in path1$path_coords) cat(sprintf("(%d,%d) ", p[1], p[2]))
cat("\nPath values:", paste(path1$path_values, collapse = " -> "), "\n")
cat("Path sum:", path1$path_sum, "\n\n")

# Test 2: Single cell
cat("Test 2: Single Cell\n")
grid2 <- matrix(7, nrow = 1)
res2 <- min_path_sum_topdown(grid2)
cat("Min Sum:", res2$min_sum, "\n\n")

# Test 3: Rectangular grid 2x4
cat("Test 3: Rectangular 2x4 Grid\n")
grid3 <- matrix(c(
  5, 1, 3, 2,
  4, 2, 1, 7
), nrow = 2, byrow = TRUE)
print(grid3)
res_td_3 <- min_path_sum_topdown(grid3)
res_bu_3 <- min_path_sum_bottomup(grid3)
cat("Top-Down Min Sum:", res_td_3$min_sum, "\n")
cat("Bottom-Up Min Sum:", res_bu_3$min_sum, "\n")
cat("Match:", res_td_3$min_sum == res_bu_3$min_sum, "\n\n")

# Test 4: Larger random grid (performance sanity)
set.seed(123)
cat("Test 4: Random 10x10 Grid (sanity)\n")
grid4 <- matrix(sample(1:9, 100, replace = TRUE), nrow = 10, byrow = TRUE)
res_bu_4 <- min_path_sum_bottomup(grid4)
cat("Min Sum (Bottom-Up):", res_bu_4$min_sum, "\n\n")

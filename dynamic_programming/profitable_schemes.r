# Profitable Schemes (LeetCode 879) - Dynamic Programming in R
#
# Problem:
# Given n members, an array group[] where group[i] is the members needed for crime i,
# and an array profit[] where profit[i] is the profit for crime i, count the number
# of schemes such that the total members used <= n and total profit >= minProfit.
# Return the count modulo 1e9+7.
#
# Intuition
# Three-state DP: remaining members (n_remain), starting index (i), and current gain
# (capped at 100). We use memoization to avoid recomputation. A bottom-up version is
# also included for completeness.
#
# Complexity
# Time Complexity: O(n * minProfit * m) where m = length(group)
# Space Complexity: O(n * minProfit * m) for memo table or O(n * minProfit) for bottom-up
#
# Notes
# - We cap gain at 100 because minProfit <= 100 (common constraint).
# - Use modulo 1e9+7 to handle large counts.

MOD <- as.integer(1e9 + 7)

# ------------------------------
# Top-down DP with memoization (mirrors user-provided Java approach)
# ------------------------------
profitable_schemes_topdown <- function(n, minProfit, group, profit) {
  stopifnot(length(group) == length(profit))

  m <- length(group)
  maxGain <- 100L

  # 3D memo: dims [n_remain + 1][m + 1][maxGain + 1]
  # Initialize to -1 (uncomputed)
  memo <- array(-1L, dim = c(n + 1L, m + 1L, maxGain + 1L))

  dp <- function(n_remain, start_i, gain) {
    if (n_remain < 0L) return(0L)

    # Use min(gain, 100) to keep within bounds
    if (gain > maxGain) gain <- maxGain

    cached <- memo[n_remain + 1L, start_i + 1L, gain + 1L]
    if (cached != -1L) return(cached)

    poss <- if (gain >= minProfit) 1L else 0L

    if (start_i == m) {
      memo[n_remain + 1L, start_i + 1L, gain + 1L] <<- poss
      return(poss)
    }

    # Try all choices from start_i to end (subset enumeration by advancing index)
    for (take in start_i:(m - 1L)) {
      newGain <- gain + profit[take + 1L]
      if (newGain > maxGain) newGain <- maxGain
      poss <- (poss + dp(n_remain - group[take + 1L], take + 1L, newGain)) %% MOD
    }

    memo[n_remain + 1L, start_i + 1L, gain + 1L] <<- poss
    poss
  }

  dp(n_remain = n, start_i = 0L, gain = 0L)
}

# ------------------------------
# Bottom-up DP (classic 2D DP over members and profit)
# dp[people][p] = number of ways to achieve profit p using some subset of crimes
# Iterate crimes and update dp in reverse on people to avoid reuse
# ------------------------------
profitable_schemes_bottomup <- function(n, minProfit, group, profit) {
  stopifnot(length(group) == length(profit))

  m <- length(group)
  maxGain <- 100L

  dp <- matrix(0L, nrow = n + 1L, ncol = maxGain + 1L)
  dp[1:(n + 1L), 1L] <- 1L  # profit 0 has 1 way (choose nothing)

  for (i in seq_len(m)) {
    g <- group[i]
    pr <- profit[i]
    for (people in n:0) {
      newPeople <- people - g
      if (newPeople < 0) next
      for (p in 0:maxGain) {
        newP <- p + pr
        if (newP > maxGain) newP <- maxGain
        # dp indices are 1-based in R
        dp[newPeople + 1L, newP + 1L] <- (dp[newPeople + 1L, newP + 1L] + dp[people + 1L, p + 1L]) %% MOD
      }
    }
  }

  # Sum all dp[people][p] for p >= minProfit
  cols <- (minProfit:maxGain) + 1L
  total <- sum(dp[, cols, drop = FALSE]) %% MOD
  as.integer(total)
}

# ------------------------------
# Examples / Demonstration
# ------------------------------
cat("=== Profitable Schemes (Dynamic Programming) ===\n\n")

# Example 1: Simple
n1 <- 5L
minProfit1 <- 3L
group1 <- c(2L, 2L)
profit1 <- c(2L, 3L)
cat("Example 1 (Top-down): ", profitable_schemes_topdown(n1, minProfit1, group1, profit1), "\n")
cat("Example 1 (Bottom-up):", profitable_schemes_bottomup(n1, minProfit1, group1, profit1), "\n\n")

# Example 2: Larger arrays
n2 <- 10L
minProfit2 <- 5L
group2 <- c(2L, 3L, 5L)
profit2 <- c(6L, 7L, 8L)
cat("Example 2 (Top-down): ", profitable_schemes_topdown(n2, minProfit2, group2, profit2), "\n")
cat("Example 2 (Bottom-up):", profitable_schemes_bottomup(n2, minProfit2, group2, profit2), "\n\n")

# Example 3: Edge cases
cat("Example 3 (Edge) zero minProfit -> counts all subsets within members\n")
cat("Top-down:  ", profitable_schemes_topdown(1L, 0L, c(1L), c(1L)), "\n")
cat("Bottom-up: ", profitable_schemes_bottomup(1L, 0L, c(1L), c(1L)), "\n\n")

cat("Note: For large instances, counts are returned modulo 1e9+7.\n\n")

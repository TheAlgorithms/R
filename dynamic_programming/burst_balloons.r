# Burst Balloons (Interval DP)
#
# Problem (LeetCode 312): Given n balloons, each with a number, when you burst balloon k
# you gain nums[left] * nums[k] * nums[right], where left and right are the nearest
# unburst balloons adjacent to k. Compute the maximum coins you can collect by
# bursting all balloons in optimal order.
#
# Intuition
# Bursting a balloon changes neighbors but the score contributed by the final balloon
# chosen in an interval depends only on the boundary balloons of that interval.
# This suggests Interval Dynamic Programming: define dp[i][j] as the maximum coins
# obtainable by bursting all balloons strictly between i and j (i and j are boundaries).
# After padding with 1 on both ends, the answer is dp[0][n+1].
#
# Approach (Bottom-up Interval DP)
# - Pad the array: vals <- c(1, nums, 1)
# - dp is an (n+2) x (n+2) matrix of zeros
# - For increasing interval length len from 2 to n+1:
#   For i from 0 to n+1-len:
#     j = i + len
#     For k in (i+1)..(j-1): last balloon to burst in (i, j)
#       dp[i, j] = max(dp[i, j], dp[i, k] + vals[i]*vals[k]*vals[j] + dp[k, j])
# - Answer: dp[0, n+1]
#
# Complexity
# - Time: O(N^3)
# - Space: O(N^2)

# ------------------------------
# Bottom-up interval DP
# ------------------------------
burst_balloons <- function(nums) {
  n <- length(nums)
  if (n == 0) return(0L)

  vals <- c(1L, as.integer(nums), 1L)  # pad with 1s
  N <- n + 2L
  dp <- matrix(0L, nrow = N, ncol = N)

  # len is the distance between boundaries i and j
  for (len in 2L:N) {
    for (i in 0L:(N - len)) {
      j <- i + len
      if (j >= N) next
      best <- 0L
      # choose k as the last balloon to burst between (i, j)
      for (k in (i + 1L):(j - 1L)) {
        gain <- vals[i + 1L] * vals[k + 1L] * vals[j + 1L]
        total <- dp[i + 1L, k + 1L] + gain + dp[k + 1L, j + 1L]
        if (total > best) best <- total
      }
      dp[i + 1L, j + 1L] <- best
    }
  }

  dp[1L, N]
}

# ------------------------------
# Optional: Top-down recursion with memoization
# dp(i, j) = max over k in (i, j): dp(i, k) + vals[i]*vals[k]*vals[j] + dp(k, j)
# where i, j are boundary indices in the padded array
# ------------------------------
burst_balloons_topdown <- function(nums) {
  n <- length(nums)
  if (n == 0) return(0L)
  vals <- c(1L, as.integer(nums), 1L)
  N <- n + 2L
  memo <- matrix(-1L, nrow = N, ncol = N)

  dp <- function(i, j) {
    if (i + 1L >= j) return(0L)
    if (memo[i + 1L, j + 1L] != -1L) return(memo[i + 1L, j + 1L])
    best <- 0L
    for (k in (i + 1L):(j - 1L)) {
      gain <- vals[i + 1L] * vals[k + 1L] * vals[j + 1L]
      total <- dp(i, k) + gain + dp(k, j)
      if (total > best) best <- total
    }
    memo[i + 1L, j + 1L] <<- best
    best
  }

  dp(0L, N - 1L)
}

# ------------------------------
# Examples / Demonstration
# ------------------------------
cat("=== Burst Balloons (Interval DP) ===\n\n")

# Example 1
nums1 <- c(3L, 1L, 5L, 8L)
cat("Example 1 (Bottom-up): ", burst_balloons(nums1), "\n")
cat("Example 1 (Top-down): ", burst_balloons_topdown(nums1), "\n\n")

# Example 2 (edge cases)
cat("Example 2 (Edge): empty -> ", burst_balloons(integer(0)), "\n")
cat("Single balloon [7] -> ", burst_balloons(c(7L)), "\n\n")

# Example 3 (random small)
set.seed(7)
nums3 <- sample(1:5, 5, replace = TRUE)
cat("Example 3 nums:", paste(nums3, collapse = ","), " -> ", burst_balloons(nums3), "\n\n")

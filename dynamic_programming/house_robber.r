# House Robber (Dynamic Programming, O(1) space)
#
# Given an array nums where nums[i] is the amount of money of the i-th house,
# return the maximum amount you can rob without robbing two adjacent houses.
#
# Intuition
# For each index i, choose between:
#  - Take: nums[i] + best up to i-2
#  - Skip: best up to i-1
# This leads to a DP relation and can be optimized to O(1) space using two variables.
#
# Complexity
# - Time:  O(n)
# - Space: O(1)

# O(1) space DP
house_robber <- function(nums) {
  if (length(nums) == 0) return(0L)
  n <- length(nums)
  if (n == 1L) return(as.integer(nums[1]))

  prev1 <- as.integer(nums[1])  # best up to house 1
  prev2 <- 0L                   # best up to house 0 (none)

  for (i in 2:n) {
    take <- as.integer(nums[i]) + prev2
    notTake <- prev1
    curr <- if (take > notTake) take else notTake
    prev2 <- prev1
    prev1 <- curr
  }

  prev1
}

# Optional: Tabulation DP (O(n) space)
house_robber_tabulation <- function(nums) {
  if (length(nums) == 0) return(0L)
  n <- length(nums)
  if (n == 1L) return(as.integer(nums[1]))
  dp <- integer(n)
  dp[1] <- as.integer(nums[1])
  dp[2] <- max(dp[1], as.integer(nums[2]))
  if (n == 2L) return(dp[2])
  for (i in 3:n) {
    dp[i] <- max(dp[i - 1L], as.integer(nums[i]) + dp[i - 2L])
  }
  dp[n]
}

# ------------------------------
# Examples / Demonstration
# ------------------------------
cat("=== House Robber (DP) ===\n\n")

cases <- list(
  c(1L,2L,3L,1L),
  c(2L,7L,9L,3L,1L),
  c(2L),
  c(2L,1L),
  integer(0)
)

for (nums in cases) {
  cat("nums = [", paste(nums, collapse = ","), "] -> O(1): ", house_robber(nums),
      ", tab: ", house_robber_tabulation(nums), "\n", sep = "")
}
cat("\n")

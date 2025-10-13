# Wiggle Subsequence (O(n) DP)
#
# Given an integer vector nums, return the length of the longest wiggle subsequence.
# A wiggle sequence alternates between positive and negative differences.
#
# Intuition
# Track two values at each step:
# - up:   length of a longest wiggle subsequence ending at i with last diff > 0
# - down: length of a longest wiggle subsequence ending at i with last diff < 0
# Transitions:
# - If nums[i] > nums[i-1]: up = down + 1, down stays
# - If nums[i] < nums[i-1]: down = up + 1, up stays
# - Else: both stay
#
# Complexity
# - Time:  O(n)
# - Space: O(1)

wiggle_max_length <- function(nums) {
  n <- length(nums)
  if (n == 0L) return(0L)
  if (n == 1L) return(1L)

  up <- 1L
  down <- 1L

  for (i in 2:n) {
    if (nums[i] > nums[i - 1L]) {
      up <- down + 1L
    } else if (nums[i] < nums[i - 1L]) {
      down <- up + 1L
    } else {
      # equal: no change
    }
  }

  if (up > down) up else down
}

# ------------------------------
# Examples / Demonstration
# ------------------------------
cat("=== Wiggle Subsequence (DP) ===\n\n")

examples <- list(
  c(1L,7L,4L,9L,2L,5L),         # 6
  c(1L,17L,5L,10L,13L,15L,10L,5L,16L,8L), # 7
  c(1L,2L,3L,4L,5L,6L,7L,8L,9L), # 2
  c(0L,0L),                      # 1 or 2 depending definition; here 1
  integer(0)
)

for (nums in examples) {
  cat("nums = [", paste(nums, collapse=","), "] -> ", wiggle_max_length(nums), "\n", sep = "")
}
cat("\n")

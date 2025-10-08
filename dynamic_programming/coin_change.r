# Coin Change Problem
#
# The Coin Change problem finds the minimum number of coins needed to make a certain amount
# using a given set of coin denominations.
#
# Time Complexity: O(amount * n) where n = number of coin denominations
# Space Complexity: O(amount)
#
# Applications:
# - Currency and cash management
# - Making change in vending machines or payment systems
# - Dynamic resource allocation
# - Minimum cost problems in algorithms

# Function to compute minimum coins required
coin_change <- function(coins, amount) {
  #' Compute minimum number of coins needed to make the given amount
  #' @param coins: Numeric vector of coin denominations
  #' @param amount: Total amount to make
  #' @return: Minimum number of coins required, or -1 if not possible
  
  # Initialize DP array
  dp <- rep(Inf, amount + 1)
  dp[1] <- 0
  dp[0 + 1] <- 0  # zero coins needed for amount 0
  
  for (i in 1:amount) {
    for (coin in coins) {
      if (coin <= i) {
        dp[i + 1] <- min(dp[i + 1], 1 + dp[i - coin + 1])
      }
    }
  }
  
  if (dp[amount + 1] == Inf) {
    return(-1)
  } else {
    return(dp[amount + 1])
  }
}

# Function to print the DP table (for educational purposes)
print_coin_change_dp <- function(dp, amount) {
  cat("DP Table for Coin Change:\n")
  for (i in 0:amount) {
    cat(sprintf("Amount %2d: %s\n", i, dp[i + 1]))
  }
  cat("\n")
}


# Example Usage & Testing
cat("=== Coin Change Problem ===\n\n")

# Test 1: Basic Example
coins <- c(1, 2, 5)
amount <- 11
cat("Coins:", paste(coins, collapse = ", "), "\n")
cat("Amount:", amount, "\n")
min_coins <- coin_change(coins, amount)
cat("Minimum Coins Needed:", min_coins, "\n\n")

# Test 2: No solution case
coins <- c(2, 4)
amount <- 7
cat("Coins:", paste(coins, collapse = ", "), "\n")
cat("Amount:", amount, "\n")
min_coins <- coin_change(coins, amount)
cat("Minimum Coins Needed:", min_coins, "\n\n")

# Test 3: Larger dataset
coins <- c(1, 3, 4, 5)
amount <- 7
cat("Coins:", paste(coins, collapse = ", "), "\n")
cat("Amount:", amount, "\n")
min_coins <- coin_change(coins, amount)
cat("Minimum Coins Needed:", min_coins, "\n\n")
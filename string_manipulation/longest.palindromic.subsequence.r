# Longest Palindromic Subsequence in R
# Author: sgindeed
# Description: Finds and prints the longest palindromic subsequence and its length

# Ask for user input
input.string <- readline(prompt = "Enter a string: ")

# Convert string to lowercase for consistency
clean.string <- tolower(input.string)

# Get length of string
n <- nchar(clean.string)

# Split string into characters
chars <- strsplit(clean.string, "")[[1]]

# Initialize DP table for lengths
dp <- matrix(0, nrow = n, ncol = n)

# Each single character is a palindrome of length 1
for (i in seq_len(n)) {
  dp[i, i] <- 1
}

# Fill the DP table
for (cl in 2:n) {
  for (i in 1:(n - cl + 1)) {
    j <- i + cl - 1
    if (chars[i] == chars[j] && cl == 2) {
      dp[i, j] <- 2
    } else if (chars[i] == chars[j]) {
      dp[i, j] <- dp[i + 1, j - 1] + 2
    } else {
      dp[i, j] <- max(dp[i + 1, j], dp[i, j - 1])
    }
  }
}

# Function to reconstruct the subsequence
reconstructLPS <- function(chars, dp, i, j) {
  if (i > j) {
    return("")
  }
  if (i == j) {
    return(chars[i])
  }
  if (chars[i] == chars[j]) {
    return(paste0(chars[i], reconstructLPS(chars, dp, i + 1, j - 1), chars[j]))
  }
  if (dp[i + 1, j] > dp[i, j - 1]) {
    return(reconstructLPS(chars, dp, i + 1, j))
  } else {
    return(reconstructLPS(chars, dp, i, j - 1))
  }
}

# Get the longest palindromic subsequence
lps <- reconstructLPS(chars, dp, 1, n)

# Display the result
cat("Longest Palindromic Subsequence:", lps, "\n")
cat("Length:", nchar(lps), "\n")

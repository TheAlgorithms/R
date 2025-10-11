# Shortest Common Supersequence (SCS) in R
# Finds the shortest string that contains both input strings as subsequences.

shortest_common_supersequence <- function(X, Y) {
  m <- nchar(X)
  n <- nchar(Y)
  
  # Initialize DP table for Longest Common Subsequence (LCS)
  dp <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  for (i in 1:m) {
    for (j in 1:n) {
      if (substr(X, i, i) == substr(Y, j, j)) {
        dp[i + 1, j + 1] <- dp[i, j] + 1
      } else {
        dp[i + 1, j + 1] <- max(dp[i, j + 1], dp[i + 1, j])
      }
    }
  }
  
  # Backtrack to build the SCS
  i <- m; j <- n
  scs <- ""
  while (i > 0 && j > 0) {
    if (substr(X, i, i) == substr(Y, j, j)) {
      scs <- paste0(substr(X, i, i), scs)
      i <- i - 1
      j <- j - 1
    } else if (dp[i, j + 1] > dp[i + 1, j]) {
      scs <- paste0(substr(X, i, i), scs)
      i <- i - 1
    } else {
      scs <- paste0(substr(Y, j, j), scs)
      j <- j - 1
    }
  }
  
  # Add remaining characters from X or Y
  while (i > 0) {
    scs <- paste0(substr(X, i, i), scs)
    i <- i - 1
  }
  while (j > 0) {
    scs <- paste0(substr(Y, j, j), scs)
    j <- j - 1
  }
  
  return(list(length = nchar(scs), supersequence = scs))
}

# Interactive input
X <- tolower(readline("Enter first string: "))
Y <- tolower(readline("Enter second string: "))

result <- shortest_common_supersequence(X, Y)
cat("Length of SCS:", result$length, "\n")
cat("Shortest Common Supersequence:", result$supersequence, "\n")

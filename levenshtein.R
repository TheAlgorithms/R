# Levenshtein Distance in R
# Calculates minimum number of edits (insert, delete, replace)
# to convert one string into another and prints the distance.

levenshtein_distance <- function(s1, s2) {
  n <- nchar(s1)
  m <- nchar(s2)
  
  # Initialize DP matrix
  dp <- matrix(0, nrow = n + 1, ncol = m + 1)
  
  for (i in 0:n) dp[i + 1, 1] <- i
  for (j in 0:m) dp[1, j + 1] <- j
  
  # Fill DP table
  for (i in 1:n) {
    for (j in 1:m) {
      if (substr(s1, i, i) == substr(s2, j, j)) {
        dp[i + 1, j + 1] <- dp[i, j]
      } else {
        dp[i + 1, j + 1] <- min(
          dp[i, j + 1] + 1,   # Deletion
          dp[i + 1, j] + 1,   # Insertion
          dp[i, j] + 1        # Substitution
        )
      }
    }
  }
  
  return(dp[n + 1, m + 1])
}

# Interactive input
s1 <- tolower(readline("Enter first string: "))
s2 <- tolower(readline("Enter second string: "))

distance <- levenshtein_distance(s1, s2)
cat("Levenshtein distance between '", s1, "' and '", s2, "' is:", distance, "\n")
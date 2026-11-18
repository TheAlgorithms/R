# Levenshtein Distance in R
#
# Calculates the minimum number of edits needed to transform
# one string into another.
#
# Allowed operations:
# - insertion
# - deletion
# - substitution

levenshtein_distance <- function(s1, s2) {
  if (length(s1) != 1 || length(s2) != 1) {
    stop("s1 and s2 must each contain exactly one string")
  }
  
  s1 <- as.character(s1)
  s2 <- as.character(s2)
  
  n <- nchar(s1)
  m <- nchar(s2)
  
  dp <- matrix(
    0,
    nrow = n + 1,
    ncol = m + 1
  )
  
  for (i in 0:n) {
    dp[i + 1, 1] <- i
  }
  
  for (j in 0:m) {
    dp[1, j + 1] <- j
  }
  
  # Avoid 1:0 when either string is empty.
  if (n > 0 && m > 0) {
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        substitution_cost <- if (
          substr(s1, i, i) == substr(s2, j, j)
        ) {
          0
        } else {
          1
        }
        
        dp[i + 1, j + 1] <- min(
          dp[i, j + 1] + 1,
          dp[i + 1, j] + 1,
          dp[i, j] + substitution_cost
        )
      }
    }
  }
  
  dp[n + 1, m + 1]
}

# Interactive example.
# This does not run when the file is sourced for testing.
if (interactive()) {
  s1 <- tolower(readline("Enter first string: "))
  s2 <- tolower(readline("Enter second string: "))
  
  distance <- levenshtein_distance(s1, s2)
  
  cat(
    "Levenshtein distance between '",
    s1,
    "' and '",
    s2,
    "' is: ",
    distance,
    "\n",
    sep = ""
  )
}
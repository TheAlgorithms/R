# Z-Algorithm for Pattern Searching in R
# Finds all occurrences of a pattern in a text efficiently in O(n + m).

z_algorithm_search <- function(text, pattern) {
  concat <- paste0(pattern, "$", text)
  n <- nchar(concat)
  Z <- integer(n)
  
  L <- 0
  R <- 0
  for (i in 2:n) {
    if (i <= R)
      Z[i] <- min(R - i + 1, Z[i - L + 1])
    
    while (i + Z[i] <= n && substr(concat, Z[i] + 1, Z[i] + 1) ==
           substr(concat, i + Z[i], i + Z[i]))
      Z[i] <- Z[i] + 1
    
    if (i + Z[i] - 1 > R) {
      L <- i
      R <- i + Z[i] - 1
    }
  }
  
  positions <- which(Z > nchar(pattern))
  matches <- positions - nchar(pattern) - 1
  return(matches)
}

text <- tolower(readline("Enter text: "))
pattern <- tolower(readline("Enter pattern: "))

matches <- z_algorithm_search(text, pattern)
if (length(matches) > 0)
  cat("Pattern found at positions:", matches, "\n")
else
  cat("Pattern not found.\n")
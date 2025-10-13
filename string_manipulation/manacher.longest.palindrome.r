# Manacher's Algorithm – Longest Palindromic Substring in R

longest_palindrome <- function(s) {
  T <- paste0("^#", paste(unlist(strsplit(s, "")), collapse = "#"), "#$")
  n <- nchar(T)
  P <- integer(n)
  center <- 0
  right <- 0
  
  for (i in 2:(n - 1)) {
    mirror <- 2 * center - i
    if (i < right)
      P[i] <- min(right - i, P[mirror])
    
    while (substr(T, i + (1 + P[i]), i + (1 + P[i])) ==
           substr(T, i - (1 + P[i]), i - (1 + P[i])))
      P[i] <- P[i] + 1
    
    if (i + P[i] > right) {
      center <- i
      right <- i + P[i]
    }
  }
  
  max_len <- max(P)
  center_index <- which.max(P)
  start <- (center_index - max_len) / 2
  palindrome <- substr(s, start + 1, start + max_len)
  return(palindrome)
}

s <- tolower(readline("Enter a string: "))
result <- longest_palindrome(s)
cat("Longest Palindromic Substring:", result, "\n")
#' Computes the minimum number of characters to insert to make a string a palindrome.
#'
#' @param s A character string (case-insensitive).
#' @return A list containing:
#'   \item{min_insertions}{Minimum number of insertions required.}
#'   \item{palindrome}{One possible resulting palindrome.}
#' @details
#' Uses dynamic programming to compute the minimum insertions in O(n^2) time and space.
#' Also reconstructs one possible palindrome.
#'
#' @examples
#' min_palindrome_insertion("race")
#' # Returns 3 insertions and palindrome "ecarace"
# Minimum Palindrome Insertion in R
# Computes the minimum number of characters to insert to make a string a palindrome
# Also prints one possible resulting palindrome.

min_palindrome_insertion <- function(s) {
  n <- nchar(s)
  chars <- strsplit(s, "")[[1]]
  
  # DP table to store minimum insertions for substring s[i..j]
  dp <- matrix(0, nrow = n, ncol = n)
  
  # Fill the table
  for (length_sub in 2:n) {
    for (i in 1:(n - length_sub + 1)) {
      j <- i + length_sub - 1
      if (chars[i] == chars[j]) {
        dp[i, j] <- dp[i + 1, j - 1]
      } else {
        dp[i, j] <- min(dp[i + 1, j], dp[i, j - 1]) + 1
      }
    }
  }
  
  # Reconstruct one possible palindrome
  i <- 1
  j <- n
  length_sub <- dp[1, n] + n
  res <- character(length_sub)
  left <- 1
  right <- length(res)
  
  while (i <= j) {
    if (chars[i] == chars[j]) {
      res[left] <- chars[i]
      res[right] <- chars[j]
      i <- i + 1
      j <- j - 1
    } else if (dp[i + 1, j] < dp[i, j - 1]) {
      res[left] <- chars[i]
      res[right] <- chars[i]
      i <- i + 1
    } else {
      res[left] <- chars[j]
      res[right] <- chars[j]
      j <- j - 1
    }
    left <- left + 1
    right <- right - 1
  }
  
  list(min_insertions = dp[1, n], palindrome = paste(res, collapse = ""))
}

# --- Interactive input ---
s <- tolower(readline("Enter a string: "))

result <- min_palindrome_insertion(s)
cat("Minimum insertions required:", result$min_insertions, "\n")
cat("One possible resulting palindrome:", result$palindrome, "\n")

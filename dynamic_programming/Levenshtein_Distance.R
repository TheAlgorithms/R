# Levenshtein Distance Algorithm
#
# The Levenshtein distance measures the difference between two strings by
# counting the minimum number of single-character edits required to
# change one string into the other.
#
# Time Complexity: O(m * n) where m, n are the lengths of the strings
# Space Complexity: O(m * n) for the DP table
#
# Applications:
# - Spell checkers and auto-correct
# - Fuzzy string searching in databases
# - DNA sequence analysis in bioinformatics
# - Plagiarism and duplicate content detection
# - Optical Character Recognition (OCR) correction

levenshtein_distance <- function(s1, s2) {
  #' Calculate the Levenshtein distance between two strings
  #' @param s1: First string
  #' @param s2: Second string
  #' @return: The integer Levenshtein distance
  
  # Get the length of the strings
  len1 <- nchar(s1)
  len2 <- nchar(s2)
  
  # Create a matrix to store the distances
  dist_matrix <- matrix(0, nrow = len1 + 1, ncol = len2 + 1)
  
  # Initialize the first row and column
  # This represents the cost of transforming a prefix to an empty string
  for (i in 1:(len1 + 1)) {
    dist_matrix[i, 1] <- i - 1
  }
  for (j in 1:(len2 + 1)) {
    dist_matrix[1, j] <- j - 1
  }
  
  # Fill the rest of the matrix
  for (j in 2:(len2 + 1)) {
    for (i in 2:(len1 + 1)) {
      # Check if the characters at the current position are the same
      # The cost of substitution is 0 if they are, 1 otherwise
      substitution_cost <- if (substr(s1, i - 1, i - 1) == substr(s2, j - 1, j - 1)) 0 else 1
      
      # Calculate the costs of insertion, deletion, and substitution
      deletion <- dist_matrix[i - 1, j] + 1
      insertion <- dist_matrix[i, j - 1] + 1
      substitution <- dist_matrix[i - 1, j - 1] + substitution_cost
      
      # The value of the current cell is the minimum of these three costs
      dist_matrix[i, j] <- min(deletion, insertion, substitution)
    }
  }
  
  # The final distance is in the bottom-right corner of the matrix
  return(dist_matrix[len1 + 1, len2 + 1])
}

# Example usage and testing
cat("=== Levenshtein Distance Algorithm ===\n\n")

# Test 1: Classic example
cat("1. Classic Example\n")
str1 <- "saturday"
str2 <- "sunday"
cat("String 1:", str1, "\n")
cat("String 2:", str2, "\n")
cat("Distance:", levenshtein_distance(str1, str2), "\n\n")

# Test 2: Another common example
cat("2. Common Example\n")
str1 <- "kitten"
str2 <- "sitting"
cat("String 1:", str1, "\n")
cat("String 2:", str2, "\n")
cat("Distance:", levenshtein_distance(str1, str2), "\n\n")

# Test 3: Edge cases
cat("3. Edge Cases\n")
cat("Distance between 'apple' and '':", levenshtein_distance("apple", ""), "\n")
cat("Distance between '' and 'banana':", levenshtein_distance("", "banana"), "\n")
cat("Distance between 'book' and 'book':", levenshtein_distance("book", "book"), "\n")
cat("Distance between 'car' and 'bus':", levenshtein_distance("car", "bus"), "\n\n")

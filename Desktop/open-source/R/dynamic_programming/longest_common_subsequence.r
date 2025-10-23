# Longest Common Subsequence (LCS) Algorithm
#
# The LCS problem finds the longest subsequence common to two sequences.
# A subsequence is a sequence derived from another sequence by deleting some 
# or no elements without changing the order of the remaining elements.
#
# Time Complexity: O(m * n) where m, n are lengths of the sequences
# Space Complexity: O(m * n) for the DP table, O(min(m, n)) optimized version
#
# Applications:
# - DNA sequence analysis in bioinformatics
# - File difference utilities (diff command)  
# - Version control systems (git diff)
# - Plagiarism detection
# - Data compression algorithms
# - Edit distance calculations

# Basic LCS algorithm with full DP table
lcs_length <- function(str1, str2) {
  #' Find the length of longest common subsequence
  #' @param str1: First string
  #' @param str2: Second string  
  #' @return: Length of LCS
  
  m <- nchar(str1)
  n <- nchar(str2)
  
  # Create DP table
  dp <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Fill the DP table
  for (i in 1:(m + 1)) {
    for (j in 1:(n + 1)) {
      if (i == 1 || j == 1) {
        dp[i, j] <- 0
      } else if (substr(str1, i - 1, i - 1) == substr(str2, j - 1, j - 1)) {
        dp[i, j] <- dp[i - 1, j - 1] + 1
      } else {
        dp[i, j] <- max(dp[i - 1, j], dp[i, j - 1])
      }
    }
  }
  
  return(dp[m + 1, n + 1])
}

# LCS algorithm that returns the actual subsequence
lcs_string <- function(str1, str2) {
  #' Find the longest common subsequence string
  #' @param str1: First string
  #' @param str2: Second string
  #' @return: List containing LCS string and its length
  
  m <- nchar(str1)
  n <- nchar(str2)
  
  # Create DP table
  dp <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Fill the DP table
  for (i in 1:(m + 1)) {
    for (j in 1:(n + 1)) {
      if (i == 1 || j == 1) {
        dp[i, j] <- 0
      } else if (substr(str1, i - 1, i - 1) == substr(str2, j - 1, j - 1)) {
        dp[i, j] <- dp[i - 1, j - 1] + 1
      } else {
        dp[i, j] <- max(dp[i - 1, j], dp[i, j - 1])
      }
    }
  }
  
  # Backtrack to find the actual LCS string
  lcs <- ""
  i <- m + 1
  j <- n + 1
  
  while (i > 1 && j > 1) {
    if (substr(str1, i - 1, i - 1) == substr(str2, j - 1, j - 1)) {
      lcs <- paste0(substr(str1, i - 1, i - 1), lcs)
      i <- i - 1
      j <- j - 1
    } else if (dp[i - 1, j] > dp[i, j - 1]) {
      i <- i - 1
    } else {
      j <- j - 1
    }
  }
  
  return(list(
    lcs = lcs,
    length = dp[m + 1, n + 1],
    dp_table = dp
  ))
}

# Space-optimized LCS (only returns length)
lcs_length_optimized <- function(str1, str2) {
  #' Space-optimized LCS length calculation
  #' Uses only O(min(m, n)) space instead of O(m * n)
  #' @param str1: First string
  #' @param str2: Second string
  #' @return: Length of LCS
  
  m <- nchar(str1)
  n <- nchar(str2)
  
  # Make str1 the shorter string for space optimization
  if (m > n) {
    temp <- str1
    str1 <- str2
    str2 <- temp
    temp <- m
    m <- n
    n <- temp
  }
  
  # Use two arrays instead of full matrix
  prev <- rep(0, m + 1)
  curr <- rep(0, m + 1)
  
  for (j in 1:(n + 1)) {
    for (i in 1:(m + 1)) {
      if (i == 1 || j == 1) {
        curr[i] <- 0
      } else if (substr(str1, i - 1, i - 1) == substr(str2, j - 1, j - 1)) {
        curr[i] <- prev[i - 1] + 1
      } else {
        curr[i] <- max(prev[i], curr[i - 1])
      }
    }
    # Swap arrays
    temp <- prev
    prev <- curr
    curr <- temp
  }
  
  return(prev[m + 1])
}

# Find all possible LCS strings (there can be multiple)
find_all_lcs <- function(str1, str2) {
  #' Find all possible longest common subsequences
  #' @param str1: First string
  #' @param str2: Second string
  #' @return: List of all LCS strings
  
  m <- nchar(str1)
  n <- nchar(str2)
  
  # Create DP table
  dp <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Fill the DP table
  for (i in 1:(m + 1)) {
    for (j in 1:(n + 1)) {
      if (i == 1 || j == 1) {
        dp[i, j] <- 0
      } else if (substr(str1, i - 1, i - 1) == substr(str2, j - 1, j - 1)) {
        dp[i, j] <- dp[i - 1, j - 1] + 1
      } else {
        dp[i, j] <- max(dp[i - 1, j], dp[i, j - 1])
      }
    }
  }
  
  # Recursive function to find all LCS
  find_all_lcs_recursive <- function(i, j) {
    if (i == 1 || j == 1) {
      return("")
    }
    
    if (substr(str1, i - 1, i - 1) == substr(str2, j - 1, j - 1)) {
      char <- substr(str1, i - 1, i - 1)
      prev_lcs <- find_all_lcs_recursive(i - 1, j - 1)
      return(paste0(prev_lcs, char))
    } else {
      results <- c()
      
      if (dp[i - 1, j] == dp[i, j]) {
        results <- c(results, find_all_lcs_recursive(i - 1, j))
      }
      
      if (dp[i, j - 1] == dp[i, j]) {
        results <- c(results, find_all_lcs_recursive(i, j - 1))
      }
      
      return(unique(results))
    }
  }
  
  all_lcs <- find_all_lcs_recursive(m + 1, n + 1)
  return(unique(all_lcs))
}

# LCS for arrays/vectors instead of strings
lcs_array <- function(arr1, arr2) {
  #' Find LCS of two arrays/vectors
  #' @param arr1: First array
  #' @param arr2: Second array
  #' @return: List with LCS array and length
  
  m <- length(arr1)
  n <- length(arr2)
  
  # Create DP table
  dp <- matrix(0, nrow = m + 1, ncol = n + 1)
  
  # Fill the DP table
  for (i in 1:(m + 1)) {
    for (j in 1:(n + 1)) {
      if (i == 1 || j == 1) {
        dp[i, j] <- 0
      } else if (arr1[i - 1] == arr2[j - 1]) {
        dp[i, j] <- dp[i - 1, j - 1] + 1
      } else {
        dp[i, j] <- max(dp[i - 1, j], dp[i, j - 1])
      }
    }
  }
  
  # Backtrack to find the actual LCS
  lcs <- c()
  i <- m + 1
  j <- n + 1
  
  while (i > 1 && j > 1) {
    if (arr1[i - 1] == arr2[j - 1]) {
      lcs <- c(arr1[i - 1], lcs)
      i <- i - 1
      j <- j - 1
    } else if (dp[i - 1, j] > dp[i, j - 1]) {
      i <- i - 1
    } else {
      j <- j - 1
    }
  }
  
  return(list(
    lcs = lcs,
    length = dp[m + 1, n + 1]
  ))
}

# Function to print the DP table (for educational purposes)
print_dp_table <- function(str1, str2, dp_table) {
  #' Print the DP table in a readable format
  #' @param str1: First string
  #' @param str2: Second string
  #' @param dp_table: DP table from lcs_string function
  
  m <- nchar(str1)
  n <- nchar(str2)
  
  cat("DP Table for LCS calculation:\n")
  cat("String 1:", str1, "\n")
  cat("String 2:", str2, "\n\n")
  
  # Print column headers
  cat("     ε ")
  for (j in 1:n) {
    cat(sprintf("%2s ", substr(str2, j, j)))
  }
  cat("\n")
  
  # Print table rows
  for (i in 1:(m + 1)) {
    if (i == 1) {
      cat("  ε  ")
    } else {
      cat(sprintf("%2s   ", substr(str1, i - 1, i - 1)))
    }
    
    for (j in 1:(n + 1)) {
      cat(sprintf("%2d ", dp_table[i, j]))
    }
    cat("\n")
  }
  cat("\n")
}

# Example usage and testing
cat("=== Longest Common Subsequence (LCS) Algorithm ===\n\n")

# Test 1: Basic LCS example
cat("1. Basic LCS Example\n")
str1 <- "ABCDGH"
str2 <- "AEDFHR"

cat("String 1:", str1, "\n")
cat("String 2:", str2, "\n")

result1 <- lcs_string(str1, str2)
cat("LCS:", result1$lcs, "\n")
cat("Length:", result1$length, "\n")

print_dp_table(str1, str2, result1$dp_table)

# Test 2: DNA sequence analysis
cat("2. DNA Sequence Analysis\n")
dna1 <- "ATCGATCGATCG"
dna2 <- "ATGCGATGCATG"

cat("DNA Sequence 1:", dna1, "\n")
cat("DNA Sequence 2:", dna2, "\n")

dna_result <- lcs_string(dna1, dna2)
cat("Common subsequence:", dna_result$lcs, "\n")
cat("Length:", dna_result$length, "\n")
cat("Similarity:", sprintf("%.2f%%", dna_result$length / max(nchar(dna1), nchar(dna2)) * 100), "\n\n")

# Test 3: Finding all possible LCS
cat("3. Multiple LCS Solutions\n")
str3 <- "ABCDEF"
str4 <- "ACBDEF"

cat("String 1:", str3, "\n")
cat("String 2:", str4, "\n")

all_lcs <- find_all_lcs(str3, str4)
cat("All possible LCS:\n")
for (i in seq_along(all_lcs)) {
  cat("  ", i, ":", all_lcs[i], "\n")
}
cat("\n")

# Test 4: Array LCS example
cat("4. Array LCS Example\n")
arr1 <- c(1, 2, 3, 4, 5)
arr2 <- c(2, 3, 5, 7, 8)

cat("Array 1:", paste(arr1, collapse = ", "), "\n")
cat("Array 2:", paste(arr2, collapse = ", "), "\n")

arr_result <- lcs_array(arr1, arr2)
cat("LCS Array:", paste(arr_result$lcs, collapse = ", "), "\n")
cat("Length:", arr_result$length, "\n\n")

# Test 5: Performance comparison
cat("5. Performance Comparison\n")
long_str1 <- paste(sample(LETTERS[1:5], 100, replace = TRUE), collapse = "")
long_str2 <- paste(sample(LETTERS[1:5], 100, replace = TRUE), collapse = "")

cat("Testing with strings of length 100...\n")

# Standard algorithm
start_time <- Sys.time()
standard_result <- lcs_length(long_str1, long_str2)
standard_time <- as.numeric(Sys.time() - start_time, units = "secs")

# Optimized algorithm  
start_time <- Sys.time()
optimized_result <- lcs_length_optimized(long_str1, long_str2)
optimized_time <- as.numeric(Sys.time() - start_time, units = "secs")

cat("Standard algorithm: LCS length =", standard_result, 
    "Time:", sprintf("%.6f", standard_time), "seconds\n")
cat("Optimized algorithm: LCS length =", optimized_result, 
    "Time:", sprintf("%.6f", optimized_time), "seconds\n")
cat("Results match:", standard_result == optimized_result, "\n\n")

# Test 6: Edge cases
cat("6. Edge Cases\n")
cat("Empty strings:", lcs_length("", "ABC"), "\n")
cat("One empty string:", lcs_length("ABC", ""), "\n")
cat("Identical strings:", lcs_length("HELLO", "HELLO"), "\n")
cat("No common characters:", lcs_length("ABC", "DEF"), "\n")
cat("Single character:", lcs_length("A", "A"), "\n")
cat("Single vs multiple:", lcs_length("A", "ABCDEF"), "\n\n")

# Test 7: Real-world example - File diff simulation
cat("7. File Diff Simulation\n")
file1_lines <- c("Hello World", "This is line 2", "Line 3 here", "Final line")
file2_lines <- c("Hello World", "This is modified line 2", "Line 3 here", "New line", "Final line")

cat("File 1 lines:\n")
for (i in seq_along(file1_lines)) {
  cat(" ", i, ":", file1_lines[i], "\n")
}

cat("File 2 lines:\n")
for (i in seq_along(file2_lines)) {
  cat(" ", i, ":", file2_lines[i], "\n")
}

file_lcs <- lcs_array(file1_lines, file2_lines)
cat("Common lines (unchanged):\n")
for (i in seq_along(file_lcs$lcs)) {
  cat(" ", file_lcs$lcs[i], "\n")
}
cat("Similarity:", sprintf("%.1f%%", file_lcs$length / max(length(file1_lines), length(file2_lines)) * 100), "\n")
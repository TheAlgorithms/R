# Knuth-Morris-Pratt (KMP) String Matching Algorithm
#
# The KMP algorithm efficiently finds all occurrences of a pattern in a text
# by preprocessing the pattern to avoid unnecessary character comparisons.
# It uses a "failure function" (also called "prefix function") to skip characters
# intelligently when a mismatch occurs.
#
# Time Complexity: O(n + m) where n = text length, m = pattern length
# Space Complexity: O(m) for the failure function array
# 
# This is a significant improvement over naive string matching O(n*m)
#
# Applications:
# - Text editors (find/replace functionality)
# - DNA sequence analysis in bioinformatics  
# - Plagiarism detection systems
# - Web search engines

# Function to compute the failure function (prefix function)
compute_failure_function <- function(pattern) {
  #' Compute the failure function for KMP algorithm
  #' @param pattern: Pattern string to preprocess
  #' @return: Vector of failure function values
  
  m <- nchar(pattern)
  failure <- rep(0, m)
  
  # failure[1] is always 0 (single character has no proper prefix/suffix)
  if (m == 0) return(integer(0))
  if (m == 1) return(0)
  
  j <- 0  # Length of previous longest prefix suffix
  
  for (i in 2:m) {
    # Get current character
    curr_char <- substr(pattern, i, i)
    
    # Handle mismatches by following failure links
    while (j > 0 && substr(pattern, j + 1, j + 1) != curr_char) {
      j <- failure[j]
    }
    
    # If characters match, increment j
    if (substr(pattern, j + 1, j + 1) == curr_char) {
      j <- j + 1
    }
    
    failure[i] <- j
  }
  
  return(failure)
}

# Main KMP string matching algorithm
kmp_search <- function(text, pattern) {
  #' Find all occurrences of pattern in text using KMP algorithm
  #' @param text: Text string to search in
  #' @param pattern: Pattern string to search for
  #' @return: Vector of starting positions (1-indexed) where pattern occurs
  
  n <- nchar(text)
  m <- nchar(pattern)
  
  # Handle edge cases
  if (m == 0) return(integer(0))
  if (n == 0 || m > n) return(integer(0))
  
  # Precompute failure function
  failure <- compute_failure_function(pattern)
  
  matches <- c()
  j <- 0  # Index for pattern
  
  for (i in 1:n) {
    # Get current character from text
    curr_char <- substr(text, i, i)
    
    # Handle mismatches using failure function
    while (j > 0 && substr(pattern, j + 1, j + 1) != curr_char) {
      j <- failure[j]
    }
    
    # If characters match, advance pattern index
    if (substr(pattern, j + 1, j + 1) == curr_char) {
      j <- j + 1
    }
    
    # Check for complete pattern match
    if (j == m) {
      matches <- c(matches, i - m + 1)  # 1-indexed position
      j <- failure[j]  # Prepare for next potential match
    }
  }
  
  return(matches)
}

# Function to find first occurrence only (more efficient)
kmp_search_first <- function(text, pattern) {
  #' Find first occurrence of pattern in text using KMP algorithm
  #' @param text: Text string to search in
  #' @param pattern: Pattern string to search for  
  #' @return: Starting position (1-indexed) of first match, or -1 if not found
  
  n <- nchar(text)
  m <- nchar(pattern)
  
  # Handle edge cases
  if (m == 0) return(-1)
  if (n == 0 || m > n) return(-1)
  
  # Precompute failure function
  failure <- compute_failure_function(pattern)
  
  j <- 0  # Index for pattern
  
  for (i in 1:n) {
    # Get current character from text
    curr_char <- substr(text, i, i)
    
    # Handle mismatches using failure function
    while (j > 0 && substr(pattern, j + 1, j + 1) != curr_char) {
      j <- failure[j]
    }
    
    # If characters match, advance pattern index
    if (substr(pattern, j + 1, j + 1) == curr_char) {
      j <- j + 1
    }
    
    # Check for complete pattern match
    if (j == m) {
      return(i - m + 1)  # Return 1-indexed position
    }
  }
  
  return(-1)  # Pattern not found
}

# Function to count occurrences without storing positions
kmp_count <- function(text, pattern) {
  #' Count occurrences of pattern in text using KMP algorithm
  #' @param text: Text string to search in
  #' @param pattern: Pattern string to search for
  #' @return: Number of occurrences
  
  n <- nchar(text)
  m <- nchar(pattern)
  
  # Handle edge cases
  if (m == 0) return(0)
  if (n == 0 || m > n) return(0)
  
  # Precompute failure function
  failure <- compute_failure_function(pattern)
  
  count <- 0
  j <- 0  # Index for pattern
  
  for (i in 1:n) {
    # Get current character from text
    curr_char <- substr(text, i, i)
    
    # Handle mismatches using failure function
    while (j > 0 && substr(pattern, j + 1, j + 1) != curr_char) {
      j <- failure[j]
    }
    
    # If characters match, advance pattern index
    if (substr(pattern, j + 1, j + 1) == curr_char) {
      j <- j + 1
    }
    
    # Check for complete pattern match
    if (j == m) {
      count <- count + 1
      j <- failure[j]  # Prepare for next potential match
    }
  }
  
  return(count)
}

# Naive string matching for comparison
naive_search <- function(text, pattern) {
  #' Naive string matching algorithm for performance comparison
  #' @param text: Text string to search in
  #' @param pattern: Pattern string to search for
  #' @return: Vector of starting positions where pattern occurs
  
  n <- nchar(text)
  m <- nchar(pattern)
  matches <- c()
  
  if (m == 0 || n == 0 || m > n) return(matches)
  
  for (i in 1:(n - m + 1)) {
    if (substr(text, i, i + m - 1) == pattern) {
      matches <- c(matches, i)
    }
  }
  
  return(matches)
}

# Function to visualize the failure function
visualize_failure_function <- function(pattern) {
  #' Print a visual representation of the failure function
  #' @param pattern: Pattern to analyze
  
  failure <- compute_failure_function(pattern)
  m <- nchar(pattern)
  
  cat("Pattern:  ")
  for (i in 1:m) {
    cat(sprintf("%2s ", substr(pattern, i, i)))
  }
  cat("\n")
  
  cat("Index:    ")
  for (i in 1:m) {
    cat(sprintf("%2d ", i))
  }
  cat("\n")
  
  cat("Failure:  ")
  for (i in 1:m) {
    cat(sprintf("%2d ", failure[i]))
  }
  cat("\n\n")
}

# Example usage and testing
cat("=== Knuth-Morris-Pratt (KMP) String Matching Algorithm ===\n\n")

# Test 1: Basic pattern matching
cat("1. Basic Pattern Matching\n")
text1 <- "ABABDABACDABABCABCABCABCABC"
pattern1 <- "ABC"

cat("Text:    ", text1, "\n")
cat("Pattern: ", pattern1, "\n")

matches1 <- kmp_search(text1, pattern1)
cat("KMP matches at positions:", paste(matches1, collapse = ", "), "\n")

# Verify with naive approach
naive_matches1 <- naive_search(text1, pattern1)
cat("Naive matches at positions:", paste(naive_matches1, collapse = ", "), "\n")
cat("Results match:", identical(matches1, naive_matches1), "\n\n")

# Test 2: Pattern with repeating characters
cat("2. Pattern with Repeating Characters\n")
text2 <- "AABAACAADAABAABA"
pattern2 <- "AABA"

cat("Text:    ", text2, "\n")
cat("Pattern: ", pattern2, "\n")

visualize_failure_function(pattern2)

matches2 <- kmp_search(text2, pattern2)
cat("Matches at positions:", paste(matches2, collapse = ", "), "\n")

# Show the actual matches
for (pos in matches2) {
  match_str <- substr(text2, pos, pos + nchar(pattern2) - 1)
  cat("Position", pos, ":", match_str, "\n")
}
cat("\n")

# Test 3: Edge cases
cat("3. Edge Cases\n")
cat("Empty pattern:", length(kmp_search("hello", "")), "matches\n")
cat("Empty text:", length(kmp_search("", "hello")), "matches\n") 
cat("Pattern longer than text:", length(kmp_search("hi", "hello")), "matches\n")
cat("Single character pattern:", kmp_search("abcabc", "a"), "\n")
cat("Pattern not in text:", kmp_search("hello world", "xyz"), "\n\n")

# Test 4: Performance comparison
cat("4. Performance Comparison\n")
# Create a text with many potential false matches
repeated_text <- paste(rep("AAAAB", 200), collapse = "")
repeated_pattern <- "AAAAB"

cat("Text length:", nchar(repeated_text), "\n")
cat("Pattern:", repeated_pattern, "\n")

# Time the KMP algorithm
start_time <- Sys.time()
kmp_result <- kmp_search(repeated_text, repeated_pattern)
kmp_time <- as.numeric(Sys.time() - start_time, units = "secs")

# Time the naive algorithm
start_time <- Sys.time()
naive_result <- naive_search(repeated_text, repeated_pattern)
naive_time <- as.numeric(Sys.time() - start_time, units = "secs")

cat("KMP found", length(kmp_result), "matches in", sprintf("%.6f", kmp_time), "seconds\n")
cat("Naive found", length(naive_result), "matches in", sprintf("%.6f", naive_time), "seconds\n")

if (naive_time > 0 && kmp_time > 0) {
  speedup <- naive_time / kmp_time
  cat("KMP speedup:", sprintf("%.2f", speedup), "x faster\n")
}
cat("\n")

# Test 5: Real-world example - DNA sequence matching
cat("5. DNA Sequence Matching Example\n")
dna_sequence <- "ATCGATCGATCGAATCGATCGATCGAATCGATCG"
dna_pattern <- "ATCG"

cat("DNA Sequence:", dna_sequence, "\n")
cat("Pattern:     ", dna_pattern, "\n")

dna_matches <- kmp_search(dna_sequence, dna_pattern)
cat("Pattern occurs at positions:", paste(dna_matches, collapse = ", "), "\n")
cat("Total occurrences:", length(dna_matches), "\n\n")

# Test 6: Failure function examples
cat("6. Failure Function Examples\n")
patterns <- c("ABCABCAB", "AAAA", "ABCDE", "ABABABAB")

for (pattern in patterns) {
  cat("Pattern:", pattern, "\n")
  visualize_failure_function(pattern)
}

# Test 7: Case sensitivity
cat("7. Case Sensitivity\n")
text_case <- "Hello World Hello"
pattern_case <- "hello"
matches_case <- kmp_search(text_case, pattern_case)
cat("Text:", text_case, "\n")
cat("Pattern:", pattern_case, "\n")
cat("Matches (case-sensitive):", paste(matches_case, collapse = ", "), "\n")

# Case-insensitive version
matches_insensitive <- kmp_search(tolower(text_case), tolower(pattern_case))
cat("Matches (case-insensitive):", paste(matches_insensitive, collapse = ", "), "\n")
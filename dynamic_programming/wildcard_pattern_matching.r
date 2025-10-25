# Wildcard Pattern Matching using Dynamic Programming
#
# Matches a text string against a pattern containing wildcards:
# '*' matches any sequence of characters (including empty sequence)
# '?' matches exactly one character
#
# Time Complexity: O(m * n) where m is text length, n is pattern length
# Space Complexity: O(m * n) for DP table
#
# Input: text string and pattern string with wildcards
# Output: TRUE if pattern matches text, FALSE otherwise

isMatch <- function(text, pattern) {
  m <- nchar(text)
  n <- nchar(pattern)
  
  # DP table: dp[i][j] = TRUE if text[1:i] matches pattern[1:j]
  dp <- matrix(FALSE, nrow = m + 1, ncol = n + 1)
  
  # Empty pattern matches empty text
  dp[1, 1] <- TRUE
  
  # Handle patterns starting with '*'
  for (j in 2:(n + 1)) {
    if (substr(pattern, j - 1, j - 1) == "*") {
      dp[1, j] <- dp[1, j - 1]
    }
  }
  
  # Fill DP table
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      text_char <- substr(text, i - 1, i - 1)
      pattern_char <- substr(pattern, j - 1, j - 1)
      
      if (pattern_char == "*") {
        # '*' matches empty or any sequence
        dp[i, j] <- dp[i, j - 1] || dp[i - 1, j]
      } else if (pattern_char == "?" || pattern_char == text_char) {
        # '?' or exact match
        dp[i, j] <- dp[i - 1, j - 1]
      }
    }
  }
  
  return(dp[m + 1, n + 1])
}

# Space-optimized version using two rows
isMatch_optimized <- function(text, pattern) {
  m <- nchar(text)
  n <- nchar(pattern)
  
  # Use two rows instead of full table
  prev <- rep(FALSE, n + 1)
  curr <- rep(FALSE, n + 1)
  
  prev[1] <- TRUE
  
  for (j in 2:(n + 1)) {
    if (substr(pattern, j - 1, j - 1) == "*") {
      prev[j] <- prev[j - 1]
    }
  }
  
  # Early return for empty text
  if (m == 0) {
    return(prev[n + 1])
  }
  
  for (i in 2:(m + 1)) {
    curr <- rep(FALSE, n + 1)
    text_char <- substr(text, i - 1, i - 1)
    
    for (j in 2:(n + 1)) {
      pattern_char <- substr(pattern, j - 1, j - 1)
      
      if (pattern_char == "*") {
        curr[j] <- curr[j - 1] || prev[j]
      } else if (pattern_char == "?" || pattern_char == text_char) {
        curr[j] <- prev[j - 1]
      }
    }
    
    prev <- curr
  }
  
  return(curr[n + 1])
}

# Backtracking solution (alternative approach)
isMatch_backtrack <- function(text, pattern) {
  match_helper <- function(t_idx, p_idx) {
    # Base cases
    if (p_idx > nchar(pattern)) {
      return(t_idx > nchar(text))
    }
    
    if (t_idx > nchar(text)) {
      # Check if remaining pattern is all '*'
      while (p_idx <= nchar(pattern)) {
        if (substr(pattern, p_idx, p_idx) != "*") {
          return(FALSE)
        }
        p_idx <- p_idx + 1
      }
      return(TRUE)
    }
    
    pattern_char <- substr(pattern, p_idx, p_idx)
    text_char <- substr(text, t_idx, t_idx)
    
    if (pattern_char == "*") {
      # Try matching empty or any sequence
      return(match_helper(t_idx, p_idx + 1) || match_helper(t_idx + 1, p_idx))
    } else if (pattern_char == "?" || pattern_char == text_char) {
      return(match_helper(t_idx + 1, p_idx + 1))
    } else {
      return(FALSE)
    }
  }
  
  return(match_helper(1, 1))
}

# Find all matching substrings
find_matches <- function(text, pattern) {
  matches <- list()
  n <- nchar(text)
  
  for (start in 1:n) {
    for (end in start:n) {
      substring <- substr(text, start, end)
      if (isMatch(substring, pattern)) {
        matches[[length(matches) + 1]] <- list(
          start = start,
          end = end,
          text = substring
        )
      }
    }
  }
  
  return(matches)
}

# Count matching patterns
count_matches <- function(texts, pattern) {
  count <- 0
  for (text in texts) {
    if (isMatch(text, pattern)) {
      count <- count + 1
    }
  }
  return(count)
}

# Example usage and tests
cat("=== Wildcard Pattern Matching ===\n\n")

# Test cases
test_cases <- list(
  list(text = "aa", pattern = "a", expected = FALSE),
  list(text = "aa", pattern = "*", expected = TRUE),
  list(text = "cb", pattern = "?a", expected = FALSE),
  list(text = "adceb", pattern = "*a*b", expected = TRUE),
  list(text = "acdcb", pattern = "a*c?b", expected = FALSE),
  list(text = "abc", pattern = "abc", expected = TRUE),
  list(text = "abc", pattern = "a?c", expected = TRUE),
  list(text = "abc", pattern = "a*c", expected = TRUE),
  list(text = "", pattern = "*", expected = TRUE),
  list(text = "", pattern = "?", expected = FALSE),
  list(text = "mississippi", pattern = "m*iss*p*", expected = TRUE),
  list(text = "hello", pattern = "h*o", expected = TRUE),
  list(text = "world", pattern = "w?r*", expected = TRUE),
  list(text = "test", pattern = "t??t", expected = TRUE),
  list(text = "abcdef", pattern = "a*f", expected = TRUE)
)

cat("Running test cases:\n\n")
passed <- 0
failed <- 0

for (i in seq_along(test_cases)) {
  tc <- test_cases[[i]]
  result <- isMatch(tc$text, tc$pattern)
  status <- if (result == tc$expected) "PASS" else "FAIL"
  
  if (result == tc$expected) {
    passed <- passed + 1
  } else {
    failed <- failed + 1
  }
  
  cat(sprintf("Test %d: text='%s', pattern='%s' => %s [%s]\n",
              i, tc$text, tc$pattern, result, status))
}

cat(sprintf("\nResults: %d passed, %d failed out of %d tests\n\n",
            passed, failed, length(test_cases)))

# Example: Complex patterns
cat("Complex Pattern Examples:\n")

examples <- list(
  list(text = "programming", pattern = "pro*ing"),
  list(text = "dynamic", pattern = "d?n?m?c"),
  list(text = "algorithm", pattern = "*gor*"),
  list(text = "computer", pattern = "c*t*r"),
  list(text = "science", pattern = "s*e*e")
)

for (ex in examples) {
  result <- isMatch(ex$text, ex$pattern)
  cat(sprintf("  '%s' matches '%s': %s\n", ex$text, ex$pattern, result))
}

# Example: Space-optimized version comparison
cat("\nSpace-Optimized Version Test:\n")
text1 <- "abcdefghij"
pattern1 <- "a*f*j"
result_normal <- isMatch(text1, pattern1)
result_optimized <- isMatch_optimized(text1, pattern1)
cat(sprintf("Text: '%s', Pattern: '%s'\n", text1, pattern1))
cat(sprintf("Normal DP: %s, Optimized: %s\n", result_normal, result_optimized))

# Example: Multiple texts matching
cat("\nMatching Multiple Texts:\n")
texts <- c("cat", "bat", "rat", "hat", "mat", "sat")
pattern2 <- "?at"
cat(sprintf("Pattern: '%s'\n", pattern2))
cat("Matching texts:\n")
for (text in texts) {
  if (isMatch(text, pattern2)) {
    cat(sprintf("  - %s\n", text))
  }
}

# Example: Wildcard star patterns
cat("\nWildcard Star Patterns:\n")
files <- c("document.txt", "image.png", "script.r", "data.csv", "report.pdf")
pattern3 <- "*.txt"
cat(sprintf("Pattern: '%s'\n", pattern3))
cat("Matching files:\n")
for (file in files) {
  if (isMatch(file, pattern3)) {
    cat(sprintf("  - %s\n", file))
  }
}

pattern4 <- "*.r"
cat(sprintf("\nPattern: '%s'\n", pattern4))
cat("Matching files:\n")
for (file in files) {
  if (isMatch(file, pattern4)) {
    cat(sprintf("  - %s\n", file))
  }
}

# Example: Edge cases
cat("\nEdge Cases:\n")
edge_cases <- list(
  list(text = "", pattern = ""),
  list(text = "a", pattern = ""),
  list(text = "", pattern = "a"),
  list(text = "***", pattern = "*"),
  list(text = "aaa", pattern = "a*a")
)

for (ec in edge_cases) {
  result <- isMatch(ec$text, ec$pattern)
  cat(sprintf("  text='%s', pattern='%s' => %s\n", ec$text, ec$pattern, result))
}

cat("\n=== All tests completed ===\n")

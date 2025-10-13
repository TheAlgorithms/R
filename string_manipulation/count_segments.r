# Count Segments (Word Boundary Detection) 
#
# Counts the number of segments (words) in a string by detecting word boundaries.
# A segment is a sequence of non-space characters. Segments are separated by spaces.
# 
# Time Complexity: O(n) where n is the length of the string
# Space Complexity: O(1) - constant space usage
#
# Algorithm:
# - Iterate through each character in the string
# - When a non-space character follows a space (or is the first character), count it as a new segment
# - Return the total segment count

count_segments <- function(s) {
  #' Count the number of segments (words) in a string
  #'
  #' @description Counts segments by detecting word boundaries. A segment starts when 
  #' a non-space character follows a space or is at the beginning of the string.
  #' @param s A character string to analyze
  #' @return Integer representing the number of segments
  #' @usage count_segments("hello world test")
  #' @details This function efficiently counts words by identifying transitions from 
  #' space to non-space characters. Leading/trailing spaces are handled correctly.
  #' Empty strings return 0. Multiple consecutive spaces are treated as single separators.
  #' @examples
  #' count_segments("hello world")     # returns 2
  #' count_segments("  hello  world  ") # returns 2  
  #' count_segments("")               # returns 0
  #' count_segments("   ")            # returns 0
  
  # Input validation
  if (!is.character(s)) {
    stop("Input must be a character string")
  }
  
  if (length(s) != 1) {
    stop("Input must be a single string")
  }
  
  # Handle empty string
  if (nchar(s) == 0) {
    return(0)
  }
  
  # Convert string to character vector
  chars <- strsplit(s, "")[[1]]
  count <- 0
  
  # Iterate through characters
  for (i in seq_along(chars)) {
    # Check if current char is not a space AND
    # (it's the first character OR previous char was a space)
    if (chars[i] != ' ' && (i == 1 || chars[i - 1] == ' ')) {
      count <- count + 1
    }
  }
  
  return(count)
}

# Alternative implementation using regular expressions for comparison
count_segments_regex <- function(s) {
  #' Alternative implementation using regex to count segments
  #'
  #' @description Uses regular expressions to split on whitespace and count non-empty segments
  #' @param s A character string to analyze  
  #' @return Integer representing the number of segments
  
  if (!is.character(s) || length(s) != 1) {
    stop("Input must be a single character string")
  }
  
  # Trim leading/trailing spaces and split on whitespace
  trimmed <- trimws(s)
  if (nchar(trimmed) == 0) {
    return(0)
  }
  
  # Split on one or more spaces and count non-empty segments
  segments <- strsplit(trimmed, "\\s+")[[1]]
  return(length(segments))
}

# Vectorized version for processing multiple strings at once
count_segments_vectorized <- function(strings) {
  #' Vectorized version to count segments in multiple strings
  #'
  #' @description Applies segment counting to a vector of strings
  #' @param strings A character vector of strings to analyze
  #' @return Integer vector with segment counts for each input string
  
  if (!is.character(strings)) {
    stop("Input must be a character vector")
  }
  
  sapply(strings, count_segments, USE.NAMES = FALSE)
}

# -----------------------------
# Examples and Testing
# -----------------------------
cat("=== Count Segments (Word Boundary Detection) ===\n")

# Test cases
test_cases <- list(
  list(input = "hello world", expected = 2),
  list(input = "  hello  world  ", expected = 2),
  list(input = "", expected = 0),
  list(input = "   ", expected = 0),
  list(input = "single", expected = 1),
  list(input = " single ", expected = 1),
  list(input = "one two three four five", expected = 5),
  list(input = "a", expected = 1),
  list(input = " a ", expected = 1),
  list(input = "Hello, my name is John", expected = 5),
  list(input = "programming    is     fun", expected = 3)
)

cat("\nTesting count_segments function:\n")
for (i in seq_along(test_cases)) {
  test <- test_cases[[i]]
  result <- count_segments(test$input)
  status <- if (result == test$expected) "✓ PASS" else "✗ FAIL"
  cat(sprintf("Test %d: \"%s\" -> %d (expected %d) %s\n", 
              i, test$input, result, test$expected, status))
}

# Compare with regex implementation
cat("\nComparing implementations:\n")
comparison_tests <- c("hello world", "  spaced  out  ", "", "single", "multiple   spaces")
for (test_str in comparison_tests) {
  result1 <- count_segments(test_str)
  result2 <- count_segments_regex(test_str)
  match <- if (result1 == result2) "✓" else "✗"
  cat(sprintf("String: \"%s\" | Boundary: %d | Regex: %d %s\n", 
              test_str, result1, result2, match))
}

# Test vectorized version
cat("\nTesting vectorized version:\n")
vector_input <- c("hello world", "one two three", "", "single", "  spaces  everywhere  ")
vector_results <- count_segments_vectorized(vector_input)
for (i in seq_along(vector_input)) {
  cat(sprintf("Input: \"%s\" -> %d segments\n", vector_input[i], vector_results[i]))
}

# Performance demonstration (optional - only runs if microbenchmark is available)
if (requireNamespace("microbenchmark", quietly = TRUE)) {
  cat("\nPerformance comparison (if microbenchmark is available):\n")
  test_string <- paste(rep("word", 1000), collapse = " ")
  
  # Note: Actual benchmarking would require microbenchmark package
  cat("Large string test (", nchar(test_string), " characters):\n")
  start_time <- Sys.time()
  result_boundary <- count_segments(test_string)
  boundary_time <- Sys.time() - start_time
  
  start_time <- Sys.time()
  result_regex <- count_segments_regex(test_string)
  regex_time <- Sys.time() - start_time
  
  cat("Boundary method:", result_boundary, "segments in", boundary_time, "seconds\n")
  cat("Regex method:", result_regex, "segments in", regex_time, "seconds\n")
}
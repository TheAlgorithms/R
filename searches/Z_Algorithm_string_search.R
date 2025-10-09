# Z-Algorithm for String Matching
#
# The Z-Algorithm is a linear-time string-search algorithm that computes an array `Z`
# where each element `Z[i]` represents the length of the longest substring
# starting from position `i` that is also a prefix of the string.
#
# By concatenating the pattern, a unique delimiter, and the text,
# we can efficiently find all occurrences of the pattern in O(n + m) time.
#
# Time Complexity: O(n + m)
# Space Complexity: O(n + m)
#
# Applications:
# - Fast substring search (similar to KMP and Rabin–Karp)
# - Bioinformatics (motif search)
# - Finding repetitions and borders in strings
# - Prefix-based text indexing
#
# ------------------------------------------------------------

# Internal helper (non-exported): compute Z-array for a string `s`
# Note: leading dot indicates internal use; do not export.
.z_array <- function(s) {
  n <- nchar(s)
  z <- integer(n)
  l <- 0
  r <- 0
  chars <- strsplit(s, "")[[1]]

  for (i in seq(2, n)) {  # R is 1-based
    if (i > r) {
      l <- i; r <- i
      while (r <= n && chars[r] == chars[r - l + 1]) r <- r + 1
      z[i] <- r - l
      r <- r - 1
    } else {
      k <- i - l + 1
      if (z[k] < (r - i + 1)) {
        z[i] <- z[k]
      } else {
        l <- i
        while (r <= n && chars[r] == chars[r - l + 1]) r <- r + 1
        z[i] <- r - l
        r <- r - 1
      }
    }
  }
  z
}

# ------------------------------------------------------------
# Core Z-Algorithm function
# ------------------------------------------------------------
z_algorithm <- function(pattern, text) {
  #' Z-Algorithm for String Pattern Matching
  #'
  #' @param pattern Character string — the pattern to search for.
  #' @param text Character string — the text to search within.
  #' @return Integer vector of 1-based indices where the pattern occurs in text.
  #'
  #' @examples
  #' z_algorithm("abc", "ababcabc")
  #' # [1] 3 6
  #'
  #' @details
  #' The algorithm constructs the string "pattern$text" and computes
  #' its Z-array, where Z[i] indicates how many characters from position i
  #' match the prefix. Whenever Z[i] equals the pattern length,
  #' the pattern occurs at i − pattern_length − 1.
  #'
  #' @export

  # Input validation
  if (missing(pattern) || missing(text) || nchar(pattern) == 0 || nchar(text) == 0) {
    return(integer(0))
  }

  # choose a safe delimiter
  delim <- "\x01"
  if (grepl(delim, pattern, fixed = TRUE) || grepl(delim, text, fixed = TRUE)) {
    delim <- "$"
  }

  combined <- paste0(pattern, delim, text)
  z <- .z_array(combined)
  pattern_length <- nchar(pattern)
  result <- c()

  for (i in seq_along(z)) {
    if (z[i] == pattern_length) {
      result <- c(result, i - pattern_length - 1)
    }
  }

  result <- result[result > 0]
  return(result)
}

# ------------------------------------------------------------
# Helper: visualize Z-array
# ------------------------------------------------------------
print_z_array <- function(s) {
  z <- .z_array(s)
  cat("Z-array for", s, ":\n")
  cat(paste(z, collapse = " "), "\n\n")
}

# ------------------------------------------------------------
# Example Usage & Testing
# ------------------------------------------------------------
cat("=== Z-Algorithm for String Matching ===\n\n")

# Test 1: Basic Example
pattern <- "abc"
text <- "ababcabc"
cat("Test 1: Basic Example\n")
cat("Pattern:", pattern, "\nText   :", text, "\n")
result <- z_algorithm(pattern, text)
cat("Pattern found at indices:", paste(result, collapse = ", "), "\n\n")

# Test 2: Overlapping Matches
pattern <- "aa"
text <- "aaaa"
cat("Test 2: Overlapping Matches\n")
cat("Pattern:", pattern, "\nText   :", text, "\n")
result <- z_algorithm(pattern, text)
cat("Pattern found at indices:", paste(result, collapse = ", "), "\n\n")

# Test 3: No Match
pattern <- "xyz"
text <- "abcabc"
cat("Test 3: No Match\n")
result <- z_algorithm(pattern, text)
cat("Result:", if (length(result) == 0) "No matches found" else result, "\n\n")

# Test 4: Show Z-Array
cat("Test 4: Show Z-Array\n")
print_z_array(paste0("abc$", "ababcabc"))

# ------------------------------------------------------------
# End of File
# ------------------------------------------------------------

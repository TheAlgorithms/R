# Rabin–Karp String Search Algorithm in R
# Author: sgindeed
# Description: Finds all occurrences of a pattern in a given text using a rolling hash technique.

# Ask user for input
text <- readline(prompt = "Enter the text: ")
pattern <- readline(prompt = "Enter the pattern to search: ")

# Convert both to lowercase for case-insensitive matching
text <- tolower(text)
pattern <- tolower(pattern)

# Get lengths
n <- nchar(text)
m <- nchar(pattern)

# Handle empty or invalid inputs
if (m == 0) {
  cat("Empty pattern. Nothing to search.\n")
  quit(save = "no")
}

if (m > n) {
  cat("Pattern is longer than text. Pattern not found in text.\n")
  quit(save = "no")
}

# Constants
base <- 256   # Number of possible characters
mod <- 101    # A prime number for hashing

# Initialize variables
p_hash <- 0  # hash for pattern
t_hash <- 0  # hash for text window
h <- 1       # base^(m-1)
matches <- c()

# Compute (base^(m-1)) % mod safely
for (i in seq_len(max(0, m - 1))) {
  h <- (h * base) %% mod
}

# Convert characters to ASCII values
pattern_chars <- utf8ToInt(pattern)
text_chars <- utf8ToInt(text)

# Compute initial hash values for pattern and first window of text
for (i in 1:m) {
  p_hash <- (base * p_hash + pattern_chars[i]) %% mod
  t_hash <- (base * t_hash + text_chars[i]) %% mod
}

# Rabin–Karp main search
for (i in 0:(n - m)) {
  # If hash matches, verify actual substring
  if (p_hash == t_hash) {
    if (substr(text, i + 1, i + m) == pattern) {
      matches <- c(matches, i + 1)
    }
  }
  
  # Slide window: remove first char, add next char
  if (i < n - m) {
    t_hash <- (base * (t_hash - text_chars[i + 1] * h) + text_chars[i + m + 1]) %% mod
    if (t_hash < 0) {
      t_hash <- t_hash + mod
    }
  }
}

# Display results
if (length(matches) > 0) {
  cat("Pattern found at positions:", paste(matches, collapse = ", "), "\n")
} else {
  cat("Pattern not found in the given text.\n")
}

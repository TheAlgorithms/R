# Rabin–Karp String Search Algorithm in R
# Author: sgindeed
# Description: Finds all occurrences of a pattern in a given text using rolling hash

# Ask for user inputs
text <- readline(prompt = "Enter the main text: ")
pattern <- readline(prompt = "Enter the pattern to search: ")

# Convert to lowercase for case-insensitive matching
text <- tolower(text)
pattern <- tolower(pattern)

n <- nchar(text)
m <- nchar(pattern)

# Base and modulus for rolling hash
base <- 256
mod <- 101

# Convert to character vectors
text_chars <- utf8ToInt(text)
pattern_chars <- utf8ToInt(pattern)

# Compute (base^(m-1)) % mod
h <- 1
for (i in 1:(m - 1)) {
  h <- (h * base) %% mod
}

# Compute hash of pattern and first window
p_hash <- 0
t_hash <- 0
for (i in 1:m) {
  p_hash <- (base * p_hash + pattern_chars[i]) %% mod
  t_hash <- (base * t_hash + text_chars[i]) %% mod
}

# Rabin–Karp pattern search
matches <- c()
for (i in 0:(n - m)) {
  # If hash matches, check actual substring
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
  cat("Pattern not found in text.\n")
}

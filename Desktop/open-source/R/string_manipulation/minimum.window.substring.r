# Minimum Window Substring in R
# Author: sgindeed
# Description: Finds the smallest substring of s that contains all characters of t

# Ask for inputs
s <- readline(prompt = "Enter main string: ")
t <- readline(prompt = "Enter target characters: ")

# Convert to lowercase for case-insensitivity
s <- tolower(s)
t <- tolower(t)

# Edge case
if (nchar(s) == 0 || nchar(t) == 0) {
  cat("Empty input. Exiting.\n")
  quit(save = "no")
}

# Convert to char arrays
s_chars <- strsplit(s, "")[[1]]
t_chars <- strsplit(t, "")[[1]]

# Frequency of characters in t
t_count <- table(t_chars)
window_count <- list()

required <- length(t_count)
formed <- 0

left <- 1
right <- 0
min_len <- Inf
min_window <- ""

# Sliding window
while (right < length(s_chars)) {
  right <- right + 1
  char <- s_chars[right]
  window_count[[char]] <- (window_count[[char]] %||% 0) + 1
  
  if (!is.na(t_count[char]) && window_count[[char]] == t_count[char]) {
    formed <- formed + 1
  }
  
  # Try to contract the window
  while (left <= right && formed == required) {
    if ((right - left + 1) < min_len) {
      min_len <- right - left + 1
      min_window <- paste0(s_chars[left:right], collapse = "")
    }
    
    left_char <- s_chars[left]
    window_count[[left_char]] <- window_count[[left_char]] - 1
    if (!is.na(t_count[left_char]) && window_count[[left_char]] < t_count[left_char]) {
      formed <- formed - 1
    }
    left <- left + 1
  }
}

if (is.infinite(min_len)) {
  cat("No valid window found.\n")
} else {
  cat("Minimum window substring:", min_window, "\n")
  cat("Length:", min_len, "\n")
}

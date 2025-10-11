# Longest Substring Without Repeating Characters in R
# Author: sgindeed
# Description: Finds the length of the longest substring without repeating characters

# Ask for user input
input.string <- readline(prompt = "Enter a string: ")

# Convert the string to lowercase for case-insensitive processing (optional)
clean.string <- tolower(input.string)

# Split string into characters
chars <- strsplit(clean.string, "")[[1]]

# Initialize variables
hash.table <- list()  # stores last index of characters
max.length <- 0
start <- 1  # start of current window

# Iterate over characters
for (i in seq_along(chars)) {
  char <- chars[i]
  
  # If character was seen before and is inside current window
  if (!is.null(hash.table[[char]]) && hash.table[[char]] >= start) {
    start <- hash.table[[char]] + 1  # move start to one after previous occurrence
  }
  
  # Update last seen index of the character
  hash.table[[char]] <- i
  
  # Update max length
  current.length <- i - start + 1
  if (current.length > max.length) {
    max.length <- current.length
  }
}

# Display the result
cat("Length of the longest substring without repeating characters:", max.length, "\n")

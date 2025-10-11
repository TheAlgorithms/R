# Burrows-Wheeler Transform (BWT) in R
# Computes the Burrows-Wheeler Transform of a string
# Useful in compression and efficient substring searching

burrows_wheeler_transform <- function(s) {
  s <- paste0(s, "$")  # Append unique end-of-string character
  n <- nchar(s)
  
  # Generate all rotations of the string
  rotations <- character(n)
  for (i in 1:n) {
    rotations[i] <- paste0(substr(s, i, n), substr(s, 1, i - 1))
  }
  
  # Sort the rotations lexicographically
  rotations_sorted <- sort(rotations)
  
  # Build BWT by taking the last character of each sorted rotation
  bwt <- paste0(sapply(rotations_sorted, function(x) substr(x, n, n)), collapse = "")
  
  return(list(original = s, rotations_sorted = rotations_sorted, bwt = bwt))
}

# Interactive input
s <- readline(prompt = "Enter a string: ")
result <- burrows_wheeler_transform(s)

cat("Original string with end marker: ", result$original, "\n")
cat("Sorted rotations:\n")
print(result$rotations_sorted)
cat("Burrows-Wheeler Transform: ", result$bwt, "\n")

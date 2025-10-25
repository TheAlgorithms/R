# Pigeonhole Sort Function
# Sorts an integer vector using the Pigeonhole Sort algorithm.
# Parameters:
# - arr: Input integer vector to be sorted.
# Returns:
# - Sorted vector.

pigeonhole_sort <- function(arr) {
  # Handle empty input
  if (length(arr) == 0) return(arr)
  
  # Pigeonhole sort is defined for integers
  if (!all(arr == floor(arr))) {
    stop("pigeonhole_sort works with integer values only.")
  }
  
  # Find range
  min_val <- min(arr)
  max_val <- max(arr)
  range <- as.integer(max_val - min_val + 1L)
  
  # Create holes (counts per value)
  holes <- integer(range)
  
  # Populate holes
  for (i in seq_along(arr)) {
    holes[arr[i] - min_val + 1L] <- holes[arr[i] - min_val + 1L] + 1L
  }
  
  # Reconstruct sorted array
  idx <- 1L
  for (i in seq_len(range)) {
    cnt <- holes[i]
    if (cnt > 0L) {
      val <- (i - 1L) + min_val
      arr[idx:(idx + cnt - 1L)] <- val
      idx <- idx + cnt
    }
  }
  
  return(arr)
}

# Example usage:
# ---------------
# Input:  c(8, 3, 2, 7, 4, 6, 8)
# Output: c(2, 3, 4, 6, 7, 8, 8)

elements_vec <- c(8, 3, 2, 7, 4, 6, 8)
sorted_vec <- pigeonhole_sort(elements_vec)
print(sorted_vec)

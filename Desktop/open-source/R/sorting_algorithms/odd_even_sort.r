# Odd-Even Sort Function
# Sorts an input vector in-place using the Odd-Even Sort algorithm.
# Parameters:
# - arr: Input vector to be sorted.
# Returns:
# - Sorted vector.
odd_even_sort <- function(arr) {
  n <- length(arr)
  sorted <- FALSE
  while (!sorted) {
    sorted <- TRUE
    
    # Odd-Even Sort (Phase 1 - Odd)
    for (i in seq(1, n - 1, by = 2)) {
      if (arr[i] > arr[i + 1]) {
        temp <- arr[i]
        arr[i] <- arr[i + 1]
        arr[i + 1] <- temp
        sorted <- FALSE
      }
    }
    
    # Odd-Even Sort (Phase 2 - Even)
    for (i in seq(2, n - 1, by = 2)) {
      if (arr[i] > arr[i + 1]) {
        temp <- arr[i]
        arr[i] <- arr[i + 1]
        arr[i + 1] <- temp
        sorted <- FALSE
      }
    }
  }
  return(arr)
}

# Example usage:
elements_vec <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
odd_even_sorted_vec <- odd_even_sort(elements_vec)
print(odd_even_sorted_vec)

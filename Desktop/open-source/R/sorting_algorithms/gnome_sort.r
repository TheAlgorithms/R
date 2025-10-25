# Gnome Sort Function
# Sorts an input vector using the Gnome Sort algorithm.
# Parameters:
# - arr: Input vector to be sorted.
# Returns:
# - Sorted vector.

gnome_sort <- function(arr) {
  index <- 1
  n <- length(arr)
  
  while (index <= n) {
    if (index == 1 || arr[index] >= arr[index - 1]) {
      index <- index + 1
    } else {
      # Swap arr[index] and arr[index - 1]
      temp <- arr[index]
      arr[index] <- arr[index - 1]
      arr[index - 1] <- temp
      index <- index - 1
    }
  }
  
  return(arr)
}

# Example usage:
elements_vec <- c(34, 2, 10, -9)
gnome_sorted_vec <- gnome_sort(elements_vec)
print(gnome_sorted_vec)

# Patience Sort Function
# Sorts an input vector using the Patience Sort algorithm.
# Parameters:
# - arr: Input vector to be sorted.
# Returns:
# - Sorted vector.

patience_sort <- function(arr) {
  if (length(arr) == 0) {
    return(arr)
  }
  
  piles <- list()
  
  # Build piles
  for (x in arr) {
    placed <- FALSE
    for (i in seq_along(piles)) {
      if (x < tail(piles[[i]], n=1)) {
        piles[[i]] <- c(piles[[i]], x)
        placed <- TRUE
        break
      }
    }
    if (!placed) {
      piles[[length(piles) + 1]] <- c(x)
    }
  }
  
  # Collect sorted elements
  sorted_arr <- c()
  while (length(piles) > 0) {
    # Find the pile with the smallest top element
    min_top <- Inf
    min_index <- -1
    for (i in seq_along(piles)) {
      if (tail(piles[[i]], n=1) < min_top) {
        min_top <- tail(piles[[i]], n=1)
        min_index <- i
      }
    }
    # Remove the smallest top element and add it to the sorted array
    sorted_arr <- c(sorted_arr, min_top)
    piles[[min_index]] <- head(piles[[min_index]], -1)
    if (length(piles[[min_index]]) == 0) {
      piles[[min_index]] <- NULL
    }
  }
  
  return(sorted_arr)
}

# Example usage:
elements_vec <- c(4, 3, 2, 1)
patience_sorted_vec <- patience_sort(elements_vec)
print(patience_sorted_vec)

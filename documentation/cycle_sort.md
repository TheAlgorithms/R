

```r
# Cycle Sort Function
# Sorts an input vector in-place using the Cycle Sort algorithm.
# Parameters:
# - arr: Input vector to be sorted.
# Returns:
# - Sorted vector.
cycle_sort <- function(arr) {
  n <- length(arr)
  for (cycle_start in 1:(n - 1)) {
    item <- arr[cycle_start]
    pos <- cycle_start
    
    # Find the correct position for the current item
    for (i in (cycle_start + 1):n) {
      if (arr[i] < item) {
        pos <- pos + 1
      }
    }
    
    # Skip if the item is already in the correct position
    if (pos == cycle_start) {
      next
    }
    
    # Move the item to its correct position
    while (item == arr[pos]) {
      pos <- pos + 1
    }
    temp <- arr[pos]
    arr[pos] <- item
    item <- temp
    
    # Rotate the remaining cycle
    while (pos != cycle_start) {
      pos <- cycle_start
      for (i in (cycle_start + 1):n) {
        if (arr[i] < item) {
          pos <- pos + 1
        }
      }
      
      # Skip if the item is already in the correct position
      while (item == arr[pos]) {
        pos <- pos + 1
      }
      
      # Move the item to its correct position
      temp <- arr[pos]
      arr[pos] <- item
      item <- temp
    }
  }
  return(arr)
}

# Example usage:
elements_vec <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
cycle_sorted_vec <- cycle_sort(elements_vec)
print(cycle_sorted_vec)
```

```
##  [1] 1 1 2 3 3 4 5 5 5 6 9
```


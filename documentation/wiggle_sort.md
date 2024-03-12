

```r
# Wiggle Sort Function
# Rearranges the elements in the input vector into a wiggle pattern.
# Parameters:
# - arr: Input vector to be rearranged.
# Returns:
# - Wiggle sorted vector.
wiggle_sort <- function(arr) {
  n <- length(arr)
  for (i in 2:n) {
    if ((i %% 2 == 0 && arr[i] < arr[i - 1]) || (i %% 2 != 0 && arr[i] > arr[i - 1])) {
      # Swap elements at odd positions if they are greater
      # or at even positions if they are smaller.
      temp <- arr[i]
      arr[i] <- arr[i - 1]
      arr[i - 1] <- temp
    }
  }
  return(arr)
}

# Example usage:
elements_vec <- c(3, 5, 2, 1, 6, 4)
wiggle_sorted_vec <- wiggle_sort(elements_vec)
print(wiggle_sorted_vec)
```

```
## [1] 3 5 1 6 2 4
```


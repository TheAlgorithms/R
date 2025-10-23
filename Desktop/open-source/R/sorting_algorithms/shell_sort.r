# Function to perform Shell Sort
shellSort <- function(arr) {
  n <- length(arr)
  
  # Start with a large gap and reduce it
  gap <- n %/% 2  # Initial gap
  
  while (gap > 0) {
    for (i in (gap + 1):n) {
      # Store the current element to be compared
      temp <- arr[i]
      
      # Compare the current element with elements at positions 'i - gap', 'i - 2 * gap', ...
      j <- i
      while (j > gap && arr[j - gap] > temp) {
        arr[j] <- arr[j - gap]
        j <- j - gap
      }
      
      # Place the current element in its correct position
      arr[j] <- temp
    }
    
    # Reduce the gap for the next iteration
    gap <- gap %/% 2
  }
  
  return(arr)
}

# Example usage:
arr <- c(12, 34, 54, 2, 3)
cat("Original Array:", arr, "\n")

# Call the Shell Sort function to sort the array
sortedArr <- shellSort(arr)
cat("Sorted Array:", sortedArr, "\n")

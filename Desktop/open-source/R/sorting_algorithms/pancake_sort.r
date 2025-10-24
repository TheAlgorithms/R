# Function to flip the first 'k' elements of an array
flip <- function(arr, k) {
  arr[1:k] <- rev(arr[1:k])  # Reverse the first 'k' elements
  return(arr)
}

# Function to find the index of the maximum element in an array
findMaxIndex <- function(arr, n) {
  maxIndex <- 1
  for (i in 2:n) {
    if (arr[i] > arr[maxIndex]) {
      maxIndex <- i
    }
  }
  return(maxIndex)
}

# Function to perform Pancake Sort
pancakeSort <- function(arr) {
  n <- length(arr)
  
  for (currentSize in n:2) {
    # Find the index of the maximum element in the unsorted part of the array
    maxIndex <- findMaxIndex(arr, currentSize)
    
    # If the maximum element is not at the end of the unsorted part, flip it
    if (maxIndex != currentSize) {
      # Flip the maximum element to the beginning of the array
      arr <- flip(arr, maxIndex)
      
      # Flip the maximum element to its correct position
      arr <- flip(arr, currentSize)
    }
  }
  
  return(arr)
}

# Example usage:
arr <- c(3, 1, 5, 2, 4)
cat("Original Array:", arr, "\n")

# Call the Pancake Sort function to sort the array
sortedArr <- pancakeSort(arr)
cat("Sorted Array:", sortedArr, "\n")

# Binary Insertion Sort Function
# Sorts an input vector using the Binary Insertion Sort algorithm.
# Parameters:
# - arr: Input vector to be sorted.
# Returns:
# - Sorted vector.
binary_insertion_sort <- function(arr) {
  # Loop through the input vector starting from the second element.
  for (i in 2:length(arr)) {
    # Store the current element in a variable.
    key <- arr[i]
    # Initialize left and right pointers for binary search.
    left <- 1
    right <- i - 1
    
    # Binary search to find the correct position to insert the key.
    while (left <= right) {
      mid <- left + (right - left) %/% 2
      
      if (key < arr[mid]) {
        right <- mid - 1
      } else {
        left <- mid + 1
      }
    }
    
    # Shift elements to the right to make space for the key.
    for (j in i: (left + 1)) {
      arr[j] <- arr[j - 1]
    }
    
    # Insert the key into its correct position.
    arr[left] <- key
  }
  
  # Return the sorted vector.
  return(arr)
}

# Example usage:
elements_vec <- c(64, 34, 25, 12, 22, 11, 90)
sorted_vec <- binary_insertion_sort(elements_vec)
print(sorted_vec)

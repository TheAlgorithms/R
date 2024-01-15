# Bucket Sort Function
# Sorts an input vector using the Bucket Sort algorithm.
# Parameters:
# - arr: Input vector to be sorted.
# Returns:
# - Sorted vector.
bucket_sort <- function(arr) {
  if (length(arr) == 0) {
    return(arr)
  }
  
  # Find the maximum and minimum values in the input vector
  max_val <- max(arr)
  min_val <- min(arr)
  
  # Create an array of buckets
  num_buckets <- max_val - min_val + 1
  buckets <- vector("list", length = num_buckets)
  
  # Initialize the buckets
  for (i in 1:num_buckets) {
    buckets[[i]] <- numeric(0)
  }
  
  # Place elements into buckets
  for (val in arr) {
    bucket_index <- val - min_val + 1
    buckets[[bucket_index]] <- c(buckets[[bucket_index]], val)
  }
  
  # Sort each bucket (using any sorting algorithm, e.g., Bubble Sort)
  sorted_buckets <- lapply(buckets, bubble.sort)
  
  # Concatenate the sorted buckets to obtain the final sorted array
  sorted_arr <- unlist(sorted_buckets)
  
  return(sorted_arr)
}

# Example usage:
elements_vec <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
bucket_sorted_vec <- bucket_sort(elements_vec)
print(bucket_sorted_vec)

# Strand Sort Function
# Sorts an input vector using the Strand Sort algorithm.
# Parameters:
# - arr: Input vector to be sorted.
# Returns:
# - Sorted vector.

strand_sort <- function(arr) {
  if (length(arr) <= 1) {
    return(arr)
  }
  
  output <- c()
  
  while (length(arr) > 0) {
    sublist <- c(arr[1])
    arr <- arr[-1]
    i <- 1
    while (i <= length(arr)) {
      if (arr[i] >= tail(sublist, n=1)) {
        sublist <- c(sublist, arr[i])
        arr <- arr[-i]
      } else {
        i <- i + 1
      }
    }
    output <- merge_sorted_lists(output, sublist)
  }
  
  return(output)
}

# Helper function to merge two sorted lists
merge_sorted_lists <- function(list1, list2) {
  result <- c()
  i <- 1
  j <- 1
  
  while (i <= length(list1) && j <= length(list2)) {
    if (list1[i] <= list2[j]) {
      result <- c(result, list1[i])
      i <- i + 1
    } else {
      result <- c(result, list2[j])
      j <- j + 1
    }
  }
  
  if (i <= length(list1)) {
    result <- c(result, list1[i:length(list1)])
  }
  
  if (j <= length(list2)) {
    result <- c(result, list2[j:length(list2)])
  }
  
  return(result)
}

# Example usage:
elements_vec <- c(4, 2, 5, 3, 1)
strand_sorted_vec <- strand_sort(elements_vec)
print(strand_sorted_vec)

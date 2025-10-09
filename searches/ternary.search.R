# Ternary Search in R – works on sorted arrays

ternary_search <- function(arr, target) {
  l <- 1
  r <- length(arr)
  while (r >= l) {
    mid1 <- l + (r - l) %/% 3
    mid2 <- r - (r - l) %/% 3
    if (arr[mid1] == target) return(mid1)
    if (arr[mid2] == target) return(mid2)
    if (target < arr[mid1]) r <- mid1 - 1
    else if (target > arr[mid2]) l <- mid2 + 1
    else {
      l <- mid1 + 1
      r <- mid2 - 1
    }
  }
  return(-1)
}

arr <- as.integer(strsplit(readline("Enter sorted integers: "), " ")[[1]])
target <- as.integer(readline("Enter target to search: "))
index <- ternary_search(arr, target)
if (index != -1)
  cat("Element found at position:", index, "\n")
else
  cat("Element not found.\n")
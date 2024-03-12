

```r
binary_search <- function(arr, target) { #function for finding position of value 
  low <- 1
  high <- length(arr) 
  
  while (low <= high) {
    mid <- low + (high - low) %/% 2 #finding mid of array
    
    if (arr[mid] == target) { #comapring the mis value with the value to search
      return(mid)  # Target found, return its index
    } else if (arr[mid] < target) {
      low <- mid + 1  # Search in the right half
    } else {
      high <- mid - 1  # Search in the left half
    }
  }
  return(-1)  # Target not found in the array
}

arr <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) #input array (hard code)
target <- 7 #input value to be searched (hard code)

result <- binary_search(arr, target) #binary_seach function calling

if (result == -1) {
  cat("Element", target, "not found in the array.\n")
} else {
  cat("Element", target, "found at position", result, ".\n")
}
```

```
## Element 7 found at position 7 .
```


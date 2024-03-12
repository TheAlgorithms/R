

```r
cocktailSort <- function(arr) {
  n <- length(arr)
  swapped <- TRUE
  beg <- 1
  end <- n - 1

  while (swapped) {
    swapped <- FALSE
    
    # Forward pass (left to right)
    for (i in seq(beg, end)) {
      if (arr[i] > arr[i + 1]) {
        # Swap arr[i] and arr[i + 1]
        temp <- arr[i]
        arr[i] <- arr[i + 1]
        arr[i + 1] <- temp
        swapped <- TRUE
      }
    }

    # If no swaps occurred in the forward pass, the array is sorted
    if (!swapped) {
      break
    }

    swapped <- FALSE
    end <- end - 1

    # Backward pass (right to left)
    for (i in seq(end, beg, by = -1)) {
      if (arr[i] > arr[i + 1]) {
        # Swap arr[i] and arr[i + 1]
        temp <- arr[i]
        arr[i] <- arr[i + 1]
        arr[i + 1] <- temp
        swapped <- TRUE
      }
    }

    beg <- beg + 1
  }
  
  return(arr)
}

# Example Usage
unsorted_array <- c(38, 27, 43, 3, 9, 82, 10)
cat("Unsorted Array: ", unsorted_array, "\n")
```

```
## Unsorted Array:  38 27 43 3 9 82 10
```

```r
# Call the Cocktail Sort function to sort the array
sorted_array <- cocktailSort(unsorted_array)

cat("Sorted Array: ", sorted_array, "\n")
```

```
## Sorted Array:  3 9 10 27 38 43 82
```

```r
# Example: The 'unsorted_array' is sorted using Cocktail Sort
```


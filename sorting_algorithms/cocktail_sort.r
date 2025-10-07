# Cocktail Sort in R
cocktail.sort <- function(arr) {
  n <- length(arr)
  swapped <- TRUE
  start <- 1
  end <- n - 1

  while (swapped) {
    swapped <- FALSE
    
    # Forward pass
    for (i in seq(start, end)) {
      if (arr[i] > arr[i + 1]) {
        temp <- arr[i]
        arr[i] <- arr[i + 1]
        arr[i + 1] <- temp
        swapped <- TRUE
      }
    }

    if (!swapped) break

    swapped <- FALSE
    end <- end - 1

    # Backward pass
    for (i in seq(end, start, by = -1)) {
      if (arr[i] > arr[i + 1]) {
        temp <- arr[i]
        arr[i] <- arr[i + 1]
        arr[i + 1] <- temp
        swapped <- TRUE
      }
    }

    start <- start + 1
  }
  
  return(arr)
}

# Example usage
unsorted.array <- c(38, 27, 43, 3, 9, 82, 10)
cat("Unsorted Array:", unsorted.array, "\n")

sorted.array <- cocktail.sort(unsorted.array)
cat("Sorted Array:  ", sorted.array, "\n")

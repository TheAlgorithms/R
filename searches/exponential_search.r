# Exponential Search in R – efficient search on sorted arrays

#' Performs exponential search on a sorted numeric vector.
#'
#' @param arr Numeric vector sorted in ascending order.
#' @param target Numeric value to search for in arr.
#' @return The 1-based index of target in arr if found; otherwise -1.
#' @details Exponential search first finds a range where the target may reside by
#' doubling the index (1, 2, 4, 8, ...), then performs a binary search within that range.
#' Time complexity: O(log n); requires the array to be sorted in ascending order.
exponential_search <- function(arr, target) {
  n <- length(arr)
  if (n == 0) return(-1)

  # If the first element is the target
  if (arr[1] == target) return(1)

  # Find range for binary search by repeated doubling
  i <- 2
  while (i <= n && arr[i] <= target) {
    i <- i * 2
  }

  low <- max(1, i %/% 2)
  high <- min(i, n)

  # Binary search within [low, high]
  return(.binary_search_range(arr, target, low, high))
}

# Internal helper: standard binary search on subrange [low, high]
.binary_search_range <- function(arr, target, low, high) {
  while (low <= high) {
    mid <- low + (high - low) %/% 2
    if (arr[mid] == target) {
      return(mid)
    } else if (arr[mid] < target) {
      low <- mid + 1
    } else {
      high <- mid - 1
    }
  }
  return(-1)
}

# --- Demonstration (mirrors other search files) ---
arr <- c(2, 3, 4, 10, 15, 18, 20, 25, 30) # sorted input array
target <- 18 # value to search

idx <- exponential_search(arr, target)

if (idx == -1) {
  cat("Element", target, "not found in the array.\n")
} else {
  cat("Element", target, "found at position", idx, ".\n")
}

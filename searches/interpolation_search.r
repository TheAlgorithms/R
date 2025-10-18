# Interpolation Search in R – works on sorted arrays with uniformly distributed values

#' Performs interpolation search on a sorted numeric vector.
#'
#' @param arr Numeric vector sorted in ascending order.
#' @param target Numeric value to search for in arr.
#' @return The 1-based index of target in arr if found; otherwise -1.
#' @details Interpolation search estimates the position of target using
#' linear interpolation, which is efficient on uniformly distributed data.
#' Time complexity: average O(log log n), worst-case O(n). Requires a sorted array.
interpolation_search <- function(arr, target) {
  n <- length(arr)
  if (n == 0) return(-1)
  low <- 1
  high <- n

  # Guard: if target is outside the bounds, fail early
  if (target < arr[low] || target > arr[high]) return(-1)

  while (low <= high && target >= arr[low] && target <= arr[high]) {
    # Avoid division by zero when all values in range are equal
    if (arr[low] == arr[high]) {
      if (arr[low] == target) return(low)
      return(-1)
    }

    # Estimate position using linear interpolation
    pos <- low + floor((target - arr[low]) * (high - low) / (arr[high] - arr[low]))

    # Bounds safety in case of numerical issues
    if (pos < low) pos <- low
    if (pos > high) pos <- high

    if (arr[pos] == target) {
      return(pos)
    } else if (arr[pos] < target) {
      low <- pos + 1
    } else {
      high <- pos - 1
    }
  }

  return(-1)
}

# --- Demonstration (simple usage, mirrors other search files) ---
arr <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100) # sorted input array
target <- 70 # value to search

idx <- interpolation_search(arr, target)

if (idx == -1) {
  cat("Element", target, "not found in the array.\n")
} else {
  cat("Element", target, "found at position", idx, ".\n")
}

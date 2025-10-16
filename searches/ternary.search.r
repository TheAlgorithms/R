#' Performs ternary search on a sorted integer array.
#'
#' @param arr Integer vector. Must be sorted in ascending order.
#' @param target Integer value to search for in arr.
#' @return The index (1-based) of the target in arr if found, otherwise -1.
#' @details The input array must be sorted in ascending order for correct results.
# Ternary Search in R – works on sorted arrays

ternary_search <- function(arr, target) {
  l <- 1
  r <- length(arr)
  while (l <= r) {
    mid1 <- l + (r - l) %/% 3
    mid2 <- r - (r - l) %/% 3

    if (arr[mid1] == target) return(mid1)
    if (arr[mid2] == target) return(mid2)

    if (target < arr[mid1]) {
      r <- mid1 - 1
    } else if (target > arr[mid2]) {
      l <- mid2 + 1
    } else {
      l <- mid1 + 1
      r <- mid2 - 1
    }
  }
  return(-1)
}

# --- User Input Section with Validation ---
arr_input <- readline("Enter sorted integers: ")
arr_split <- strsplit(arr_input, " ")[[1]]
arr <- as.integer(arr_split)
if (length(arr) == 0 || any(is.na(arr))) {
  cat("Error: Please enter a non-empty list of valid integers separated by spaces.\n")
  quit(status = 1)
}

target_input <- readline("Enter target to search: ")
target <- suppressWarnings(as.integer(target_input))
if (is.na(target_input) || is.na(target)) {
  cat("Error: Please enter a valid integer for the target.\n")
  quit(status = 1)
}

# --- Execute Search ---
index <- ternary_search(arr, target)

if (index != -1)
  cat("Element found at position:", index, "\n")
else
  cat("Element not found.\n")

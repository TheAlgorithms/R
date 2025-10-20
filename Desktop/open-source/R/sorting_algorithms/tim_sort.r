# Insertion Sort: Sort small subarrays (runs)
insertion_sort <- function(arr, left, right) {
  for (i in (left + 1):right) {
    key <- arr[i]
    j <- i - 1
    while (j >= left && arr[j] > key) {
      arr[j + 1] <- arr[j]
      j <- j - 1
    }
    arr[j + 1] <- key
  }
  return(arr)
}

# Merge two sorted subarrays
merge <- function(arr, left, mid, right) {
  n1 <- mid - left + 1
  n2 <- right - mid

  left_part <- arr[left:(mid)]
  right_part <- arr[(mid + 1):right]

  i <- 1
  j <- 1
  k <- left

  # Merge left_part and right_part into arr
  while (i <= n1 && j <= n2) {
    if (left_part[i] <= right_part[j]) {
      arr[k] <- left_part[i]
      i <- i + 1
    } else {
      arr[k] <- right_part[j]
      j <- j + 1
    }
    k <- k + 1
  }

  # Copy remaining elements of left_part, if any
  while (i <= n1) {
    arr[k] <- left_part[i]
    i <- i + 1
    k <- k + 1
  }

  # Copy remaining elements of right_part, if any
  while (j <= n2) {
    arr[k] <- right_part[j]
    j <- j + 1
    k <- k + 1
  }

  return(arr)
}

# TimSort function
tim_sort <- function(arr) {
  n <- length(arr)
  min_run <- 32
  
  # Sort individual subarrays of size min_run using insertion sort
  for (start in seq(1, n, by = min_run)) {
    end <- min(start + min_run - 1, n)
    arr <- insertion_sort(arr, start, end)
  }
  
  # Merge sorted subarrays
  size <- min_run
  while (size < n) {
    for (left in seq(1, n, by = 2 * size)) {
      mid <- min(left + size - 1, n)
      right <- min(left + 2 * size - 1, n)
      if (mid < right) {
        arr <- merge(arr, left, mid, right)
      }
    }
    size <- 2 * size
  }
  
  return(arr)
}

# Example usage:
# -------------------
# Input: c(5, 21, 7, 23, 19, 11, 16, 13)
# Expected Output: c(5, 7, 11, 13, 16, 19, 21, 23)

elements_vec <- c(5, 21, 7, 23, 19, 11, 16, 13)
tim_sorted_vec <- tim_sort(elements_vec)
print(tim_sorted_vec)

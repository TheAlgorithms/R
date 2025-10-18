# Intro sort in R:

# IntroSort is a hybrid sorting algorithm that starts with Quick Sort,
# switches to Heap Sort when recursion depth grows too large, and uses
# Insertion Sort for small partitions. This provides robust O(n log n)
# worst-case performance with excellent average-case behavior.

intro.sort <- function(elements.vec) {
  n <- length(elements.vec)
  if (n <= 1) return(elements.vec)

  SIZE_THRESHOLD <- 16
  # Depth limit: 2 * floor(log2(n)) similar to the canonical definition
  max.depth <- floor(2 * log(n, base = 2))

  insertion.sort.range <- function(arr, low, high) {
    if (low >= high) return(arr)
    if ((low + 1) <= high) {
      for (i in (low + 1):high) {
        key <- arr[i]
        j <- i - 1
        while (j >= low && arr[j] > key) {
          arr[j + 1] <- arr[j]
          j <- j - 1
        }
        arr[j + 1] <- key
      }
    }
    return(arr)
  }

  heap.sort.range <- function(arr, low, high) {
    n <- high - low + 1
    if (n <= 1) return(arr)

    heapify <- function(a, heap.size, i, offset) {
      # 0-based heap indices mapped into 'a' via 'offset'
      repeat {
        largest <- i
        left <- 2 * i + 1
        right <- 2 * i + 2

        if (left < heap.size && a[offset + left] > a[offset + largest]) {
          largest <- left
        }
        if (right < heap.size && a[offset + right] > a[offset + largest]) {
          largest <- right
        }

        if (largest != i) {
          tmp <- a[offset + i]
          a[offset + i] <- a[offset + largest]
          a[offset + largest] <- tmp
          i <- largest
        } else {
          break
        }
      }
      return(a)
    }

    # Build max heap
    if ((floor(n / 2) - 1) >= 0) {
      for (i in seq(floor(n / 2) - 1, 0, by = -1)) {
        arr <- heapify(arr, n, i, low)
      }
    }

    # Extract elements from heap
    for (i in seq(n - 1, 1, by = -1)) {
      tmp <- arr[low]
      arr[low] <- arr[low + i]
      arr[low + i] <- tmp
      arr <- heapify(arr, i, 0, low)
    }
    return(arr)
  }

  partition.range <- function(arr, low, high) {
    pivot <- arr[high]
    i <- low - 1
    if (low <= (high - 1)) {
      for (j in low:(high - 1)) {
        if (arr[j] <= pivot) {
          i <- i + 1
          tmp <- arr[i]
          arr[i] <- arr[j]
          arr[j] <- tmp
        }
      }
    }
    tmp <- arr[i + 1]
    arr[i + 1] <- arr[high]
    arr[high] <- tmp
    return(list(arr = arr, p = i + 1))
  }

  util <- function(arr, low, high, depth.limit) {
    if (low >= high) return(arr)
    size <- high - low + 1
    if (size <= SIZE_THRESHOLD) {
      return(insertion.sort.range(arr, low, high))
    }
    if (depth.limit == 0) {
      return(heap.sort.range(arr, low, high))
    }
    pr <- partition.range(arr, low, high)
    arr <- pr$arr
    p <- pr$p
    arr <- util(arr, low, p - 1, depth.limit - 1)
    arr <- util(arr, p + 1, high, depth.limit - 1)
    return(arr)
  }

  elements.vec <- util(elements.vec, 1, n, max.depth)
  return(elements.vec)
}

# Example:
# intro.sort(c(10, 7, 8, 9, 1, 5, 3, 12))
# [1] 1 3 5 7 8 9 10 12

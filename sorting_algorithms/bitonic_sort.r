# Bitonic sort in R:

# This implementation follows the style used across this repository: a single exported
# function with a commented example at the bottom. Bitonic sort is typically defined
# for sequences with length as a power of two. For convenience, we handle arbitrary
# lengths by padding with +Inf (for ascending) or -Inf (for descending) and removing
# the padding before returning.

# Helper to perform the bitonic compare and swap across a segment
.bitonic_compare_swap <- function(a, low, cnt, asc = TRUE) {
  if (cnt <= 1) return(a)
  k <- cnt %/% 2
  idx <- seq.int(low, length.out = k)
  partner <- idx + k
  if (asc) {
    swap <- a[idx] > a[partner]
  } else {
    swap <- a[idx] < a[partner]
  }
  if (any(swap)) {
    tmp <- a[idx[swap]]
    a[idx[swap]] <- a[partner[swap]]
    a[partner[swap]] <- tmp
  }
  # Recurse on both halves
  a <- .bitonic_compare_swap(a, low, k, asc)
  a <- .bitonic_compare_swap(a, low + k, k, asc)
  return(a)
}

# Core recursive builder of a bitonic sequence followed by merge
.bitonic_sort_core <- function(a, low, cnt, asc = TRUE) {
  if (cnt <= 1) return(a)
  k <- cnt %/% 2
  # Sort first half ascending, second half descending to form bitonic sequence
  a <- .bitonic_sort_core(a, low, k, TRUE)
  a <- .bitonic_sort_core(a, low + k, k, FALSE)
  # Merge whole sequence in desired direction
  a <- .bitonic_compare_swap(a, low, cnt, asc)
  return(a)
}

# Public API: bitonic.sort
# - elements.vec: numeric or comparable vector
# - ascending: TRUE for increasing order, FALSE for decreasing
bitonic.sort <- function(elements.vec, ascending = TRUE) {
  n <- length(elements.vec)
  if (n <= 1) return(elements.vec)

  # Determine next power of two
  next_pow2 <- 1L
  while (next_pow2 < n) next_pow2 <- next_pow2 * 2L

  # Pad to power of two for algorithmic convenience
  pad_len <- next_pow2 - n
  if (pad_len > 0) {
    pad_val <- if (ascending) Inf else -Inf
    a <- c(elements.vec, rep(pad_val, pad_len))
  } else {
    a <- elements.vec
  }

  a <- .bitonic_sort_core(a, 1L, length(a), asc = ascending)

  # Trim padding if any
  if (pad_len > 0) {
    a <- a[seq_len(n)]
  }
  return(a)
}

# Example:
# bitonic.sort(c(3, 7, 4, 8, 6, 2, 1, 5))
# [1] 1 2 3 4 5 6 7 8
# bitonic.sort(c(3, 7, 4, 8, 6, 2, 1, 5), ascending = FALSE)
# [1] 8 7 6 5 4 3 2 1

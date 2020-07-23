# Quick sort in R:

quick.sort <- function(elements.vec) {
  if(length(elements.vec) <= 1) {
    return(elements.vec)
  }
  pivot <- elements.vec[1]
  non.pivot  <- elements.vec[-1]
  pivot_less    <- quick.sort(non.pivot[non.pivot < pivot])
  pivot_greater <- quick.sort(non.pivot[non.pivot >= pivot])
  return(c(pivot_less, pivot, pivot_greater))
}

# Example:
# quick.sort(c(5, 2, 3, 1, 1, 4)) 
# [1] 1 1 2 3 4 5
# Note that quick sort is not a stable sorting algorithm.

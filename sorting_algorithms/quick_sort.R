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

# Notes:
# 1. Quick sort is not a stable sorting algorithm.
# 2. It is implemented in the 'sort' function of base R:
# sort(c(5, 2, 3, 1, 1, 4), method = "quick" , index.return = FALSE)
# [1] 1 1 2 3 4 5

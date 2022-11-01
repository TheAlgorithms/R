# Selection sort in R:

selection.sort <- function(elements.vec, ascending = TRUE) {
  max <- length(elements.vec)
  for (j in 1:(max - 1)) {
    m <- elements.vec[j]
    p <- j
    for(k in (j + 1):max) {
      if(ascending && elements.vec[k] < m || !ascending && elements.vec[k] > m) {
        m <- elements.vec[k]
        p <- k
      }
    } 
    elements.vec[p] <- elements.vec[j]
    elements.vec[j] <- m
  } 
  return(elements.vec)
}

# Example:
# selection.sort(c(5, 2, 3, 1, 1, 4)) 
# [1] 1 1 2 3 4 5
# Note that selection sort is not a stable sorting algorithm.

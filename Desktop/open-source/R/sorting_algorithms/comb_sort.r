# Comb sort in R:

comb.sort <- function(elements.vec) {
  gap <- length(elements.vec)
  swaps <- 1
  while (gap > 1 && swaps == 1) {
    gap = floor(gap / 1.3)
    if (gap < 1) {
      gap = 1
    }
    swaps = 0
    i = 1
    while (i + gap <= length(a)) {
      if (elements.vec[i] > elements.vec[i + gap]) {
        elements.vec[c(i, i + gap)] <- elements.vec[c(i + gap, i)]
        swaps = 1
      }
      i <- i + 1
    }
  }  
  return(elements.vec) 
}

# Example:
# comb.sort(sample(1:100,10))
# [1] 9 49 50 51 56 60 61 71 86 95

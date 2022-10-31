# Bubble sort in R:

bubble.sort <- function(elements.vec) { 
  n <- length(elements.vec)
  for(i in 1:(n-1)) {
    for(j in 1:(n-i)) {
      if(elements.vec[j+1] < elements.vec[j]) { 
        temp <- elements.vec[j]
        elements.vec[j] <- elements.vec[j+1]
        elements.vec[j+1] <- temp
      }
    }
  }
  return(elements.vec)
}

# Example:
# bubble.sort(c(5, 2, 3, 1, 4))
# [1] 1 2 3 4 5

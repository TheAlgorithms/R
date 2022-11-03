# Counting sort in R:

counting.sort <- function(elements.vec){  
  min <- min(elements.vec)
  max <- max(elements.vec)  
  count <- rep(0,(max - min + 1))
  for(i in 1:length(elements.vec)){
    x <- 1 - min + elements.vec[i]
    count[x] <- count[x] + 1
  } 
  for(i in 2:length(count)){
    count[i] <- count[i] + count[i-1]
  }  
  result <- rep(0,length(elements.vec))
  for(i in 1:length(elements.vec)){
    x <- 1 - min + elements.vec[i]
    index <- count[x]
    result[index] <- elements.vec[i]
    count[x] = count[x] - 1
  } 
  return(result)  
}

# Example:
# counting.sort(c(5, 2, 3, 1, 4))
# [1] 1 2 3 4 5

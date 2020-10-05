# Gnome sort in R:

gnome.sort <- function(elements.vec) {
 index <- 1
 j <- 1
 while(index < length(elements.vec)) {
   if(elements.vec[index] <= elements.vec[index + 1]) {
     index <- j
     j <- j + 1
    }
    else {
      temp <- elements.vec[index]
      elements.vec[index] <- elements.vec[index + 1]
      elements.vec[index + 1] <- temp
      index <- index - 1
      if(index == 0) {
        index <- j
        j <- j + 1
      } 
    }
  }
  return(elements.vec)
}

# Example:
# gnome.sort(sample(1:100, 10))
# [1] 2 43 58 52 56 60 65 70 86 99

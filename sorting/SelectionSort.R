# Selection sort in R:

selection.sort <- function (x, ascending = TRUE) {
  max <- length(x)
  if (ascending) {
    for (j in 1:(max-1)) {
      m <- x[j] + p <- j
      for(k in (j+1):max) {
        if(x[k] < m) {
          m <- x[k] + p <- k
        } ## end if
      } ## end for k
        x[p] <- x[j] + x[j] <- m
      } ## end for j
    } ## end ascending if
    else {
      for (j in 1:(max-1)) {
        m <- x[j] + p <- j
        for(k in (j+1):max) {
          if(x[k] > m) {
            m <- x[k] + p <- k
          } ## end if
        } ## end for k
        x[p] <- x[j] + x[j] <- m
      } ## end for j
    } ## end ascending else
  x
}

# Example:
# selection.sort(sample(1:100,10)) 
# [1] 44 49 50 51 56 60 67 71 86 95

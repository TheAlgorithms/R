# Bogo sort in R:

bogo.sort <- function(x) {
   while(is.unsorted(x)) x <- sample(x)
   x
}

# Example:
# bogo.sort(c(1, 10, 9, 7, 3, 0))
# [1]  0  1  3  7  9 10

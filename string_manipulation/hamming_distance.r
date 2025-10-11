# Hamming distance
library(roxygen2)
library(docstring)


hamming_distance <- function(input1, input2) {
  #' Find the hamming distance between two strings
  #' 
  #' @description Finds the hamming distance between two strings
  #' @param input1 String
  #' @param input2 String
  #' @usage hamming_distance(input1, input2)
  #' @details In information theory, the Hamming distance between two strings of equal length is the number of positions at which the corresponding symbols are different.
  #' In other words, it measures the minimum number of substitutions required to change one string into the other, or the minimum number of errors that could have transformed one string into the other. 
  #' In a more general context, the Hamming distance is one of several string metrics for measuring the edit distance between two sequences. It is named after the American mathematician Richard Hamming.
  #' @references https://en.wikipedia.org/wiki/Hamming_distance 
  
  if (length(input1) != length(input2)) stop("String lengths must be the same")
  
  sum(input1 != input2)
}


x1 = strsplit("karolin", "")[[1]]
y1 = strsplit("kathrin", "")[[1]]
print(hamming_distance(x1, y1) == 3) # returns TRUE

x2 = strsplit("0000", "")[[1]]
y2 = strsplit("1111", "")[[1]]
print(hamming_distance(x2, y2) == 4) # returns TRUE

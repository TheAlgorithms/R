# Permutation Calculation
calculate_permutations <- function(n, r) {
  
  #' @description Calculates the number of permutations of n objects taken r at a time.
  #' @param n Total number of objects
  #' @param r Number of objects in each arrangement
  #' @usage calculate_permutations(n, r)
  #' @details Permutations represent the number of ways to arrange r objects from n.
  #' It is calculated as n! / (n - r)! and is widely used in combinatorics.
  #' @references https://en.wikipedia.org/wiki/Permutation
  
  if (r > n) stop("r must be less than or equal to n")
  
  factorial <- function(x) if (x == 0) 1 else prod(1:x)
  
  return(factorial(n) / factorial(n - r))
}

# Example
print(calculate_permutations(5, 3))  # expected 60
print(calculate_permutations(10, 2)) # expected 90

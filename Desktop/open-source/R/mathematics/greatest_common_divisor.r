# GCD Calculation using Euclidean Algorithm
find_gcd <- function(a, b) {
  
  #' @description Computes the Greatest Common Divisor (GCD) of two integers.
  #' @param a Integer
  #' @param b Integer
  #' @usage find_gcd(a, b)
  #' @details This function uses the Euclidean algorithm to find the GCD.
  #' GCD is essential in various mathematical contexts, particularly in
  #' simplification of fractions and number theory applications.
  #' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
  
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  
  return(abs(a))
}

# Examples
print(find_gcd(48, 18))  # expected 6
print(find_gcd(54, 24))  # expected 6

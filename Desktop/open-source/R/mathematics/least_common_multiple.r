# LCM Calculation
find_lcm <- function(a, b) {
  
  #' @description Computes the Least Common Multiple (LCM) of two integers.
  #' @param a Integer
  #' @param b Integer
  #' @usage find_lcm(a, b)
  #' @details This function uses the relationship between GCD and LCM,
  #' i.e., LCM(a, b) = |a * b| / GCD(a, b).
  #' LCM is useful in fraction operations and periodicity calculations.
  #' @references https://en.wikipedia.org/wiki/Least_common_multiple
  
  gcd <- function(x, y) {
    while (y != 0) {
      temp <- y
      y <- x %% y
      x <- temp
    }
    return(abs(x))
  }
  
  return(abs(a * b) / gcd(a, b))
}

# Examples
print(find_lcm(48, 18))  # expected 144
print(find_lcm(54, 24))  # expected 216

is_perfect_number <- function(n) {

  #' @description Checks if number passed as a parameter is a perfect number.
  #' @param n number to check
  #' @usage is_perfect_number(n)
  #' @details In number theory, a perfect number is a positive integer that
  #' is equal to the sum of its positive divisors, excluding the number itself.
  #' For instance, 6 has divisors 1, 2 and 3 (excluding itself)
  #' and 1 + 2 + 3 = 6, so 6 is a perfect number.
  #' @references https://en.wikipedia.org/wiki/Perfect_number

  if (n < 0) stop("Parameter n must have positive value")

  sum_of_divisors <- 0
  limit <- n - 1

  for (i in 1:limit) {
     if (n %% i == 0) {
        sum_of_divisors <- sum_of_divisors + i
     }
  }

  return(sum_of_divisors == n)
}

result <- is_perfect_number(4)
print(result)  # expected false

result <- is_perfect_number(5)
print(result)  # expected false

result <- is_perfect_number(6)
print(result)  # expected true

result <- is_perfect_number(7)
print(result)  # expected false

result <- is_perfect_number(28)
print(result)  # expected true
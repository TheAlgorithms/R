are_numbers_amicable <- function(x, y) {

  #' @description Checks if numbers passed as a parameter are amicable numbers.
  #' @param x first number to check
  #' @param y second number to check
  #' @usage are_numbers_amicable(x, y)
  #' @details Amicable numbers are two different natural numbers related
  #' in such a way that the sum of the proper divisors of each
  #' is equal to the other number.
  #' @references https://en.wikipedia.org/wiki/Amicable_numbers

  x_divisors_sum <- get_sum_of_divisors(x)
  y_divisors_sum <- get_sum_of_divisors(y)

  return((x_divisors_sum == y) && (y_divisors_sum == x))
}

get_sum_of_divisors <- function(n) {
  sum <- 0
  limit <- n - 1
  for (i in 1:limit) {
     if (n %% i == 0) {
        sum <- sum + i
     }
  }

  return(sum)
}

result <- are_numbers_amicable(220, 284)
print(result)  # expected true

result <- are_numbers_amicable(15, 100)
print(result)  # expected false
# Prime Number Checking in R
isPrime <- function(number) {
  (number == 2L || number == 3L) && return(TRUE)
  (number %% 2L == 0L || number %% 3L == 0L) && return(FALSE)
  s <- sqrt(number)
  k <- 1L
  while (6L * k - 1L <= s) {
    if (number %% (6L * k + 1L) == 0L || number %% (6L * k - 1L) == 0L)
      return(FALSE)
    k <- k + 1L
  }
  TRUE
}

isPrime(2)
isPrime(5)
isPrime(4)

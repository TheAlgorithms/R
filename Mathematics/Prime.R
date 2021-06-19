# Prime Number Checking in R

isPrime <- function(number) {
  if (number == 2) {
    return(TRUE)
  }
  
  for (x in 2:(number-1)) {
    if (number %% x == 0) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

isPrime(2)
isPrime(5)
isPrime(4)

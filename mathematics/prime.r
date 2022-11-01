# Prime Number Checking in R
isPrime <- function(number) {
  if (number == 2 | number == 3) {
    return(TRUE)
  } else if (number %% 2 == 0 | number %% 3 == 0){
    return(FALSE)
  } else {
    k <- 1
    while(6 * k - 1 <= sqrt(number)){
      if(number %% (6 * k + 1) == 0){
        return(FALSE)
      } else if(number %% (6 * k - 1) == 0){
        return(FALSE)
      }
      k <- k + 1
    }
    return(TRUE)
  }
}

isPrime(2)
isPrime(5)
isPrime(4)

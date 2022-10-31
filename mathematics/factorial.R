Fact <- function(n){
  if(n < 0){
    stop("Error: your input is wrong!")
  } else if(n == 0){
    return(1)
  } else {
    return(prod(1:n))
  }
}

Fact(5)
Fact(6)

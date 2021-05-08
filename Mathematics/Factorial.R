Fact <- function(n){
  if(n < 0) print("warning: your input is wrong!")
  else if(n == 0 | n == 1) print(1)
  else {
    s <- numeric()
    s[1] = 1
    for(i in 2 : n){
      s[i] = i * s[i - 1]
    }
    return(s[n])
  }
}

Fact(5)
Fact(6)

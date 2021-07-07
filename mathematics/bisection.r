
f <- function(x){
  x^3-x-2
}


bisection <- function(func, a, b){
  iters = ceiling(log((b-a)/tol, 2))
  if (f(a)*f(b)<=0){
    iter_range = 1:iters
    for (i in iter_range){
      c <- (a+b)*0.5
      if (f(c)/abs(f(c)) == f(a)/abs(f(a))){
        a<-c
      }
      else{
        b<-c
      }
    }
  }
 c
}

x<-bisection(f,1,2)
print(x)

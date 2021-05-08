ols<-function(y,x){
    x<-as.matrix(x)
    x<-cbind(intercept=1,x)
    return(solve(t(x) %*% x) %*% t(x) %*% y)
  }

  ols(y=diamonds$price,x=diamonds %>% select(-price)) %>% print()

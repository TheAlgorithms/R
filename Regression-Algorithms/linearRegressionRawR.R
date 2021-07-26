ols<-function(y,x){
    x<-as.matrix(x)
    x<-cbind(intercept=1,x)
    decomp <- svd(x)
    return(decomp$v %*% diag(1 / decomp$d) %*% t(decomp$u) %*% y)
  }

  ols(y=diamonds$price,x=diamonds %>% select(-price)) %>% print()

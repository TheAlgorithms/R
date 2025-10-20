ols<-function(y,x){
    data<-model.matrix(y ~ ., data = x)
    decomp <- svd(data)
    return(decomp$v %*% diag(1 / decomp$d) %*% t(decomp$u) %*% y)
  }

set.seed(1)
x <- rnorm(1000)
y <- 4 * x + rnorm(1000, sd = .5)
ols(y=y,x=matrix(x, ncol = 1))

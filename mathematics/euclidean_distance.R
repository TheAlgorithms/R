euclideanDistance <- function(x, y) {
    return(sqrt(sum((x - y)^2)))
}

set.seed(1)
x <- rnorm(1000)
y <- runif(1000)
print(euclideanDistance(x, y))
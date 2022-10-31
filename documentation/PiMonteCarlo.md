

```r
estimatePi <- function(numSims){
    x <- runif(numSims)
    y <- runif(numSims)
    inUnitCircle <- as.integer(x^2 + y^2 <= 1)
    return(4 * sum(inUnitCircle) / numSims)
}

set.seed(1)
estimatePi(3000)
```

```
## [1] 3.125333
```

```r
estimatePi(30000)
```

```
## [1] 3.132933
```


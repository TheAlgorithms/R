

```r
perfectSquare <- function(x){
    return(floor(sqrt(x)) == sqrt(x))
}

set.seed(1)
inputs <- sample(1:100, 10)
perfectSquare(inputs)
```

```
##  [1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
```


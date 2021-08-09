

```r
Fibonacci <- function(n)
{
  if(n == 1|n == 2)
  {
    return(1)
  }
  else
  {
    return(Fibonacci(n-1) + Fibonacci(n - 2))
  }
}

Fibonacci(1)
```

```
## [1] 1
```

```r
Fibonacci(11)
```

```
## [1] 89
```


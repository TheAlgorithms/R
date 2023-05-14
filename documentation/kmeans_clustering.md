

```r
set.seed(42)
cl <- kmeans(iris[,-5], 3)
plot(iris[,-5], col = cl$cluster)
points(cl$centers, col = 1:3, pch = 8)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)


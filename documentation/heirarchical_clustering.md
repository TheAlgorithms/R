

``` r
set.seed(42)
clusters <- hclust(dist(iris[, -5]))
plot(clusters)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)


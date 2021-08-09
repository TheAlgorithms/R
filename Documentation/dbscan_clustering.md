

```r
library(dbscan)
```

```
## Error in library(dbscan): there is no package called 'dbscan'
```

```r
cl <- dbscan(iris[,-5], eps = .5, minPts = 5)
```

```
## Error in dbscan(iris[, -5], eps = 0.5, minPts = 5): could not find function "dbscan"
```

```r
plot(iris[,-5], col = cl$cluster)
```

```
## Error in pairs.default(data.matrix(x), ...): object 'cl' not found
```


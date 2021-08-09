

```r
library(mclust) # Gaussian mixture model (GMM)
```

```
## Error in library(mclust): there is no package called 'mclust'
```

```r
gmm_fit <- Mclust(iris[, 1:4]) # Fit a GMM model
```

```
## Error in Mclust(iris[, 1:4]): could not find function "Mclust"
```

```r
summary(gmm_fit) # Summary table 
```

```
## Error in summary(gmm_fit): object 'gmm_fit' not found
```

```r
plot(gmm_fit, 'BIC') # Select model based on BIC
```

```
## Error in plot(gmm_fit, "BIC"): object 'gmm_fit' not found
```


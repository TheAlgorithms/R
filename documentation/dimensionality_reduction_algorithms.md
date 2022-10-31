

```r
library(stats)
pca <- princomp(train, cor = TRUE)
```

```
## Error in princomp(train, cor = TRUE): object 'train' not found
```

```r
train_reduced  <- predict(pca,train)
```

```
## Error in predict(pca, train): object 'pca' not found
```

```r
test_reduced  <- predict(pca,test)
```

```
## Error in predict(pca, test): object 'pca' not found
```


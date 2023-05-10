

```r
library(knn)
```

```
## Error in library(knn): there is no package called 'knn'
```

```r
x <- cbind(x_train,y_train)
```

```
## Error in cbind(x_train, y_train): object 'x_train' not found
```

```r
# Fitting model
fit <-knn(y_train ~ ., data = x,k=5)
```

```
## Error in knn(y_train ~ ., data = x, k = 5): could not find function "knn"
```

```r
summary(fit)
```

```
## Error in summary(fit): object 'fit' not found
```

```r
# Predict Output 
predicted= predict(fit,x_test)
```

```
## Error in predict(fit, x_test): object 'fit' not found
```


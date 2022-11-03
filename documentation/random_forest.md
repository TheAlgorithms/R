

```r
library(randomForest)
```

```
## Error in library(randomForest): there is no package called 'randomForest'
```

```r
x <- cbind(x_train,y_train)
```

```
## Error in cbind(x_train, y_train): object 'x_train' not found
```

```r
# Fitting model
fit <- randomForest(Species ~ ., x,ntree=500)
```

```
## Error in randomForest(Species ~ ., x, ntree = 500): could not find function "randomForest"
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


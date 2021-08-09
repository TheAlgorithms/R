

```r
library(e1071)
```

```
## Error in library(e1071): there is no package called 'e1071'
```

```r
x <- cbind(x_train,y_train)
```

```
## Error in cbind(x_train, y_train): object 'x_train' not found
```

```r
# Fitting model
fit <-naiveBayes(y_train ~ ., data = x)
```

```
## Error in naiveBayes(y_train ~ ., data = x): could not find function "naiveBayes"
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


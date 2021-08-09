

```r
library(rpart)
x <- cbind(x_train,y_train)
```

```
## Error in cbind(x_train, y_train): object 'x_train' not found
```

```r
# grow tree 
fit <- rpart(y_train ~ ., data = x,method="class")
```

```
## Error in model.frame.default(formula = y_train ~ ., data = x, na.action = function (x) : 'data' must be a data.frame, environment, or list
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


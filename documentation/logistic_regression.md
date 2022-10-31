

```r
x <- cbind(x_train,y_train)
```

```
## Error in cbind(x_train, y_train): object 'x_train' not found
```

```r
# Train the model using the training sets and check score
logistic <- glm(y_train ~ ., data = x,family='binomial')
```

```
## Error in model.frame.default(formula = y_train ~ ., data = x, drop.unused.levels = TRUE): 'data' must be a data.frame, environment, or list
```

```r
summary(logistic)
```

```
## Error in summary(logistic): object 'logistic' not found
```

```r
# Predict Output
predicted= predict(logistic,x_test)
```

```
## Error in predict(logistic, x_test): object 'logistic' not found
```


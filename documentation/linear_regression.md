

``` r
# Load Train and Test datasets
# Identify feature and response variable(s) and values must be numeric and numpy arrays
x_train <- input_variables_values_training_datasets
```

```
## Error: object 'input_variables_values_training_datasets' not found
```

``` r
y_train <- target_variables_values_training_datasets
```

```
## Error: object 'target_variables_values_training_datasets' not found
```

``` r
x_test <- input_variables_values_test_datasets
```

```
## Error: object 'input_variables_values_test_datasets' not found
```

``` r
x <- cbind(x_train,y_train)
```

```
## Error in cbind(x_train, y_train): object 'x_train' not found
```

``` r
# Train the model using the training sets and check score
linear <- lm(y_train ~ ., data = x)
```

```
## Error in model.frame.default(formula = y_train ~ ., data = x, drop.unused.levels = TRUE): 'data' must be a data.frame, environment, or list
```

``` r
summary(linear)
```

```
## Error in summary(linear): object 'linear' not found
```

``` r
# Predict Output
predicted= predict(linear,x_test) 
```

```
## Error in predict(linear, x_test): object 'linear' not found
```


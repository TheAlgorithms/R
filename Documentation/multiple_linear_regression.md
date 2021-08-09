

```r
# Introduction to multiple linear regression

# lm stands for Linear Model
# y_data are modeled as a.x1 + b.x2 + c.x3 + d.x4 + e
mod3 <- lm(y_data~x1_data+x2_data+x3_data+x4_data, data=name_of_the_dataframe)
```

```
## Error in is.data.frame(data): object 'name_of_the_dataframe' not found
```

```r
# displays the output of the model computed by the previous line
summary(mod3)
```

```
## Error in summary(mod3): object 'mod3' not found
```

```r
# modeled data : it predicts the output for x_test_data as input information for the model
predicted <- predict(mod3, x1_test_data, x2_test_data, x3_test_data, x4_test_data)
```

```
## Error in predict(mod3, x1_test_data, x2_test_data, x3_test_data, x4_test_data): object 'mod3' not found
```


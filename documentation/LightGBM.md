

```r
library(RLightGBM)
```

```
## Error in library(RLightGBM): there is no package called 'RLightGBM'
```

```r
data(example.binary)
```

```
## Warning in data(example.binary): data set 'example.binary' not found
```

```r
#Parameters

num_iterations <- 100
config <- list(objective = "binary",  metric="binary_logloss,auc", learning_rate = 0.1, num_leaves = 63, tree_learner = "serial", feature_fraction = 0.8, bagging_freq = 5, bagging_fraction = 0.8, min_data_in_leaf = 50, min_sum_hessian_in_leaf = 5.0)

#Create data handle and booster
handle.data <- lgbm.data.create(x)
```

```
## Error in lgbm.data.create(x): could not find function "lgbm.data.create"
```

```r
lgbm.data.setField(handle.data, "label", y)
```

```
## Error in lgbm.data.setField(handle.data, "label", y): could not find function "lgbm.data.setField"
```

```r
handle.booster <- lgbm.booster.create(handle.data, lapply(config, as.character))
```

```
## Error in lgbm.booster.create(handle.data, lapply(config, as.character)): could not find function "lgbm.booster.create"
```

```r
#Train for num_iterations iterations and eval every 5 steps

lgbm.booster.train(handle.booster, num_iterations, 5)
```

```
## Error in lgbm.booster.train(handle.booster, num_iterations, 5): could not find function "lgbm.booster.train"
```

```r
#Predict
pred <- lgbm.booster.predict(handle.booster, x.test)
```

```
## Error in lgbm.booster.predict(handle.booster, x.test): could not find function "lgbm.booster.predict"
```

```r
#Test accuracy
sum(y.test == (y.pred > 0.5)) / length(y.test)
```

```
## Error in eval(expr, envir, enclos): object 'y.test' not found
```

```r
#Save model (can be loaded again via lgbm.booster.load(filename))
lgbm.booster.save(handle.booster, filename = "/tmp/model.txt")
```

```
## Error in lgbm.booster.save(handle.booster, filename = "/tmp/model.txt"): could not find function "lgbm.booster.save"
```


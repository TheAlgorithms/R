

```r
library(tidyverse)
```

```
## Error in library(tidyverse): there is no package called 'tidyverse'
```

```r
library(xgboost)
```

```
## Error in library(xgboost): there is no package called 'xgboost'
```

```r
ind<-sample(2,nrow(diamonds),replace = T,prob = c(0.7,0.3))
```

```
## Error in nrow(diamonds): object 'diamonds' not found
```

```r
train.set<-diamonds[ind==1,]
```

```
## Error in eval(expr, envir, enclos): object 'diamonds' not found
```

```r
test.set<-diamonds[ind==2,]
```

```
## Error in eval(expr, envir, enclos): object 'diamonds' not found
```

```r
xgb.train<-bind_cols(select_if(train.set,is.numeric),model.matrix(~cut-1,train.set) %>% as.tibble(),model.matrix(~color-1,train.set) %>% as.tibble(),model.matrix(~clarity-1,train.set) %>% as.tibble())
```

```
## Error in bind_cols(select_if(train.set, is.numeric), model.matrix(~cut - : could not find function "bind_cols"
```

```r
xgboost.train<-xgb.DMatrix(data = as.matrix(select(xgb.train,-price)),label=xgb.train$price)
```

```
## Error in xgb.DMatrix(data = as.matrix(select(xgb.train, -price)), label = xgb.train$price): could not find function "xgb.DMatrix"
```

```r
xgb.test<-bind_cols(select_if(test.set,is.numeric),model.matrix(~cut-1,test.set) %>% as.tibble(),model.matrix(~color-1,test.set) %>% as.tibble(),model.matrix(~clarity-1,test.set) %>% as.tibble())
```

```
## Error in bind_cols(select_if(test.set, is.numeric), model.matrix(~cut - : could not find function "bind_cols"
```

```r
xgboost.test<-xgb.DMatrix(data = select(xgb.test,-price) %>% as.matrix(),label=xgb.test$price)
```

```
## Error in xgb.DMatrix(data = select(xgb.test, -price) %>% as.matrix(), : could not find function "xgb.DMatrix"
```

```r
param<-list(eval_metric='rmse',gamma=1,max_depth=6,nthread = 3)
xg.model<-xgb.train(data = xgboost.train,params = param,watchlist = list(test=xgboost.test),nrounds = 500,early_stopping_rounds = 60,
                      print_every_n = 30)
```

```
## Error in xgb.train(data = xgboost.train, params = param, watchlist = list(test = xgboost.test), : could not find function "xgb.train"
```

```r
xg.predict<-predict(xg.model,xgboost.test)
```

```
## Error in predict(xg.model, xgboost.test): object 'xg.model' not found
```

```r
mse.xgb<-sqrt(mean((test.set$price-xg.predict)^2))
```

```
## Error in mean((test.set$price - xg.predict)^2): object 'test.set' not found
```

```r
plot((test.set$price-xg.predict))
```

```
## Error in plot((test.set$price - xg.predict)): object 'test.set' not found
```




```r
data(ggplot2::diamonds)
```

```
## Warning in data(ggplot2::diamonds): data set 'ggplot2::diamonds' not found
```

```r
library(caret)
```

```
## Error in library(caret): there is no package called 'caret'
```

```r
library(dplyr)
```

```
## Error in library(dplyr): there is no package called 'dplyr'
```

```r
dia.trans<-bind_cols(diamonds %>% select_if(is.numeric),
                     model.matrix(~cut-1,diamonds) %>% as_tibble(),
                     model.matrix(~color-1,diamonds) %>% as_tibble(),
                     model.matrix(~clarity-1,diamonds) %>% as_tibble())
```

```
## Error in bind_cols(diamonds %>% select_if(is.numeric), model.matrix(~cut - : could not find function "bind_cols"
```

```r
#setting parameters alpha and lambda
lasso_expand<-expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
lasso_mod <- train(x=dia.trans %>% select(-price), y=dia.trans$price, method='glmnet', 
                   tuneGrid=lasso_expand)
```

```
## Error in train(x = dia.trans %>% select(-price), y = dia.trans$price, : could not find function "train"
```

```r
#best tune
lasso_mod$bestTune
```

```
## Error in eval(expr, envir, enclos): object 'lasso_mod' not found
```

```r
lasso_mod$results$RMSE
```

```
## Error in eval(expr, envir, enclos): object 'lasso_mod' not found
```

```r
lasso_imp<-varImp(lasso_mod)
```

```
## Error in varImp(lasso_mod): could not find function "varImp"
```

```r
#get the importance of each feature and eliminate some of them
lasso_imp$importance
```

```
## Error in eval(expr, envir, enclos): object 'lasso_imp' not found
```


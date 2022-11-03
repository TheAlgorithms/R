data(ggplot2::diamonds)
library(caret)
library(dplyr)
dia.trans<-bind_cols(diamonds %>% select_if(is.numeric),
                     model.matrix(~cut-1,diamonds) %>% as_tibble(),
                     model.matrix(~color-1,diamonds) %>% as_tibble(),
                     model.matrix(~clarity-1,diamonds) %>% as_tibble())

#setting parameters alpha and lambda
lasso_expand<-expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
lasso_mod <- train(x=dia.trans %>% select(-price), y=dia.trans$price, method='glmnet', 
                   tuneGrid=lasso_expand)

#best tune
lasso_mod$bestTune
lasso_mod$results$RMSE

lasso_imp<-varImp(lasso_mod)
#get the importance of each feature and eliminate some of them
lasso_imp$importance

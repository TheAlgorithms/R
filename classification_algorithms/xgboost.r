library(tidyverse)
library(xgboost)

ind<-sample(2,nrow(diamonds),replace = T,prob = c(0.7,0.3))
train.set<-diamonds[ind==1,]
test.set<-diamonds[ind==2,]

xgb.train<-bind_cols(select_if(train.set,is.numeric),model.matrix(~cut-1,train.set) %>% as.tibble(),model.matrix(~color-1,train.set) %>% as.tibble(),model.matrix(~clarity-1,train.set) %>% as.tibble())
xgboost.train<-xgb.DMatrix(data = as.matrix(select(xgb.train,-price)),label=xgb.train$price)
xgb.test<-bind_cols(select_if(test.set,is.numeric),model.matrix(~cut-1,test.set) %>% as.tibble(),model.matrix(~color-1,test.set) %>% as.tibble(),model.matrix(~clarity-1,test.set) %>% as.tibble())
xgboost.test<-xgb.DMatrix(data = select(xgb.test,-price) %>% as.matrix(),label=xgb.test$price)

param<-list(eval_metric='rmse',gamma=1,max_depth=6,nthread = 3)
xg.model<-xgb.train(data = xgboost.train,params = param,watchlist = list(test=xgboost.test),nrounds = 500,early_stopping_rounds = 60,
                      print_every_n = 30)
                      
xg.predict<-predict(xg.model,xgboost.test)
mse.xgb<-sqrt(mean((test.set$price-xg.predict)^2))
plot((test.set$price-xg.predict))

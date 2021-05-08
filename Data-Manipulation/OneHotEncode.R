library(tidyverse)
#divide data into train and test in 70% and 30%
ind<-sample(2,nrow(diamonds),replace = T,prob = c(0.7,0.3))
train.set<-diamonds[ind==1,]
test.set<-diamonds[ind==2,]

# one hot encode using model.matrix(built-in function)
#along with %>%, select_if, bind_cols, as.tibble in dplyr and tibble
train<-bind_cols(select_if(train.set,is.numeric),model.matrix(~cut-1,train.set) %>% as.tibble(),
                model.matrix(~color-1,train.set) %>% as.tibble(),model.matrix(~clarity-1,train.set) %>% as.tibble())
test<-bind_cols(select_if(test.set,is.numeric),model.matrix(~cut-1,test.set) %>% as.tibble(),
                model.matrix(~color-1,test.set) %>% as.tibble(),model.matrix(~clarity-1,test.set) %>% as.tibble())

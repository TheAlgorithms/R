library(tidyverse)
#Divide data into train and test in 70% and 30%
ind<-sample(2,nrow(diamonds),replace = T,prob = c(0.7,0.3))
train.set <- diamonds[ind==1,]
test.set <- diamonds[ind==2,]

#Combine the dataset using rbind function(inbuilt function)
combi <- rbind(train.set, test.set)

##Label Encoding
combi[, cut_num := ifelse(cut == "Fair",0,
                                   ifelse(cut == "Good",1,
                                   ifelse(cut == "Very Good",2,
                                   ifelse(cut == "Premium",3,4))))]
combi[, color_num := ifelse(color == "D",0,
                                   ifelse(color == "E",2,
                                   ifelse(color == "F",3,
                                   ifelse(color == "G",4,
                                   ifelse(color == "H",5,
                                   ifelse(color == "I",6,7))))))]

# Column "clarity" won't be taken in label encoding as it contains more variables.
#The more variables in column in label encoding, the model will perform less.

#Removing categorical variables after label encoding
combi[,c("color", "cut") := NULL)
                                                     
#Divide data back into train and test in 70% and 30%
ind<-sample(2,nrow(combi),replace = T,prob = c(0.7,0.3))
train.set <- combi[ind==1,]
test.set <- combi[ind==2,]

# Introduction to the Multinomial Model

rm(list = ls())

# \Packages

library(nnet)
library(rsample)

# Model 
model_mult <- multinom(Class~., data = base_train)

# Example
# Train and Test 

db = datasets::iris

data_split <- rsample::initial_split(data = db,
                                     prop = 0.75)

base_train = rsample::training(data_split)
base_test  = rsample::testing(data_split)

# Example - Model 

model_mult <- nnet::multinom(Species~., data = base_train)

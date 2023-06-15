
rm(list = ls())
graphics.off()

#\ Package 
library(glmnet)
library(tidyverse)

# make iris dataset a binary dataset
iris_raw <- datasets::iris %>% 
            janitor::clean_names() %>% 
            dplyr::filter(species != "versicolor") %>% 
            dplyr::mutate(species = as.factor(as.character(species)))
  
# model

cv.fit <- glmnet::cv.glmnet(x = as.matrix(iris_raw[1:4]),
                            y = iris_raw$species,
                            family = 'binomial',
                            type.measure = "auc") 

# results 
print(cv.fit)
plot(cv.fit)

# coefficients of each varibale

coefficient <- stats::coef(cv.fit$glmnet.fit, s = cv.fit$lambda.min)
coefficient %>% print()

# predict the fitted probability of each test observation
stats::predict(cv.fit$glmnet.fit, 
              as.matrix(iris_raw[1:5, 1:4]), 
              type = 'response',
              s = cv.fit$lambda.min)


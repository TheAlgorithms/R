# Introduction to logistic regression

# glm stands for Generalized Linear Model
mod1 <- glm(y_data~x_data, data=name_of_the_dataframe, family="binomial")

# displays the output of the model computed by the previous line
summary(mod1)

# modeled data : it predicts the output for x_test_data as input information for the model
predicted <- predict(mod1, x_test_data)

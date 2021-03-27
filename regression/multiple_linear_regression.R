# Introduction to multiple linear regression

# lm stands for Linear Model
# y_data are modeled as a.x1 + b.x2 + c.x3 + d.x4 + e
mod3 <- lm(y_data~x1_data+x2_data+x3_data+x4_data, data=name_of_the_dataframe)

# displays the output of the model computed by the previous line
summary(mod3)

# modeled data : it predicts the output for x_test_data as input information for the model
predicted <- predict(mod3, x1_test_data, x2_test_data, x3_test_data, x4_test_data)

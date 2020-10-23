library(glmnet)

# make iris dataset a binary dataset

iris.mdy <- iris[iris$Species != 'versicolor',]
iris.mdy$Species <- as.character(iris.mdy$Species)
# level virginica is the target class
iris.mdy$Species <- as.factor(iris.mdy$Species)
cv.fit <- cv.glmnet(x=as.matrix(iris.mdy[, 1:4]),
                                y=iris.mdy$Species,
                                family = 'binomial',
                                type.measure="auc"
                    )

plot(cv.fit)

# coefficients of each varibale
coefficient<-coef(cv.fit$glmnet.fit, s=cv.fit$lambda.min)

# predict the fitted probability of each test observation
predict(cv.fit$glmnet.fit, 
        as.matrix(iris.mdy[1:5, 1:4]), 
        type = 'response',
        s=cv.fit$lambda.min)


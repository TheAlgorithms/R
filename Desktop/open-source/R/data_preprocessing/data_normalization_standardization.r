# normalization & standardization
normalization<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

standardization<-function(x){
  return((x-mean(x))/sd(x))
}

head(iris)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa

iris<-iris[,-5]
head(iris)
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# 1          5.1         3.5          1.4         0.2
# 2          4.9         3.0          1.4         0.2
# 3          4.7         3.2          1.3         0.2
# 4          4.6         3.1          1.5         0.2
# 5          5.0         3.6          1.4         0.2
# 6          5.4         3.9          1.7         0.4

#normalize
apply(as.matrix(iris),2,normalization)
# Sepal.Length Sepal.Width Petal.Length Petal.Width
# [1,]   0.22222222  0.62500000   0.06779661  0.04166667
# [2,]   0.16666667  0.41666667   0.06779661  0.04166667
# [3,]   0.11111111  0.50000000   0.05084746  0.04166667
# [4,]   0.08333333  0.45833333   0.08474576  0.04166667
# [5,]   0.19444444  0.66666667   0.06779661  0.04166667
# [6,]   0.30555556  0.79166667   0.11864407  0.12500000
# [7,]   0.08333333  0.58333333   0.06779661  0.08333333

#standardize
apply(as.matrix(iris),2,standardization)
# Sepal.Length Sepal.Width Petal.Length   Petal.Width
# [1,]  -0.89767388  1.01560199  -1.33575163 -1.3110521482
# [2,]  -1.13920048 -0.13153881  -1.33575163 -1.3110521482
# [3,]  -1.38072709  0.32731751  -1.39239929 -1.3110521482
# [4,]  -1.50149039  0.09788935  -1.27910398 -1.3110521482
# [5,]  -1.01843718  1.24503015  -1.33575163 -1.3110521482
# [6,]  -0.53538397  1.93331463  -1.16580868 -1.0486667950
# [7,]  -1.50149039  0.78617383  -1.33575163 -1.1798594716
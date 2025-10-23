library(dbscan)
cl <- dbscan(iris[,-5], eps = .5, minPts = 5)
plot(iris[,-5], col = cl$cluster)

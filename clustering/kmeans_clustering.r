cl <- kmeans(iris[,-5], 3)
plot(iris[,-5], col = cl$cluster)
points(cl$centers, col = 1:3, pch = 8)

clusters <- hclust(dist(iris[, -5]))
plot(clusters)

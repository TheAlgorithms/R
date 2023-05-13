set.seed(42)
clusters <- hclust(dist(iris[, -5]))
plot(clusters)

library(mclust) # Gaussian mixture model (GMM)
gmm_fit <- Mclust(iris[, 1:4]) # Fit a GMM model
summary(gmm_fit) # Summary table 
plot(gmm_fit, 'BIC') # Select model based on BIC

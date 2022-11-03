library(cluster) 
pam_fit <- pam(iris[, 1:4], 5) # Partition Around Medoids
summary(pam_fit) # Get summary

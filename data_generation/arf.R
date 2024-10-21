library(arf)

# training an arf
arf <-adversarial_rf(iris)

# estimate distribution parameters
psi <- forde(arf,iris)

# generate 1000 synthetic samples
one_thousand_samples <- forge(psi,1000)


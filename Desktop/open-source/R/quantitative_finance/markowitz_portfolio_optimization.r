# Required libraries
library(tidyquant)
library(quadprog)
# Set a seed for reproducibility
set.seed(123)
# Generate random data for three assets
num_assets <- 3
num_obs <- 100
returns <- matrix(rnorm(num_assets * num_obs), ncol = num_assets)
# Define the objective function for portfolio optimization
objective_function <- function(weights, cov_matrix) {
  portfolio_return <- sum(weights * colMeans(returns))
  portfolio_volatility <- sqrt(t(weights) %*% cov_matrix %*% weights)
  return(c(portfolio_return, portfolio_volatility))
}
cov_matrix <- cov(returns)
constraints <- matrix(0, nrow = 2, ncol = num_assets)
constraints[1, ] <- colMeans(returns)
constraints[2, ] <- 1
optimal_weights <- solve.QP(Dmat = 2 * cov_matrix,
                            dvec = rep(0, num_assets),
                            Amat = t(constraints),
                            bvec = c(0.05, 1),
                            meq = 1)$solution
cat("Optimal Weights:", optimal_weights, "\n")
optimal_portfolio <- objective_function(optimal_weights, cov_matrix)
cat("Expected Return:", optimal_portfolio[1], "\n")
cat("Volatility:", optimal_portfolio[2], "\n")

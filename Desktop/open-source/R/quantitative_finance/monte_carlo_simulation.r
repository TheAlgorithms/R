# Required libraries
library("quantmod")
# Parameters
S0 <- 100    # Initial stock price
K <- 100     # Strike price
r <- 0.05    # Risk-free rate
sigma <- 0.2 # Volatility
T <- 1       # Time to maturity (in years)
n <- 252     # Number of trading days
# Function to simulate stock prices using geometric Brownian motion
simulate_stock_prices <- function(S0, r, sigma, T, n) {
  dt <- T/n
  t <- seq(0, T, by = dt)
  W <- c(0, cumsum(sqrt(dt) * rnorm(n)))
  S <- S0 * exp((r - 0.5 * sigma^2) * t + sigma * W)
  return(S)
}
# Function to calculate option price using Monte Carlo simulation
monte_carlo_option_price <- function(S0, K, r, sigma, T, n, num_simulations) {
  option_prices <- numeric(num_simulations)
  for (i in 1:num_simulations) {
    ST <- simulate_stock_prices(S0, r, sigma, T, n)[n + 1] # Final stock price
    option_prices[i] <- pmax(ST - K, 0) # Payoff of the option
  }
  option_price <- mean(option_prices) * exp(-r * T) # Discounted expected payoff
  return(option_price)
}
# Number of Monte Carlo simulations
num_simulations <- 10000
option_price <- monte_carlo_option_price(S0, K, r, sigma, T, n, num_simulations)
cat("Option price:", option_price, "\n")

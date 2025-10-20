library(Metrics)
set.seed(123)
num_obs <- 100
true_returns <- rnorm(num_obs, mean = 0.001, sd = 0.01)
observed_prices <- cumprod(1 + true_returns) * 100
noise <- rnorm(num_obs, mean = 0, sd = 0.1)
noisy_prices <- observed_prices + noise
# Kalman filter implementation
kalman_filter <- function(observed_prices) {
  state <- c(observed_prices[1], 0)
  P <- matrix(c(1, 0, 0, 1), nrow = 2)
  Q <- matrix(c(0.0001, 0, 0, 0.0001), nrow = 2)
  R <- 0.1
  A <- matrix(c(1, 1, 0, 1), nrow = 2)
  H <- matrix(c(1, 0), nrow = 1)
  filtered_states <- matrix(0, nrow = length(observed_prices), ncol = 2)
  for (i in 1:length(observed_prices)) {
    state_pred <- A %*% state
    P_pred <- A %*% P %*% t(A) + Q
    K <- P_pred %*% t(H) %*% solve(H %*% P_pred %*% t(H) + R)
    state <- state_pred + K %*% (observed_prices[i] - H %*% state_pred)
    P <- (matrix(1, nrow = 2, ncol = 2) - K %*% H) %*% P_pred
    filtered_states[i, ] <- state
  }
  return(list(filtered_states = filtered_states, state_pred = state_pred, P_pred = P_pred))
}
result <- kalman_filter(noisy_prices)
plot(observed_prices, type = "l", col = "blue", lwd = 2, main = "Kalman Filter")
lines(result$filtered_states[, 1], type = "l", col = "red", lwd = 2)
lines(true_returns, type = "l", col = "green", lwd = 2)
legend("topright", legend = c("Observed Prices", "Filtered Prices", "True Returns"),
       col = c("blue", "red", "green"), lty = 1, lwd = 2)

# Black-Scholes Option Pricing Algorithm in R
# Implements the Black-Scholes-Merton model for European option pricing
# Features: Call/Put pricing, Greeks calculation, and implied volatility estimation

library(R6)

#' BlackScholesCalculator Class
#' @description R6 class for option pricing using Black-Scholes model
#' @details Calculates option prices and Greeks for European options
#' Assumptions:
#' - No dividend payments
#' - European-style options (can only be exercised at expiration)
#' - Log-normal distribution of stock prices
#' - Constant risk-free rate and volatility
#' - No transaction costs or taxes
#' - Perfectly divisible securities
BlackScholesCalculator <- R6Class(
  "BlackScholesCalculator",
  
  public = list(
    #' @description Initialize calculator with market parameters
    #' @param r Risk-free interest rate (annualized)
    #' @param include_checks Whether to perform parameter validation
    initialize = function(r = 0.05, include_checks = TRUE) {
      private$risk_free_rate <- r
      private$validate_params <- include_checks
      invisible(self)
    },

    #' @description Calculate call option price
    #' @param S Current stock price
    #' @param K Strike price
    #' @param T Time to expiration (in years)
    #' @param sigma Volatility (annualized)
    calculate_call_price = function(S, K, T, sigma) {
      if (private$validate_params) {
        private$validate_inputs(S, K, T, sigma)
      }
      
      d1 <- private$calculate_d1(S, K, T, sigma)
      d2 <- private$calculate_d2(d1, sigma, T)
      
      call_price <- S * stats::pnorm(d1) - K * exp(-private$risk_free_rate * T) * stats::pnorm(d2)
      return(call_price)
    },

    #' @description Calculate put option price
    #' @param S Current stock price
    #' @param K Strike price
    #' @param T Time to expiration (in years)
    #' @param sigma Volatility (annualized)
    calculate_put_price = function(S, K, T, sigma) {
      if (private$validate_params) {
        private$validate_inputs(S, K, T, sigma)
      }
      
      d1 <- private$calculate_d1(S, K, T, sigma)
      d2 <- private$calculate_d2(d1, sigma, T)
      
      put_price <- K * exp(-private$risk_free_rate * T) * stats::pnorm(-d2) - S * stats::pnorm(-d1)
      return(put_price)
    },

    #' @description Calculate all Greeks for a call option
    #' @param S Current stock price
    #' @param K Strike price
    #' @param T Time to expiration (in years)
    #' @param sigma Volatility (annualized)
    calculate_call_greeks = function(S, K, T, sigma) {
      if (private$validate_params) {
        private$validate_inputs(S, K, T, sigma)
      }
      
      d1 <- private$calculate_d1(S, K, T, sigma)
      d2 <- private$calculate_d2(d1, sigma, T)
      
      # Calculate Greeks
      delta <- stats::pnorm(d1)
      gamma <- stats::dnorm(d1) / (S * sigma * sqrt(T))
      theta <- (-S * stats::dnorm(d1) * sigma / (2 * sqrt(T)) - 
                private$risk_free_rate * K * exp(-private$risk_free_rate * T) * stats::pnorm(d2))
      vega <- S * sqrt(T) * stats::dnorm(d1)
      rho <- K * T * exp(-private$risk_free_rate * T) * stats::pnorm(d2)
      
      return(list(
        delta = delta,
        gamma = gamma,
        theta = theta,
        vega = vega,
        rho = rho
      ))
    },

    #' @description Calculate all Greeks for a put option
    #' @param S Current stock price
    #' @param K Strike price
    #' @param T Time to expiration (in years)
    #' @param sigma Volatility (annualized)
    calculate_put_greeks = function(S, K, T, sigma) {
      if (private$validate_params) {
        private$validate_inputs(S, K, T, sigma)
      }
      
      d1 <- private$calculate_d1(S, K, T, sigma)
      d2 <- private$calculate_d2(d1, sigma, T)
      
      # Calculate Greeks
      delta <- stats::pnorm(d1) - 1
      gamma <- stats::dnorm(d1) / (S * sigma * sqrt(T))
      theta <- (-S * stats::dnorm(d1) * sigma / (2 * sqrt(T)) + 
                private$risk_free_rate * K * exp(-private$risk_free_rate * T) * stats::pnorm(-d2))
      vega <- S * sqrt(T) * stats::dnorm(d1)
      rho <- -K * T * exp(-private$risk_free_rate * T) * stats::pnorm(-d2)
      
      return(list(
        delta = delta,
        gamma = gamma,
        theta = theta,
        vega = vega,
        rho = rho
      ))
    },

    #' @description Estimate implied volatility using Newton-Raphson method
    #' @param market_price Observed market price of the option
    #' @param S Current stock price
    #' @param K Strike price
    #' @param T Time to expiration (in years)
    #' @param is_call Whether the option is a call (TRUE) or put (FALSE)
    #' @param tolerance Convergence tolerance
    #' @param max_iter Maximum iterations
    estimate_implied_volatility = function(market_price, S, K, T, 
                                         is_call = TRUE, tolerance = 1e-5, max_iter = 100) {
      if (private$validate_params) {
        if (market_price <= 0) stop("Market price must be positive")
        private$validate_inputs(S, K, T, 0.5)  # Initial volatility check
      }
      
      # Initial guess for volatility
      sigma <- sqrt(2 * abs(log(S/K) + private$risk_free_rate * T) / T)
      sigma <- min(max(0.1, sigma), 5)  # Bound initial guess
      
      for (i in 1:max_iter) {
        # Calculate price and vega
        if (is_call) {
          price <- self$calculate_call_price(S, K, T, sigma)
          greeks <- self$calculate_call_greeks(S, K, T, sigma)
        } else {
          price <- self$calculate_put_price(S, K, T, sigma)
          greeks <- self$calculate_put_greeks(S, K, T, sigma)
        }
        
        diff <- price - market_price
        
        if (abs(diff) < tolerance) {
          return(sigma)
        }
        
        # Update volatility estimate using Newton-Raphson
        sigma <- sigma - diff / greeks$vega
        
        # Bound the volatility
        sigma <- min(max(0.001, sigma), 5)
      }
      
      warning("Implied volatility did not converge")
      return(sigma)
    }
  ),
  
  private = list(
    risk_free_rate = NULL,
    validate_params = NULL,
    
    calculate_d1 = function(S, K, T, sigma) {
      (log(S/K) + (private$risk_free_rate + sigma^2/2) * T) / (sigma * sqrt(T))
    },
    
    calculate_d2 = function(d1, sigma, T) {
      d1 - sigma * sqrt(T)
    },
    
    validate_inputs = function(S, K, T, sigma) {
      if (S <= 0) stop("Stock price must be positive")
      if (K <= 0) stop("Strike price must be positive")
      if (T <= 0) stop("Time to expiration must be positive")
      if (sigma <= 0) stop("Volatility must be positive")
    }
  )
)

# Demonstration
demonstrate_black_scholes <- function() {
  cat("=== Black-Scholes Option Pricing Demo ===\n\n")
  
  # Initialize calculator
  bs <- BlackScholesCalculator$new(r = 0.05)
  
  # Example parameters
  S <- 100    # Current stock price
  K <- 100    # Strike price
  T <- 1      # One year to expiration
  sigma <- 0.2 # 20% volatility
  
  # Calculate option prices
  call_price <- bs$calculate_call_price(S, K, T, sigma)
  put_price <- bs$calculate_put_price(S, K, T, sigma)
  
  cat("Parameters:\n")
  cat(sprintf("Stock Price: $%.2f\n", S))
  cat(sprintf("Strike Price: $%.2f\n", K))
  cat(sprintf("Time to Expiration: %.1f years\n", T))
  cat(sprintf("Volatility: %.1f%%\n", sigma * 100))
  cat(sprintf("Risk-free Rate: %.1f%%\n\n", bs$risk_free_rate * 100))
  
  cat("Option Prices:\n")
  cat(sprintf("Call Option: $%.2f\n", call_price))
  cat(sprintf("Put Option: $%.2f\n\n", put_price))
  
  # Calculate and display Greeks
  call_greeks <- bs$calculate_call_greeks(S, K, T, sigma)
  put_greeks <- bs$calculate_put_greeks(S, K, T, sigma)
  
  cat("Call Option Greeks:\n")
  cat(sprintf("Delta: %.4f\n", call_greeks$delta))
  cat(sprintf("Gamma: %.4f\n", call_greeks$gamma))
  cat(sprintf("Theta: %.4f\n", call_greeks$theta))
  cat(sprintf("Vega: %.4f\n", call_greeks$vega))
  cat(sprintf("Rho: %.4f\n\n", call_greeks$rho))
  
  cat("Put Option Greeks:\n")
  cat(sprintf("Delta: %.4f\n", put_greeks$delta))
  cat(sprintf("Gamma: %.4f\n", put_greeks$gamma))
  cat(sprintf("Theta: %.4f\n", put_greeks$theta))
  cat(sprintf("Vega: %.4f\n", put_greeks$vega))
  cat(sprintf("Rho: %.4f\n\n", put_greeks$rho))
  
  # Demonstrate implied volatility calculation
  test_market_price <- call_price * 1.1  # Use 10% higher price for demonstration
  implied_vol <- bs$estimate_implied_volatility(test_market_price, S, K, T, is_call = TRUE)
  cat("Implied Volatility Estimation:\n")
  cat(sprintf("Market Price: $%.2f\n", test_market_price))
  cat(sprintf("Implied Volatility: %.1f%%\n", implied_vol * 100))
  
  cat("\n=== Demo Complete ===\n")
}

# Run demonstration if not in interactive mode
if (!interactive()) {
  demonstrate_black_scholes()
}
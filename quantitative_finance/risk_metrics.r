# Value at Risk (VaR) and Expected Shortfall (ES) Calculator
# Implements multiple VaR calculation methods and Expected Shortfall
# Features: Historical, Parametric, and Monte Carlo VaR/ES calculations

library(R6)

#' RiskMetrics Class
#' @description R6 class for calculating Value at Risk and Expected Shortfall
#' @details Implements multiple methods for VaR and ES calculation:
#' - Historical simulation (non-parametric)
#' - Parametric (variance-covariance)
#' - Monte Carlo simulation
#' Time complexity varies by method, documented per method
RiskMetrics <- R6Class(
  "RiskMetrics",
  
  public = list(
    #' @description Initialize risk calculator
    #' @param returns Historical returns data
    #' @param confidence_level Confidence level for VaR/ES (default 0.95)
    #' @param time_horizon Time horizon in days (default 1)
    initialize = function(returns = NULL, confidence_level = 0.95, time_horizon = 1) {
      private$validate_parameters(confidence_level, time_horizon)
      
      self$returns <- returns
      self$confidence_level <- confidence_level
      self$time_horizon <- time_horizon
      
      if (!is.null(returns)) {
        private$fit_distribution()
      }
    },
    
    #' @description Calculate Historical VaR
    #' @param portfolio_value Current portfolio value
    #' @param method Calculation method ('historical', 'parametric', or 'monte_carlo')
    #' @param n_simulations Number of simulations for Monte Carlo method
    calculate_var = function(portfolio_value, method = "historical", n_simulations = 10000) {
      if (is.null(self$returns)) {
        stop("No returns data available. Please initialize with returns data.")
      }
      
      method <- match.arg(method, c("historical", "parametric", "monte_carlo"))
      
      var_value <- switch(method,
        "historical" = private$calculate_historical_var(portfolio_value),
        "parametric" = private$calculate_parametric_var(portfolio_value),
        "monte_carlo" = private$calculate_monte_carlo_var(portfolio_value, n_simulations)
      )
      
      # Scale VaR to time horizon
      var_value * sqrt(self$time_horizon)
    },
    
    #' @description Calculate Expected Shortfall (Conditional VaR)
    #' @param portfolio_value Current portfolio value
    #' @param method Calculation method ('historical', 'parametric', or 'monte_carlo')
    #' @param n_simulations Number of simulations for Monte Carlo method
    calculate_es = function(portfolio_value, method = "historical", n_simulations = 10000) {
      if (is.null(self$returns)) {
        stop("No returns data available. Please initialize with returns data.")
      }
      
      method <- match.arg(method, c("historical", "parametric", "monte_carlo"))
      
      es_value <- switch(method,
        "historical" = private$calculate_historical_es(portfolio_value),
        "parametric" = private$calculate_parametric_es(portfolio_value),
        "monte_carlo" = private$calculate_monte_carlo_es(portfolio_value, n_simulations)
      )
      
      # Scale ES to time horizon
      es_value * sqrt(self$time_horizon)
    },
    
    #' @description Generate risk report with multiple metrics
    #' @param portfolio_value Current portfolio value
    #' @param include_methods Which methods to include in report
    generate_risk_report = function(portfolio_value, 
                                  include_methods = c("historical", "parametric", "monte_carlo")) {
      results <- list()
      
      for (method in include_methods) {
        results[[method]] <- list(
          var = self$calculate_var(portfolio_value, method),
          es = self$calculate_es(portfolio_value, method)
        )
      }
      
      # Add distribution statistics
      results$statistics <- list(
        mean_return = mean(self$returns),
        volatility = sd(self$returns),
        skewness = private$calculate_skewness(),
        kurtosis = private$calculate_kurtosis()
      )
      
      return(results)
    },
    
    #' @description Update returns data and recalculate distribution parameters
    #' @param new_returns New returns data to use
    update_returns = function(new_returns) {
      self$returns <- new_returns
      private$fit_distribution()
      invisible(self)
    },
    
    # Public fields
    returns = NULL,
    confidence_level = NULL,
    time_horizon = NULL
  ),
  
  private = list(
    # Distribution parameters
    mean_return = NULL,
    volatility = NULL,
    
    #' @description Fit distribution to returns data
    fit_distribution = function() {
      private$mean_return <- mean(self$returns)
      private$volatility <- sd(self$returns)
    },
    
    #' @description Calculate Historical VaR
    calculate_historical_var = function(portfolio_value) {
      sorted_returns <- sort(self$returns)
      index <- floor((1 - self$confidence_level) * length(sorted_returns))
      -sorted_returns[index] * portfolio_value
    },
    
    #' @description Calculate Parametric VaR
    calculate_parametric_var = function(portfolio_value) {
      z_score <- stats::qnorm(self$confidence_level)
      portfolio_value * (z_score * private$volatility - private$mean_return)
    },
    
    #' @description Calculate Monte Carlo VaR
    calculate_monte_carlo_var = function(portfolio_value, n_simulations) {
      simulated_returns <- stats::rnorm(
        n_simulations, 
        mean = private$mean_return, 
        sd = private$volatility
      )
      sorted_returns <- sort(simulated_returns)
      index <- floor((1 - self$confidence_level) * n_simulations)
      -sorted_returns[index] * portfolio_value
    },
    
    #' @description Calculate Historical Expected Shortfall
    calculate_historical_es = function(portfolio_value) {
      sorted_returns <- sort(self$returns)
      var_index <- floor((1 - self$confidence_level) * length(sorted_returns))
      tail_returns <- sorted_returns[1:var_index]
      -mean(tail_returns) * portfolio_value
    },
    
    #' @description Calculate Parametric Expected Shortfall
    calculate_parametric_es = function(portfolio_value) {
      z_score <- stats::qnorm(self$confidence_level)
      phi_z <- stats::dnorm(z_score)
      lambda <- phi_z / (1 - self$confidence_level)
      portfolio_value * (lambda * private$volatility - private$mean_return)
    },
    
    #' @description Calculate Monte Carlo Expected Shortfall
    calculate_monte_carlo_es = function(portfolio_value, n_simulations) {
      simulated_returns <- stats::rnorm(
        n_simulations, 
        mean = private$mean_return, 
        sd = private$volatility
      )
      sorted_returns <- sort(simulated_returns)
      var_index <- floor((1 - self$confidence_level) * n_simulations)
      tail_returns <- sorted_returns[1:var_index]
      -mean(tail_returns) * portfolio_value
    },
    
    #' @description Calculate distribution skewness
    calculate_skewness = function() {
      r <- self$returns
      n <- length(r)
      m3 <- sum((r - mean(r))^3) / n
      s3 <- sd(r)^3
      m3 / s3
    },
    
    #' @description Calculate distribution kurtosis
    calculate_kurtosis = function() {
      r <- self$returns
      n <- length(r)
      m4 <- sum((r - mean(r))^4) / n
      s4 <- sd(r)^4
      m4 / s4 - 3  # Excess kurtosis (normal = 0)
    },
    
    #' @description Validate input parameters
    validate_parameters = function(confidence_level, time_horizon) {
      if (confidence_level <= 0 || confidence_level >= 1) {
        stop("Confidence level must be between 0 and 1")
      }
      if (time_horizon <= 0) {
        stop("Time horizon must be positive")
      }
    }
  )
)

# Demonstration
demonstrate_risk_metrics <- function() {
  cat("=== Value at Risk and Expected Shortfall Demo ===\n\n")
  
  # Generate sample returns data
  set.seed(42)
  n_days <- 1000
  returns <- rnorm(n_days, mean = 0.0001, sd = 0.01)
  
  # Initialize calculator
  risk_calc <- RiskMetrics$new(
    returns = returns,
    confidence_level = 0.95,
    time_horizon = 1
  )
  
  # Portfolio parameters
  portfolio_value <- 1000000  # $1 million portfolio
  
  cat("Portfolio Parameters:\n")
  cat(sprintf("Value: $%d\n", portfolio_value))
  cat(sprintf("Confidence Level: %.1f%%\n", risk_calc$confidence_level * 100))
  cat(sprintf("Time Horizon: %d day(s)\n\n", risk_calc$time_horizon))
  
  # Calculate VaR using different methods
  methods <- c("historical", "parametric", "monte_carlo")
  
  cat("Value at Risk (VaR) Results:\n")
  for (method in methods) {
    var_value <- risk_calc$calculate_var(portfolio_value, method)
    cat(sprintf("%s VaR: $%.2f\n", tools::toTitleCase(method), var_value))
  }
  cat("\n")
  
  cat("Expected Shortfall (ES) Results:\n")
  for (method in methods) {
    es_value <- risk_calc$calculate_es(portfolio_value, method)
    cat(sprintf("%s ES: $%.2f\n", tools::toTitleCase(method), es_value))
  }
  cat("\n")
  
  # Generate and display comprehensive risk report
  cat("Comprehensive Risk Report:\n")
  report <- risk_calc$generate_risk_report(portfolio_value)
  
  cat("\nDistribution Statistics:\n")
  cat(sprintf("Mean Return: %.6f\n", report$statistics$mean_return))
  cat(sprintf("Volatility: %.6f\n", report$statistics$volatility))
  cat(sprintf("Skewness: %.6f\n", report$statistics$skewness))
  cat(sprintf("Excess Kurtosis: %.6f\n", report$statistics$kurtosis))
  
  cat("\n=== Demo Complete ===\n")
}

# Run demonstration if not in interactive mode
if (!interactive()) {
  demonstrate_risk_metrics()
}
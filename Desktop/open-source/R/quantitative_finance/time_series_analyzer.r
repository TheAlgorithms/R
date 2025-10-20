# Time Series Analysis and ARIMA Modeling
# Implements comprehensive time series analysis with ARIMA models
# Features: Stationarity testing, model selection, forecasting, and diagnostics

library(R6)

#' TimeSeriesAnalyzer Class
#' @description R6 class for time series analysis and ARIMA modeling
#' @details Provides functionality for:
#' - Stationarity testing (ADF test)
#' - ACF/PACF analysis
#' - ARIMA model fitting
#' - Model selection using AIC/BIC
#' - Forecasting with confidence intervals
TimeSeriesAnalyzer <- R6Class(
  "TimeSeriesAnalyzer",
  
  public = list(
    #' @description Initialize analyzer with time series data
    #' @param data Time series data (numeric vector)
    #' @param frequency Frequency of the time series (default: 1)
    initialize = function(data = NULL, frequency = 1) {
      if (!is.null(data)) {
        private$validate_input(data)
        self$data <- data
        self$frequency <- frequency
        private$n <- length(data)
      }
      invisible(self)
    },

    #' @description Test for stationarity using Augmented Dickey-Fuller test
    #' @param max_lags Maximum number of lags to consider
    test_stationarity = function(max_lags = NULL) {
      if (is.null(self$data)) {
        stop("No data available. Please initialize with data first.")
      }
      
      if (is.null(max_lags)) {
        max_lags <- floor(sqrt(private$n))
      }
      
      # Perform ADF test
      result <- private$adf_test(max_lags)
      
      # Store results
      private$stationarity_results <- result
      return(result)
    },

    #' @description Calculate ACF and PACF
    #' @param max_lag Maximum lag to consider
    calculate_acf_pacf = function(max_lag = NULL) {
      if (is.null(self$data)) {
        stop("No data available. Please initialize with data first.")
      }
      
      if (is.null(max_lag)) {
        max_lag <- min(private$n - 1, floor(10 * log10(private$n)))
      }
      
      # Calculate ACF
      acf_result <- private$calculate_acf(max_lag)
      
      # Calculate PACF
      pacf_result <- private$calculate_pacf(max_lag)
      
      return(list(
        acf = acf_result,
        pacf = pacf_result,
        lags = 1:max_lag
      ))
    },

    #' @description Fit ARIMA model to the data
    #' @param p AR order
    #' @param d Differencing order
    #' @param q MA order
    fit_arima = function(p = 1, d = 0, q = 1) {
      if (is.null(self$data)) {
        stop("No data available. Please initialize with data first.")
      }
      
      # Validate parameters
      if (any(c(p, d, q) < 0)) {
        stop("ARIMA orders must be non-negative")
      }
      
      # Fit ARIMA model
      model <- private$fit_arima_model(p, d, q)
      private$current_model <- model
      
      return(model)
    },

    #' @description Automatic model selection using AIC
    #' @param max_p Maximum AR order to consider
    #' @param max_d Maximum differencing order
    #' @param max_q Maximum MA order
    select_best_model = function(max_p = 3, max_d = 2, max_q = 3) {
      if (is.null(self$data)) {
        stop("No data available. Please initialize with data first.")
      }
      
      best_aic <- Inf
      best_order <- c(0, 0, 0)
      
      # Grid search over possible orders
      for (p in 0:max_p) {
        for (d in 0:max_d) {
          for (q in 0:max_q) {
            tryCatch({
              model <- self$fit_arima(p, d, q)
              if (model$aic < best_aic) {
                best_aic <- model$aic
                best_order <- c(p, d, q)
                private$best_model <- model
              }
            }, error = function(e) {
              # Skip failed models
            })
          }
        }
      }
      
      return(list(
        order = best_order,
        aic = best_aic
      ))
    },

    #' @description Generate forecasts with confidence intervals
    #' @param h Forecast horizon
    #' @param level Confidence level (0-1)
    forecast = function(h = 10, level = 0.95) {
      if (is.null(private$current_model)) {
        stop("No model fitted. Please fit a model first.")
      }
      
      # Generate forecasts
      forecasts <- private$generate_forecasts(h, level)
      return(forecasts)
    },

    #' @description Perform model diagnostics
    diagnose_model = function() {
      if (is.null(private$current_model)) {
        stop("No model fitted. Please fit a model first.")
      }
      
      residuals <- private$current_model$residuals
      
      # Calculate diagnostic statistics
      diagnostics <- list(
        residual_mean = mean(residuals),
        residual_sd = sd(residuals),
        ljung_box = private$ljung_box_test(residuals),
        normality = private$normality_test(residuals),
        arch_effect = private$arch_test(residuals)
      )
      
      return(diagnostics)
    },

    # Public fields
    data = NULL,
    frequency = NULL
  ),
  
  private = list(
    n = NULL,
    current_model = NULL,
    best_model = NULL,
    stationarity_results = NULL,
    
    validate_input = function(data) {
      if (!is.numeric(data)) {
        stop("Input data must be numeric")
      }
      if (any(is.na(data))) {
        stop("Input data contains missing values")
      }
      if (length(data) < 3) {
        stop("Input data must have at least 3 observations")
      }
    },
    
    adf_test = function(max_lags) {
      y <- self$data
      n <- length(y)
      
      # Calculate first differences
      dy <- diff(y)
      y_1 <- y[-n]
      
      # Construct regression matrix
      X <- matrix(1, n-1, 1)
      X <- cbind(X, y_1)
      
      # Add lagged differences
      if (max_lags > 0) {
        # Add lagged differences using embed() for clarity
        lagged_dy <- embed(dy, max_lags + 1)[, -1, drop = FALSE]
        X <- X[(max_lags+1):nrow(X), ]  # Align X with lagged_dy rows
        X <- cbind(X, lagged_dy)
        dy <- dy[(max_lags+1):length(dy)]
      }
      
      # Remove NA rows
      complete_cases <- stats::complete.cases(X)
      X <- X[complete_cases, ]
      dy <- dy[complete_cases]
      
      # Fit regression
      fit <- stats::lm(dy ~ X - 1)
      
      # Calculate test statistic
      coef <- stats::coef(fit)[2]
      se <- sqrt(diag(stats::vcov(fit)))[2]
      t_stat <- coef / se
      
      # Critical values (approximate)
      crit_values <- c(
        "1%" = -3.43,
        "5%" = -2.86,
        "10%" = -2.57
      )
      
      return(list(
        statistic = t_stat,
        critical_values = crit_values,
        is_stationary = t_stat < -2.86  # 5% level
      ))
    },
    
    calculate_acf = function(max_lag) {
      y <- scale(self$data)  # Standardize data
      n <- length(y)
      acf <- numeric(max_lag)
      
      for (k in 1:max_lag) {
        acf[k] <- stats::cor(y[1:(n-k)], y[(k+1):n])
      }
      
      return(acf)
    },
    
    calculate_pacf = function(max_lag) {
      acf <- private$calculate_acf(max_lag)
      pacf <- numeric(max_lag)
      
      # Durbin-Levinson algorithm
      for (k in 1:max_lag) {
        if (k == 1) {
          pacf[k] <- acf[1]
        } else {
          r <- matrix(0, k, k)
          for (i in 1:k) {
            for (j in 1:k) {
              idx <- abs(i-j)+1
              if (idx <= length(acf)) {
                r[i,j] <- acf[idx]
              } else {
                r[i,j] <- 0
              }
            }
          }
          b <- acf[1:k]
          pacf[k] <- solve(r, b)[k]
        }
      }
      
      return(pacf)
    },
    
    fit_arima_model = function(p, d, q) {
      # Difference data
      y <- self$data
      for (i in 1:d) {
        y <- diff(y)
      }
      
      # Construct and solve Yule-Walker equations for AR part
      if (p > 0) {
        r <- private$calculate_acf(p + 1)
        phi <- solve(stats::toeplitz(r[1:p]), r[2:(p+1)])
      } else {
        phi <- numeric(0)
      }
      
      # Estimate MA parameters using innovation algorithm
      if (q > 0) {
        theta <- numeric(q)
        e <- y
        for (i in 1:10) {  # Iterate to improve estimates
          r <- private$calculate_acf(q)
          psi <- numeric(q)
          for (j in 1:q) {
            if (j > 1) {
              psi[j] <- sum(theta[1:(j-1)] * rev(psi[1:(j-1)])) + theta[j]
            } else {
              psi[j] <- theta[j]
            }
          }
          if (q == 1) {
            theta <- solve(stats::toeplitz(1), r[1])
          } else {
            theta <- solve(stats::toeplitz(c(1, psi[1:(q-1)])), r[1:q])
          }
        }
      } else {
        theta <- numeric(0)
      }
      
      # Calculate residuals and AIC
      resid <- private$calculate_residuals(y, phi, theta)
      n_params <- p + q
      aic <- length(resid) * log(var(resid)) + 2 * n_params
      
      return(list(
        coefficients = list(ar = phi, ma = theta),
        residuals = resid,
        aic = aic,
        order = c(p, d, q)
      ))
    },
    
    calculate_residuals = function(y, phi, theta) {
      n <- length(y)
      p <- length(phi)
      q <- length(theta)
      resid <- numeric(n)
      
      for (t in (max(p,q)+1):n) {
        pred <- 0
        if (p > 0) {
          pred <- pred + sum(phi * y[(t-1):(t-p)])
        }
        if (q > 0) {
          pred <- pred + sum(theta * resid[(t-1):(t-q)])
        }
        resid[t] <- y[t] - pred
      }
      
      return(resid[(max(p,q)+1):n])
    },
    
    generate_forecasts = function(h, level) {
      model <- private$current_model
      y <- self$data
      n <- length(y)
      
      # Get model orders
      p <- length(model$coefficients$ar)
      d <- model$order[2]
      q <- length(model$coefficients$ma)
      
      # Generate point forecasts
      forecasts <- numeric(h)
      for (i in 1:h) {
        pred <- 0
        if (p > 0) {
          # Use the most recent p values (from original data and previous forecasts)
          if (i <= p) {
            ar_terms <- y[(n-p+i):(n+i-1)]
          } else {
            # Combine tail of y and head of forecasts as needed
            num_from_y <- max(0, p - (i-1))
            num_from_forecasts <- p - num_from_y
            if (num_from_y > 0) {
              ar_terms <- c(
                y[(n - p + i):(n)],
                forecasts[1:num_from_forecasts]
              )
            } else {
              ar_terms <- forecasts[(i-p):(i-1)]
            }
          }
          pred <- pred + sum(model$coefficients$ar * ar_terms)
        }
        forecasts[i] <- pred
      }
      
      # Calculate prediction intervals
      sigma <- sd(model$residuals)
      z <- stats::qnorm((1 + level) / 2)
      se <- sigma * sqrt(cumsum(rep(1, h)))
      lower <- forecasts - z * se
      upper <- forecasts + z * se
      
      return(list(
        mean = forecasts,
        lower = lower,
        upper = upper,
        level = level
      ))
    },
    
    ljung_box_test = function(residuals) {
      max_lag <- min(20, length(residuals) - 1)
      acf_vals <- stats::acf(residuals, plot = FALSE, lag.max = max_lag)$acf[-1]
      n <- length(residuals)
      
      Q <- n * (n + 2) * sum((acf_vals^2) / (n - 1:max_lag))
      p_value <- 1 - stats::pchisq(Q, max_lag)
      
      return(list(
        statistic = Q,
        p_value = p_value
      ))
    },
    
    normality_test = function(residuals) {
      # Jarque-Bera test
      n <- length(residuals)
      s <- sum((residuals - mean(residuals))^3) / (n * sd(residuals)^3)  # skewness
      k <- sum((residuals - mean(residuals))^4) / (n * sd(residuals)^4) - 3  # excess kurtosis
      JB <- n * (s^2/6 + k^2/24)
      p_value <- 1 - stats::pchisq(JB, df = 2)
      
      return(list(
        statistic = JB,
        p_value = p_value
      ))
    },
    
    arch_test = function(residuals) {
      # ARCH LM test
      sq_resid <- residuals^2
      n <- length(sq_resid)
      lags <- 5
      
      # Use embed to construct lagged matrix (current and past lags)
      # embed(sq_resid, lags + 1) returns a matrix with columns: t, t-1, ..., t-lags
      if (n <= lags) {
        stop("Not enough observations for ARCH test lags.")
      }
      lagged_mat <- stats::embed(sq_resid, lags + 1)
      y <- lagged_mat[, 1]  # current squared residuals
      X <- lagged_mat[, -1] # lagged squared residuals
      fit <- stats::lm(y ~ X)
      R2 <- summary(fit)$r.squared
      LM <- nrow(lagged_mat) * R2
      p_value <- 1 - stats::pchisq(LM, lags)
      
      return(list(
        statistic = LM,
        p_value = p_value
      ))
    }
  )
)

# Demonstration
demonstrate_time_series_analysis <- function() {
  cat("=== Time Series Analysis Demo ===\n\n")
  
  # Generate sample time series
  set.seed(42)
  n <- 500
  
  # AR(1) process with trend and seasonality
  t <- 1:n
  trend <- 0.01 * t
  seasonal <- 2 * sin(2 * pi * t / 12)
  ar_process <- stats::arima.sim(list(ar = 0.7), n = n)
  y <- trend + seasonal + ar_process
  
  # Initialize analyzer
  ts_analyzer <- TimeSeriesAnalyzer$new(y, frequency = 12)
  
  # Test stationarity
  cat("Testing for stationarity...\n")
  stat_test <- ts_analyzer$test_stationarity()
  cat(sprintf("ADF test statistic: %.3f\n", stat_test$statistic))
  cat("Critical values:\n")
  print(stat_test$critical_values)
  cat(sprintf("Series is %sstationary at 5%% level\n\n",
              ifelse(stat_test$is_stationary, "", "non-")))
  
  # Analyze ACF/PACF
  cat("Calculating ACF and PACF...\n")
  corr <- ts_analyzer$calculate_acf_pacf(20)
  cat("First 5 lags:\n")
  cat("ACF:  ")
  cat(sprintf("%.3f ", corr$acf[1:5]))
  cat("\nPACF: ")
  cat(sprintf("%.3f ", corr$pacf[1:5]))
  cat("\n\n")
  
  # Select best model
  cat("Selecting best ARIMA model...\n")
  best <- ts_analyzer$select_best_model(max_p = 2, max_d = 1, max_q = 2)
  cat(sprintf("Best model: ARIMA(%d,%d,%d)\n", 
              best$order[1], best$order[2], best$order[3]))
  cat(sprintf("AIC: %.2f\n\n", best$aic))
  
  # Generate forecasts
  cat("Generating forecasts...\n")
  h <- 12  # Forecast horizon
  forecasts <- ts_analyzer$forecast(h = h, level = 0.95)
  
  cat("Point forecasts for next 12 periods:\n")
  cat(sprintf("%.2f ", forecasts$mean))
  cat("\n\n")
  
  # Model diagnostics
  cat("Performing model diagnostics...\n")
  diagnostics <- ts_analyzer$diagnose_model()
  
  cat("Residual diagnostics:\n")
  cat(sprintf("Mean: %.3f\n", diagnostics$residual_mean))
  cat(sprintf("Standard deviation: %.3f\n", diagnostics$residual_sd))
  cat(sprintf("Ljung-Box test p-value: %.3f\n", diagnostics$ljung_box$p_value))
  cat(sprintf("Normality test p-value: %.3f\n", diagnostics$normality$p_value))
  cat(sprintf("ARCH test p-value: %.3f\n", diagnostics$arch_effect$p_value))
  
  cat("\n=== Demo Complete ===\n")
}

# Run demonstration if not in interactive mode
if (!interactive()) {
  demonstrate_time_series_analysis()
}
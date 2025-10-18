# hidden_markov_model.r
# Hidden Markov Model for Financial Regime Detection
# Detects market regimes (bull/bear/sideways) using HMM
# Implements Baum-Welch (EM) and Viterbi algorithms

library(R6)

HMM <- R6Class("HMM",
  public = list(
    n_states = NULL,
    n_iter = NULL,
    tol = NULL,
    random_state = NULL,
    transition_matrix = NULL,
    emission_means = NULL,
    emission_vars = NULL,
    initial_probs = NULL,
    states = NULL,
    log_likelihood = NULL,
    
    initialize = function(n_states = 3, n_iter = 100, tol = 1e-4, random_state = NULL) {
      if (!is.null(random_state)) set.seed(random_state)
      self$n_states <- n_states
      self$n_iter <- n_iter
      self$tol <- tol
      self$random_state <- random_state
    },
    
    fit = function(returns) {
      n <- length(returns)
      
      # Initialize parameters
      private$initialize_parameters(returns)
      
      prev_ll <- -Inf
      
      for (iter in 1:self$n_iter) {
        # E-step: Forward-Backward algorithm
        fb_result <- private$forward_backward(returns)
        alpha <- fb_result$alpha
        beta <- fb_result$beta
        ll <- fb_result$log_likelihood
        
        # Check convergence
        if (abs(ll - prev_ll) < self$tol) {
          cat(sprintf("Converged at iteration %d\n", iter))
          break
        }
        prev_ll <- ll
        
        # M-step: Update parameters
        private$update_parameters(returns, alpha, beta)
        
        if (iter %% 10 == 0) {
          cat(sprintf("Iteration %d: Log-Likelihood = %.4f\n", iter, ll))
        }
      }
      
      self$log_likelihood <- ll
      
      # Decode most likely state sequence using Viterbi
      self$states <- private$viterbi(returns)
      
      invisible(self)
    },
    
    predict = function(returns) {
      if (is.null(self$transition_matrix)) {
        stop("Model must be fitted before prediction")
      }
      return(private$viterbi(returns))
    },
    
    predict_proba = function(returns) {
      if (is.null(self$transition_matrix)) {
        stop("Model must be fitted before prediction")
      }
      fb_result <- private$forward_backward(returns)
      gamma <- private$compute_gamma(fb_result$alpha, fb_result$beta)
      return(gamma)
    },
    
    get_regime_labels = function() {
      regime_names <- character(self$n_states)
      sorted_means <- sort(self$emission_means, index.return = TRUE)
      
      if (self$n_states == 2) {
        regime_names[sorted_means$ix[1]] <- "Bear"
        regime_names[sorted_means$ix[2]] <- "Bull"
      } else if (self$n_states == 3) {
        regime_names[sorted_means$ix[1]] <- "Bear"
        regime_names[sorted_means$ix[2]] <- "Sideways"
        regime_names[sorted_means$ix[3]] <- "Bull"
      } else {
        for (i in 1:self$n_states) {
          regime_names[sorted_means$ix[i]] <- paste0("Regime_", i)
        }
      }
      
      names(regime_names) <- 1:self$n_states
      return(regime_names)
    },
    
    get_expected_durations = function() {
      durations <- numeric(self$n_states)
      for (i in 1:self$n_states) {
        durations[i] <- 1 / (1 - self$transition_matrix[i, i])
      }
      names(durations) <- self$get_regime_labels()
      return(durations)
    },
    
    get_steady_state = function() {
      eigenvalues <- eigen(t(self$transition_matrix))
      steady_state <- Re(eigenvalues$vectors[, 1])
      steady_state <- steady_state / sum(steady_state)
      names(steady_state) <- self$get_regime_labels()
      return(steady_state)
    },
    
    print = function() {
      cat("Hidden Markov Model\n")
      cat("===================\n\n")
      cat(sprintf("Number of States: %d\n", self$n_states))
      
      if (!is.null(self$log_likelihood)) {
        cat(sprintf("Log-Likelihood: %.4f\n\n", self$log_likelihood))
        
        regime_labels <- self$get_regime_labels()
        
        cat("Emission Parameters:\n")
        for (i in 1:self$n_states) {
          cat(sprintf("  %s: Mean=%.6f, Std=%.6f\n", 
                      regime_labels[i], 
                      self$emission_means[i], 
                      sqrt(self$emission_vars[i])))
        }
        
        cat("\nTransition Matrix:\n")
        trans_df <- as.data.frame(self$transition_matrix)
        colnames(trans_df) <- regime_labels
        rownames(trans_df) <- regime_labels
        print(round(trans_df, 4))
        
        cat("\nExpected Durations (periods):\n")
        durations <- self$get_expected_durations()
        print(round(durations, 2))
        
        cat("\nSteady-State Probabilities:\n")
        steady <- self$get_steady_state()
        print(round(steady, 4))
      } else {
        cat("Model not fitted yet.\n")
      }
      
      invisible(self)
    }
  ),
  
  private = list(
    initialize_parameters = function(returns) {
      # K-means initialization for emission parameters
      kmeans_result <- kmeans(returns, centers = self$n_states, nstart = 10)
      
      self$emission_means <- as.numeric(kmeans_result$centers)
      self$emission_vars <- tapply(returns, kmeans_result$cluster, var)
      
      # Ensure positive variance
      self$emission_vars[self$emission_vars <= 0] <- 1e-6
      
      # Initialize transition matrix with persistence
      self$transition_matrix <- matrix(0.1 / (self$n_states - 1), 
                                       nrow = self$n_states, 
                                       ncol = self$n_states)
      diag(self$transition_matrix) <- 0.9
      
      # Uniform initial probabilities
      self$initial_probs <- rep(1 / self$n_states, self$n_states)
    },
    
    emission_prob = function(x, state) {
      mean_val <- self$emission_means[state]
      var_val <- self$emission_vars[state]
      return(dnorm(x, mean = mean_val, sd = sqrt(var_val)))
    },
    
    forward_backward = function(returns) {
      n <- length(returns)
      alpha <- matrix(0, nrow = n, ncol = self$n_states)
      beta <- matrix(0, nrow = n, ncol = self$n_states)
      scaling <- numeric(n)
      
      # Forward pass
      for (i in 1:self$n_states) {
        alpha[1, i] <- self$initial_probs[i] * private$emission_prob(returns[1], i)
      }
      scaling[1] <- sum(alpha[1, ])
      if (scaling[1] > 0) {
        alpha[1, ] <- alpha[1, ] / scaling[1]
      }
      
      for (t in 2:n) {
        for (j in 1:self$n_states) {
          alpha[t, j] <- sum(alpha[t-1, ] * self$transition_matrix[, j]) * 
                         private$emission_prob(returns[t], j)
        }
        scaling[t] <- sum(alpha[t, ])
        if (scaling[t] > 0) {
          alpha[t, ] <- alpha[t, ] / scaling[t]
        }
      }
      
      # Backward pass
      beta[n, ] <- 1
      
      for (t in (n-1):1) {
        for (i in 1:self$n_states) {
          beta[t, i] <- sum(self$transition_matrix[i, ] * 
                           sapply(1:self$n_states, function(j) {
                             private$emission_prob(returns[t+1], j)
                           }) * beta[t+1, ])
        }
        if (scaling[t+1] > 0) {
          beta[t, ] <- beta[t, ] / scaling[t+1]
        }
      }
      
      log_likelihood <- sum(log(scaling[scaling > 0]))
      
      return(list(alpha = alpha, beta = beta, log_likelihood = log_likelihood))
    },
    
    compute_gamma = function(alpha, beta) {
      gamma <- alpha * beta
      row_sums <- rowSums(gamma)
      gamma <- gamma / matrix(row_sums, nrow = nrow(gamma), ncol = ncol(gamma))
      return(gamma)
    },
    
    compute_xi = function(returns, alpha, beta) {
      n <- length(returns)
      xi <- array(0, dim = c(n-1, self$n_states, self$n_states))
      
      for (t in 1:(n-1)) {
        denominator <- 0
        for (i in 1:self$n_states) {
          for (j in 1:self$n_states) {
            xi[t, i, j] <- alpha[t, i] * self$transition_matrix[i, j] * 
                          private$emission_prob(returns[t+1], j) * beta[t+1, j]
            denominator <- denominator + xi[t, i, j]
          }
        }
        if (denominator > 0) {
          xi[t, , ] <- xi[t, , ] / denominator
        }
      }
      
      return(xi)
    },
    
    update_parameters = function(returns, alpha, beta) {
      n <- length(returns)
      gamma <- private$compute_gamma(alpha, beta)
      xi <- private$compute_xi(returns, alpha, beta)
      
      # Update initial probabilities
      self$initial_probs <- gamma[1, ]
      
      # Update transition matrix
      for (i in 1:self$n_states) {
        denom <- sum(gamma[1:(n-1), i])
        if (denom > 0) {
          for (j in 1:self$n_states) {
            self$transition_matrix[i, j] <- sum(xi[, i, j]) / denom
          }
        }
      }
      
      # Normalize transition matrix rows
      for (i in 1:self$n_states) {
        row_sum <- sum(self$transition_matrix[i, ])
        if (row_sum > 0) {
          self$transition_matrix[i, ] <- self$transition_matrix[i, ] / row_sum
        }
      }
      
      # Update emission parameters
      for (i in 1:self$n_states) {
        gamma_sum <- sum(gamma[, i])
        if (gamma_sum > 0) {
          self$emission_means[i] <- sum(gamma[, i] * returns) / gamma_sum
          self$emission_vars[i] <- sum(gamma[, i] * (returns - self$emission_means[i])^2) / gamma_sum
          
          # Ensure positive variance
          if (self$emission_vars[i] <= 0) {
            self$emission_vars[i] <- 1e-6
          }
        }
      }
    },
    
    viterbi = function(returns) {
      n <- length(returns)
      delta <- matrix(-Inf, nrow = n, ncol = self$n_states)
      psi <- matrix(0, nrow = n, ncol = self$n_states)
      
      # Initialization
      for (i in 1:self$n_states) {
        emission <- private$emission_prob(returns[1], i)
        if (emission > 0) {
          delta[1, i] <- log(self$initial_probs[i]) + log(emission)
        }
      }
      
      # Recursion
      for (t in 2:n) {
        for (j in 1:self$n_states) {
          temp <- delta[t-1, ] + log(self$transition_matrix[, j] + 1e-10)
          psi[t, j] <- which.max(temp)
          emission <- private$emission_prob(returns[t], j)
          if (emission > 0) {
            delta[t, j] <- max(temp) + log(emission)
          }
        }
      }
      
      # Backtracking
      states <- integer(n)
      states[n] <- which.max(delta[n, ])
      
      for (t in (n-1):1) {
        states[t] <- psi[t+1, states[t+1]]
      }
      
      return(states)
    }
  )
)

# Example 1: Simulated regime data
if (FALSE) {
  cat("\n=== Example 1: Basic Regime Detection ===\n\n")
  
  set.seed(42)
  
  # Simulate 3 regimes
  bear_returns <- rnorm(150, mean = -0.002, sd = 0.02)
  sideways_returns <- rnorm(200, mean = 0.0005, sd = 0.01)
  bull_returns <- rnorm(150, mean = 0.003, sd = 0.015)
  returns <- c(bear_returns, sideways_returns, bull_returns)
  
  # Fit HMM
  model <- HMM$new(n_states = 3, n_iter = 100, random_state = 42)
  model$fit(returns)
  model$print()
  
  # Plot results
  plot(returns, type = "l", col = "gray", lwd = 1,
       main = "Returns with Detected Regimes",
       xlab = "Time", ylab = "Returns")
  
  regime_labels <- model$get_regime_labels()
  colors <- c("red", "orange", "green")
  names(colors) <- c("Bear", "Sideways", "Bull")
  
  for (i in 1:length(returns)) {
    state_label <- regime_labels[model$states[i]]
    points(i, returns[i], col = colors[state_label], pch = 16, cex = 0.5)
  }
  
  legend("topright", legend = names(colors), col = colors, pch = 16)
  
  # Example 2: Two-state model
  cat("\n=== Example 2: Bull/Bear Detection ===\n\n")
  
  set.seed(123)
  stock_returns <- rnorm(252, mean = 0.0005, sd = 0.015)
  
  model2 <- HMM$new(n_states = 2, random_state = 123)
  model2$fit(stock_returns)
  model2$print()
  
  predicted_states <- model2$predict(stock_returns)
  regime_labels <- model2$get_regime_labels()
  
  cat("\nRegime Distribution:\n")
  cat(sprintf("Bull: %d days (%.1f%%)\n", 
              sum(regime_labels[predicted_states] == "Bull"),
              100 * mean(regime_labels[predicted_states] == "Bull")))
  cat(sprintf("Bear: %d days (%.1f%%)\n", 
              sum(regime_labels[predicted_states] == "Bear"),
              100 * mean(regime_labels[predicted_states] == "Bear")))
  
  # Example 3: Regime probabilities
  cat("\n=== Example 3: Regime Probability Evolution ===\n\n")
  
  regime_probs <- model$predict_proba(returns)
  
  par(mfrow = c(1, 1))
  plot(regime_probs[, 1], type = "l", col = "red", ylim = c(0, 1), lwd = 2,
       main = "Regime Probabilities Over Time",
       xlab = "Time", ylab = "Probability")
  lines(regime_probs[, 2], col = "orange", lwd = 2)
  lines(regime_probs[, 3], col = "green", lwd = 2)
  legend("topright", legend = c("Bear", "Sideways", "Bull"),
         col = c("red", "orange", "green"), lty = 1, lwd = 2)
  
  # Example 4: Trading signals
  cat("\n=== Example 4: Trading Signals ===\n\n")
  
  regime_labels_seq <- regime_labels[model$states]
  
  # Generate signals based on regime changes
  signals <- rep("HOLD", length(returns))
  for (i in 2:length(returns)) {
    if (regime_labels_seq[i] == "Bull" && regime_labels_seq[i-1] != "Bull") {
      signals[i] <- "BUY"
    } else if (regime_labels_seq[i] == "Bear" && regime_labels_seq[i-1] != "Bear") {
      signals[i] <- "SELL"
    }
  }
  
  cat(sprintf("Buy signals: %d\n", sum(signals == "BUY")))
  cat(sprintf("Sell signals: %d\n", sum(signals == "SELL")))
  
  # Plot with signals
  plot(returns, type = "l", col = "gray", main = "Returns with Trading Signals",
       xlab = "Time", ylab = "Returns")
  points(which(signals == "BUY"), returns[signals == "BUY"], 
         col = "green", pch = 24, cex = 1.5, bg = "green")
  points(which(signals == "SELL"), returns[signals == "SELL"], 
         col = "red", pch = 25, cex = 1.5, bg = "red")
  legend("topright", legend = c("Buy", "Sell"), 
         col = c("green", "red"), pch = c(24, 25), pt.bg = c("green", "red"))
}

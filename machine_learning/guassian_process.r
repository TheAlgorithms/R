# ==============================================
# Gaussian Process Regression (GP)
# ==============================================
# Algorithm: Non-parametric Bayesian regression using Gaussian Processes.
# Framework: R (kernlab package)
#
# Purpose:
# - Perform regression while providing uncertainty estimates.
# - Useful for small datasets and Bayesian optimization.
#
# Core Idea:
# - Define a prior over functions using a kernel (covariance) function.
# - Update the posterior distribution using observed data.
# - Predictions include mean and variance (uncertainty) for each point.
#
# Complexity:
# - Time:  O(n^3) due to inversion of the kernel matrix
# - Space: O(n^2) for storing the kernel matrix
#
# Edge Cases / Notes:
# - Choice of kernel is critical for good performance.
# - Computationally heavy for large datasets; sparse approximations exist.
# - Great for uncertainty quantification in predictions.
#
# Typical Applications:
# - Bayesian optimization
# - Small-data regression tasks
# - Time-series forecasting with uncertainty estimates
#
# Reference:
# Rasmussen, C. E., & Williams, C. K. I. (2006). Gaussian Processes for Machine Learning.
# ==============================================

# Load required library
suppressPackageStartupMessages(library(kernlab))

# ---- Core Functions ----

#' Train a Gaussian Process Regression model
#' @param x Numeric vector or matrix of input features
#' @param y Numeric vector of target values
#' @param kernel Kernel to use (default: "rbfdot")
#' @param ... Additional arguments passed to gausspr
#' @return Trained GP model (kernlab::gausspr object)
gp_train <- function(x, y, kernel = "rbfdot", ...) {
  gausspr(
    x = as.matrix(x), y = y,
    kernel = kernel,
    ...
  )
}

#' Predict using a trained Gaussian Process Regression model
#' @param model Trained GP model (from gp_train)
#' @param x_test Numeric vector or matrix of test inputs
#' @param type Prediction type (default: "response")
#' @param ... Additional arguments passed to predict
#' @return Predicted values
gp_predict <- function(model, x_test, type = "response", ...) {
  predict(model, as.matrix(x_test), type = type, ...)
}

# ---- Example Usage (runs only in interactive sessions) ----
if (interactive()) {
  # Example Dataset (Synthetic)
  set.seed(42)
  x <- seq(-5, 5, length.out = 50)
  y <- sin(x) + rnorm(length(x), sd = 0.2)

  # Train GP model
  gp_model <- gp_train(x, y)

  # Make Predictions
  x_test <- seq(-6, 6, length.out = 100)
  y_pred <- gp_predict(gp_model, x_test)

  # Plot Results
  plot(x, y, main = "Gaussian Process Regression", xlab = "X", ylab = "Y", pch = 19)
  lines(x_test, y_pred, col = "blue", lwd = 2)
  legend("topright", legend = c("Observations", "GP Prediction"), col = c("black", "blue"), pch = c(19, NA), lty = c(NA, 1))
}
# ==============================================
# Note:
# - This script defines a Gaussian Process Regression model in R.
# - Can be applied to other regression datasets by replacing x and y.
# - For large datasets, consider sparse GP approximations.
# ==============================================

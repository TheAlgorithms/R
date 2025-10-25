# LSTM Time Series Prediction in R
# Long Short-Term Memory (LSTM) Neural Network for Time Series Forecasting
#
# Required libraries: keras, tensorflow, (optional) tidyr or reshape2, ggplot2
# Install with: install.packages("keras"); install.packages("tensorflow")
# Optionally: install.packages(c("tidyr","reshape2","ggplot2"))
# Then run: keras::install_keras()

suppressPackageStartupMessages({
  library(keras)
  library(tensorflow)
})

#' Create sequences for LSTM training
#' @param data: Numeric vector or matrix of time series data
#' @param seq_length: Length of input sequences
#' @return: List containing X (input sequences) and y (target values)
create_sequences <- function(data, seq_length) {
  n <- length(data)
  
  # Initialize lists to store sequences
  X <- list()
  y <- list()
  
  # Create sequences
  for (i in 1:(n - seq_length)) {
    X[[i]] <- data[i:(i + seq_length - 1)]
    y[[i]] <- data[i + seq_length]
  }
  
  # Convert lists to arrays
  X <- array(unlist(X), dim = c(length(X), seq_length, 1))
  y <- array(unlist(y), dim = c(length(y), 1))
  
  return(list(X = X, y = y))
}

#' Normalize data to [0, 1] range
#' @param data: Numeric vector
#' @return: List with normalized data, min, and max values
normalize_data <- function(data) {
  min_val <- min(data)
  max_val <- max(data)
  normalized <- (data - min_val) / (max_val - min_val)
  
  return(list(
    data = normalized,
    min = min_val,
    max = max_val
  ))
}

#' Inverse normalize data back to original scale
#' @param data: Normalized data
#' @param min_val: Original minimum value
#' @param max_val: Original maximum value
#' @return: Data in original scale
denormalize_data <- function(data, min_val, max_val) {
  return(data * (max_val - min_val) + min_val)
}

#' Build LSTM model for time series prediction
#' @param seq_length: Length of input sequences
#' @param lstm_units: Number of LSTM units (neurons)
#' @param dropout_rate: Dropout rate for regularization (0 to 1)
#' @param learning_rate: Learning rate for optimizer
#' @return: Compiled Keras model
build_lstm_model <- function(seq_length, lstm_units = 50, 
                             dropout_rate = 0.2, learning_rate = 0.001) {
  
  model <- keras_model_sequential() %>%
    layer_lstm(units = lstm_units, 
               activation = 'tanh',
               input_shape = c(seq_length, 1),
               return_sequences = FALSE) %>%
    layer_dropout(rate = dropout_rate) %>%
    layer_dense(units = 1)  # Output layer for regression
  
  # Compile model
  model %>% compile(
    optimizer = optimizer_adam(learning_rate = learning_rate),
    loss = 'mean_squared_error',
    metrics = c('mae')
  )
  
  return(model)
}

#' Calculate evaluation metrics
#' @param actual: Actual values
#' @param predicted: Predicted values
#' @return: List of evaluation metrics
calculate_metrics <- function(actual, predicted) {
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  mae <- mean(abs(actual - predicted))
  
  # R-squared
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r_squared <- 1 - (ss_res / ss_tot)
  
  return(list(
    MSE = mse,
    RMSE = rmse,
    MAE = mae,
    R_squared = r_squared
  ))
}

# ========== Main Example: Sine Wave Prediction ==========

cat("========== LSTM Time Series Prediction Example ==========\n\n")
cat("Generating synthetic sine wave data...\n")

# Set random seed for reproducibility
set.seed(42)
tensorflow::tf$random$set_seed(42)

# Generate sine wave data (500 points)
time_points <- seq(0, 20, length.out = 500)
data <- sin(time_points)

cat(sprintf("Generated %d data points\n\n", length(data)))

# Normalize data
cat("Normalizing data to [0, 1] range...\n")
normalized <- normalize_data(data)
data_norm <- normalized$data

# Create sequences for LSTM
seq_length <- 20
cat(sprintf("Creating sequences with length: %d\n", seq_length))
sequences <- create_sequences(data_norm, seq_length)

X <- sequences$X
y <- sequences$y

cat(sprintf("Total sequences created: %d\n\n", dim(X)[1]))

# Split into train and test sets (80-20 split)
train_size <- floor(0.8 * dim(X)[1])

# IMPORTANT: For time series, use sequential split (preserve temporal order)
train_indices <- 1:train_size
test_indices <- (train_size + 1):dim(X)[1]

X_train <- X[train_indices, , , drop = FALSE]
y_train <- y[train_indices, , drop = FALSE]
X_test <- X[test_indices, , , drop = FALSE]
y_test <- y[test_indices, , drop = FALSE]

cat(sprintf("Training samples: %d\n", dim(X_train)[1]))
cat(sprintf("Test samples: %d\n\n", dim(X_test)[1]))

# Build LSTM model
cat("Building LSTM model...\n")
model <- build_lstm_model(
  seq_length = seq_length,
  lstm_units = 50,
  dropout_rate = 0.2,
  learning_rate = 0.001
)

cat("\nModel Architecture:\n")
print(summary(model))

# Train the model
cat("\n========== Training LSTM Model ==========\n\n")

history <- model %>% fit(
  X_train, y_train,
  epochs = 50,
  batch_size = 16,
  validation_split = 0.1,
  verbose = 1
)

# Plot training history
if (requireNamespace("ggplot2", quietly = TRUE)) {
  cat("\nPlotting training history...\n")
  plot(history)
}

# Evaluate on test data
cat("\n========== Model Evaluation ==========\n\n")
evaluation <- model %>% evaluate(X_test, y_test, verbose = 0)
cat(sprintf("Test Loss (MSE): %.6f\n", evaluation[[1]]))
cat(sprintf("Test MAE: %.6f\n\n", evaluation[[2]]))

# Make predictions
cat("Making predictions on test set...\n")
y_pred <- model %>% predict(X_test, verbose = 0)

# Denormalize predictions and actual values
y_test_orig <- denormalize_data(y_test, normalized$min, normalized$max)
y_pred_orig <- denormalize_data(y_pred, normalized$min, normalized$max)

# Calculate metrics on original scale
metrics <- calculate_metrics(y_test_orig, y_pred_orig)

cat("\n========== Performance Metrics (Original Scale) ==========\n\n")
cat(sprintf("Mean Squared Error (MSE): %.6f\n", metrics$MSE))
cat(sprintf("Root Mean Squared Error (RMSE): %.6f\n", metrics$RMSE))
cat(sprintf("Mean Absolute Error (MAE): %.6f\n", metrics$MAE))
cat(sprintf("R-squared: %.6f\n\n", metrics$R_squared))

# Display sample predictions
cat("========== Sample Predictions ==========\n\n")
cat(sprintf("%-15s %-15s %-15s\n", "Actual", "Predicted", "Error"))
cat(strrep("-", 50), "\n")

n_samples <- min(10, length(y_test_orig))
for (i in 1:n_samples) {
  error <- abs(y_test_orig[i] - y_pred_orig[i])
  cat(sprintf("%-15.6f %-15.6f %-15.6f\n", 
              y_test_orig[i], y_pred_orig[i], error))
}

# ========== Visualization ==========

if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  
  cat("\n========== Creating Prediction Plot ==========\n\n")
  
  # Create dataframe for plotting
  plot_data <- data.frame(
    Index = 1:length(y_test_orig),
    Actual = as.vector(y_test_orig),
    Predicted = as.vector(y_pred_orig)
  )
  
  # Reshape for ggplot: prefer tidyr::pivot_longer if available, fallback to reshape2::melt
  if (requireNamespace("tidyr", quietly = TRUE)) {
    plot_data_long <- tidyr::pivot_longer(plot_data, 
                                          cols = -Index,
                                          names_to = "variable",
                                          values_to = "value")
  } else if (requireNamespace("reshape2", quietly = TRUE)) {
    plot_data_long <- reshape2::melt(plot_data, id.vars = "Index")
    # Ensure consistent column names with pivot_longer
    names(plot_data_long) <- c("Index", "variable", "value")
  } else {
    stop("Please install 'tidyr' or 'reshape2' to create the plot (install.packages('tidyr')).")
  }
  
  # Create plot (use linewidth instead of size for modern ggplot2)
  p <- ggplot(plot_data_long, aes(x = Index, y = value, color = variable)) +
    geom_line(linewidth = 1) +
    geom_point(alpha = 0.5) +
    scale_color_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
    labs(
      title = "LSTM Time Series Predictions",
      subtitle = sprintf("Test Set: %d samples (RMSE: %.4f)", 
                        length(y_test_orig), metrics$RMSE),
      x = "Sample Index",
      y = "Value",
      color = "Series"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "bottom"
    )
  
  print(p)
  
  cat("Plot created successfully!\n\n")
}

# ========== Additional Example: Multi-step Prediction ==========

cat("========== Multi-Step Ahead Prediction ==========\n\n")

#' Make multi-step predictions
#' @param model: Trained LSTM model
#' @param initial_seq: Initial sequence to start prediction
#' @param n_steps: Number of steps to predict ahead
#' @param min_val: Min value for denormalization
#' @param max_val: Max value for denormalization
#' @return: Vector of predictions
predict_multi_step <- function(model, initial_seq, n_steps, min_val, max_val) {
  predictions <- numeric(n_steps)
  current_seq <- initial_seq
  
  for (i in 1:n_steps) {
    # Predict next value
    pred <- model %>% predict(current_seq, verbose = 0)
    predictions[i] <- denormalize_data(pred, min_val, max_val)
    
    # Update sequence: remove first value, add prediction
    current_seq <- array(
      c(current_seq[1, 2:seq_length, 1], pred),
      dim = c(1, seq_length, 1)
    )
  }
  
  return(predictions)
}

# Use first test sequence for multi-step prediction
initial_sequence <- X_test[1, , , drop = FALSE]
n_future_steps <- 20

cat(sprintf("Predicting %d steps ahead...\n", n_future_steps))
future_predictions <- predict_multi_step(
  model, 
  initial_sequence, 
  n_future_steps,
  normalized$min,
  normalized$max
)

cat("\nMulti-step predictions:\n")
for (i in 1:min(10, n_future_steps)) {
  cat(sprintf("Step %2d: %.6f\n", i, future_predictions[i]))
}

# ========== Tips and Best Practices ==========

cat("\n========== LSTM Best Practices ==========\n\n")
cat("1. Data Preprocessing:\n")
cat("   - Normalize/standardize input data\n")
cat("   - Handle missing values appropriately\n")
cat("   - Consider detrending for non-stationary series\n\n")

cat("2. Model Architecture:\n")
cat("   - Start with 1-2 LSTM layers\n")
cat("   - Use dropout for regularization (0.2-0.5)\n")
cat("   - Consider bidirectional LSTM for complex patterns\n\n")

cat("3. Training:\n")
cat("   - Use appropriate batch size (16-128)\n")
cat("   - Monitor validation loss to prevent overfitting\n")
cat("   - Use early stopping and model checkpointing\n\n")

cat("4. Hyperparameter Tuning:\n")
cat("   - Sequence length: depends on temporal dependencies\n")
cat("   - LSTM units: 32-256 typically works well\n")
cat("   - Learning rate: 0.001-0.01 for Adam optimizer\n\n")

cat("5. Evaluation:\n")
cat("   - Use walk-forward validation for time series\n")
cat("   - Check residuals for patterns\n")
cat("   - Compare with baseline models (ARIMA, simple average)\n\n")

cat("========== Example Complete ==========\n")
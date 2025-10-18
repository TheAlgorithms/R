# ==============================================
# Convolutional Neural Network (CNN)
# ==============================================
# Algorithm: Deep learning model using convolutional, pooling, and dense layers.
# Framework: Keras (TensorFlow backend)
#
# Purpose:
# - Automatically extract spatial and hierarchical features from image data.
# - Commonly used for image classification, object detection, and visual recognition.
#
# Architecture Steps:
# 1. Convolution Layer: Extracts local spatial patterns using learnable filters.
# 2. Activation (ReLU): Adds non-linearity by thresholding at zero.
# 3. Pooling Layer: Reduces spatial dimensions (downsampling) while preserving features.
# 4. Flatten Layer: Converts 2D feature maps into 1D vector.
# 5. Dense Layers: Combines extracted features for classification.
# 6. Output Layer: Uses Softmax activation for class probabilities.
#
# Complexity:
# - Time:  O(E × N × F × K²)  where E=epochs, N=samples, F=filters, K=kernel size
# - Space: O(parameters + feature maps)
#
# Reference:
# LeCun et al., "Gradient-based learning applied to document recognition" (1998)
# https://yann.lecun.com/exdb/lenet/
#
# ==============================================

# Load Required Library
suppressPackageStartupMessages(library(keras))

# Define CNN Architecture as a Function (Reusable)
build_cnn_model <- function(input_shape = c(28, 28, 1), num_classes = 10) {
  keras_model_sequential() %>%
    layer_conv_2d(
      filters = 32, kernel_size = c(3, 3), activation = "relu",
      input_shape = input_shape, padding = "same"
    ) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_conv_2d(
      filters = 64, kernel_size = c(3, 3),
      activation = "relu", padding = "same"
    ) %>%
    layer_max_pooling_2d(pool_size = c(2, 2)) %>%
    layer_flatten() %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dense(units = num_classes, activation = "softmax")
}

# Example: Display Model Summary (only in interactive sessions)
if (interactive()) {
  model <- build_cnn_model()
  summary(model)
}

# ==============================================
# Note:
# - This script defines the CNN algorithm structure only.
# - You can compile and train it using model %>% compile() and model %>% fit()
#   with any dataset (e.g., MNIST, CIFAR-10).
# ==============================================

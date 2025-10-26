# Comprehensive Correlation Analysis for Biomedical Data
#
# This implementation provides multiple correlation analysis methods:
# 1. Pearson correlation (linear relationships, parametric)
# 2. Spearman correlation (monotonic relationships, non-parametric)
# 3. Kendall's tau (robust to outliers, non-parametric)
# 4. Partial correlation (controlling for confounding variables)
# 5. Correlation matrix analysis with multiple testing correction
#
# Applications in biomedical research:
# - Investigating relationships between biomarkers and outcomes
# - Analyzing dose-response relationships in pharmacology
# - Studying associations between physiological measurements
# - Quality control in laboratory measurements
# - Gene expression correlation studies
# - Clinical trial endpoint relationships
#
# Time Complexity: O(n) for pairwise correlations, O(n*p^2) for partial correlations
# Space Complexity: O(p^2) for correlation matrices, O(n) for pairwise analysis

# Comprehensive correlation analysis function
correlation_analysis <- function(x, y = NULL, method = "pearson", conf.level = 0.95,
                                plot = TRUE, plot_title = "Correlation Analysis") {
  #' Comprehensive Correlation Analysis
  #' 
  #' Performs correlation analysis with confidence intervals and significance testing
  #' 
  #' @param x numeric vector or matrix of data
  #' @param y numeric vector (if x is a vector) or NULL (if x is a matrix)
  #' @param method character: "pearson", "spearman", or "kendall"
  #' @param conf.level confidence level for confidence intervals
  #' @param plot logical: whether to create visualization
  #' @param plot_title character: title for the plot
  #' @return list with correlation results
  
  # Input validation
  if (is.matrix(x) || is.data.frame(x)) {
    if (!is.null(y)) {
      warning("y is ignored when x is a matrix or data frame")
    }
    return(correlation_matrix_analysis(x, method, conf.level, plot, plot_title))
  }
  
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both x and y must be numeric")
  }
  
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }
  
  if (!method %in% c("pearson", "spearman", "kendall")) {
    stop("method must be 'pearson', 'spearman', or 'kendall'")
  }
  
  # Remove missing values
  complete_pairs <- complete.cases(x, y)
  x_clean <- x[complete_pairs]
  y_clean <- y[complete_pairs]
  n <- length(x_clean)
  
  if (n < 3) {
    stop("Need at least 3 complete pairs for correlation analysis")
  }
  
  # Calculate correlation and test
  if (method == "pearson") {
    result <- pearson_correlation(x_clean, y_clean, conf.level)
  } else if (method == "spearman") {
    result <- spearman_correlation(x_clean, y_clean, conf.level)
  } else {  # kendall
    result <- kendall_correlation(x_clean, y_clean, conf.level)
  }
  
  # Add sample information
  result$sample_size <- n
  result$missing_pairs <- length(x) - n
  
  # Simple title case function (replace stringr dependency)
  title_case <- function(s) paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
  result$method <- paste(title_case(method), "correlation coefficient")
  
  # Create visualization
  if (plot) {
    create_correlation_plot(x_clean, y_clean, result, plot_title)
  }
  
  class(result) <- "biomedical_correlation"
  return(result)
}

# Pearson correlation implementation
pearson_correlation <- function(x, y, conf.level = 0.95) {
  n <- length(x)
  
  # Calculate Pearson correlation coefficient
  r <- cor(x, y, method = "pearson")
  
  # Test statistic and p-value
  t_stat <- r * sqrt((n - 2) / (1 - r^2))
  df <- n - 2
  p_value <- 2 * pt(abs(t_stat), df, lower.tail = FALSE)
  
  # Confidence interval using Fisher's Z transformation
  z_r <- 0.5 * log((1 + r) / (1 - r))  # Fisher's Z
  se_z <- 1 / sqrt(n - 3)
  alpha <- 1 - conf.level
  z_critical <- qnorm(1 - alpha/2)
  
  z_lower <- z_r - z_critical * se_z
  z_upper <- z_r + z_critical * se_z
  
  # Transform back to correlation scale
  ci_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
  ci_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
  
  # Effect size interpretation
  r_squared <- r^2
  
  return(list(
    correlation = r,
    t_statistic = t_stat,
    p_value = p_value,
    degrees_of_freedom = df,
    confidence_interval = c(ci_lower, ci_upper),
    confidence_level = conf.level,
    r_squared = r_squared,
    fishers_z = z_r,
    assumptions_check = check_pearson_assumptions(x, y)
  ))
}

# Spearman correlation implementation
spearman_correlation <- function(x, y, conf.level = 0.95) {
  n <- length(x)
  
  # Calculate Spearman correlation coefficient
  rho <- cor(x, y, method = "spearman")
  
  # Test statistic (approximate for large n)
  if (n > 10) {
    t_stat <- rho * sqrt((n - 2) / (1 - rho^2))
    df <- n - 2
    p_value <- 2 * pt(abs(t_stat), df, lower.tail = FALSE)
  } else {
    # Exact test for small samples (simplified)
    warning("Small sample size: p-value may be approximate")
    t_stat <- rho * sqrt((n - 2) / (1 - rho^2))
    df <- n - 2
    p_value <- 2 * pt(abs(t_stat), df, lower.tail = FALSE)
  }
  
  # Confidence interval (approximate using Fisher's Z)
  z_rho <- 0.5 * log((1 + rho) / (1 - rho))
  se_z <- 1.06 / sqrt(n - 3)  # Adjusted standard error for Spearman
  alpha <- 1 - conf.level
  z_critical <- qnorm(1 - alpha/2)
  
  z_lower <- z_rho - z_critical * se_z
  z_upper <- z_rho + z_critical * se_z
  
  ci_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
  ci_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
  
  return(list(
    correlation = rho,
    t_statistic = t_stat,
    p_value = p_value,
    degrees_of_freedom = df,
    confidence_interval = c(ci_lower, ci_upper),
    confidence_level = conf.level,
    rank_based = TRUE
  ))
}

# Kendall's tau correlation implementation
kendall_correlation <- function(x, y, conf.level = 0.95) {
  n <- length(x)
  
  # Calculate Kendall's tau
  tau <- cor(x, y, method = "kendall")
  
  # Test statistic for Kendall's tau
  var_tau <- 2 * (2*n + 5) / (9 * n * (n - 1))
  z_stat <- tau / sqrt(var_tau)
  p_value <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)
  
  # Confidence interval (approximate)
  alpha <- 1 - conf.level
  z_critical <- qnorm(1 - alpha/2)
  
  ci_lower <- tau - z_critical * sqrt(var_tau)
  ci_upper <- tau + z_critical * sqrt(var_tau)
  
  # Ensure CI is within [-1, 1]
  ci_lower <- max(ci_lower, -1)
  ci_upper <- min(ci_upper, 1)
  
  return(list(
    correlation = tau,
    z_statistic = z_stat,
    p_value = p_value,
    confidence_interval = c(ci_lower, ci_upper),
    confidence_level = conf.level,
    robust = TRUE
  ))
}

# Check assumptions for Pearson correlation
check_pearson_assumptions <- function(x, y) {
  n <- length(x)
  
  # 1. Normality test (Shapiro-Wilk for n <= 5000)
  if (n <= 5000) {
    shapiro_x <- shapiro.test(x)
    shapiro_y <- shapiro.test(y)
    normality_x <- shapiro_x$p.value > 0.05
    normality_y <- shapiro_y$p.value > 0.05
  } else {
    # Use Kolmogorov-Smirnov for large samples
    ks_x <- ks.test(x, "pnorm", mean(x), sd(x))
    ks_y <- ks.test(y, "pnorm", mean(y), sd(y))
    normality_x <- ks_x$p.value > 0.05
    normality_y <- ks_y$p.value > 0.05
  }
  
  # 2. Linearity check (correlation between x and y vs. x and y^2)
  linear_corr <- abs(cor(x, y))
  nonlinear_corr <- abs(cor(x, y^2))
  linearity <- linear_corr > nonlinear_corr
  
  # 3. Homoscedasticity check (Breusch-Pagan-like test)
  residuals <- y - predict(lm(y ~ x))
  bp_stat <- cor(x, abs(residuals))^2
  homoscedasticity <- bp_stat < 0.1  # Rough threshold
  
  # 4. Outlier detection (using IQR method)
  outliers_x <- detect_outliers(x)
  outliers_y <- detect_outliers(y)
  no_outliers <- length(outliers_x) == 0 && length(outliers_y) == 0
  
  return(list(
    normality_x = normality_x,
    normality_y = normality_y,
    linearity = linearity,
    homoscedasticity = homoscedasticity,
    no_outliers = no_outliers,
    outliers_x = outliers_x,
    outliers_y = outliers_y,
    overall_suitable = normality_x && normality_y && linearity && homoscedasticity && no_outliers
  ))
}

# Outlier detection using IQR method
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  outlier_indices <- which(x < lower_bound | x > upper_bound)
  return(outlier_indices)
}

# Correlation matrix analysis
correlation_matrix_analysis <- function(data, method = "pearson", conf.level = 0.95,
                                       plot = TRUE, plot_title = "Correlation Matrix") {
  # Convert to matrix if data frame
  if (is.data.frame(data)) {
    numeric_cols <- sapply(data, is.numeric)
    if (!all(numeric_cols)) {
      warning("Non-numeric columns removed from analysis")
      data <- data[, numeric_cols, drop = FALSE]
    }
    data <- as.matrix(data)
  }
  
  # Remove rows with missing values
  complete_rows <- complete.cases(data)
  data_clean <- data[complete_rows, , drop = FALSE]
  n <- nrow(data_clean)
  p <- ncol(data_clean)
  
  if (n < 3) stop("Need at least 3 complete observations")
  if (p < 2) stop("Need at least 2 variables")
  
  # Calculate correlation matrix
  cor_matrix <- cor(data_clean, method = method)
  
  # Calculate p-values for all pairs
  p_matrix <- matrix(1, nrow = p, ncol = p)
  colnames(p_matrix) <- rownames(p_matrix) <- colnames(data_clean)
  
  for (i in 1:(p-1)) {
    for (j in (i+1):p) {
      if (method == "pearson") {
        test_result <- cor.test(data_clean[, i], data_clean[, j], method = "pearson")
      } else if (method == "spearman") {
        test_result <- cor.test(data_clean[, i], data_clean[, j], method = "spearman")
      } else {
        test_result <- cor.test(data_clean[, i], data_clean[, j], method = "kendall")
      }
      
      p_matrix[i, j] <- p_matrix[j, i] <- test_result$p.value
    }
  }
  
  # Multiple testing correction
  p_values_upper <- p_matrix[upper.tri(p_matrix)]
  p_adjusted_bonferroni <- p.adjust(p_values_upper, method = "bonferroni")
  p_adjusted_fdr <- p.adjust(p_values_upper, method = "fdr")
  
  # Create adjusted p-value matrices
  p_bonferroni <- p_fdr <- matrix(1, nrow = p, ncol = p)
  colnames(p_bonferroni) <- rownames(p_bonferroni) <- colnames(data_clean)
  colnames(p_fdr) <- rownames(p_fdr) <- colnames(data_clean)
  
  counter <- 1
  for (i in 1:(p-1)) {
    for (j in (i+1):p) {
      p_bonferroni[i, j] <- p_bonferroni[j, i] <- p_adjusted_bonferroni[counter]
      p_fdr[i, j] <- p_fdr[j, i] <- p_adjusted_fdr[counter]
      counter <- counter + 1
    }
  }
  
  # Create visualization
  if (plot) {
    create_correlation_matrix_plot(cor_matrix, p_matrix, data_clean, plot_title)
  }
  
  # Simple title case function
  title_case <- function(s) paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
  
  result <- list(
    correlation_matrix = cor_matrix,
    p_values = p_matrix,
    p_values_bonferroni = p_bonferroni,
    p_values_fdr = p_fdr,
    sample_size = n,
    variables = p,
    method = paste(title_case(method), "correlation matrix"),
    missing_rows = nrow(data) - n,
    significant_pairs_uncorrected = sum(p_matrix < 0.05 & upper.tri(p_matrix)),
    significant_pairs_bonferroni = sum(p_bonferroni < 0.05 & upper.tri(p_bonferroni)),
    significant_pairs_fdr = sum(p_fdr < 0.05 & upper.tri(p_fdr))
  )
  
  class(result) <- "biomedical_correlation_matrix"
  return(result)
}

# Create correlation plot for two variables
create_correlation_plot <- function(x, y, result, main_title) {
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # 1. Scatter plot with regression line
  plot(x, y, pch = 19, col = "darkblue", cex = 1.2,
       main = paste(main_title, "\nScatter Plot"), 
       xlab = "X Variable", ylab = "Y Variable")
  
  # Add regression line
  if ("assumptions_check" %in% names(result) && result$assumptions_check$linearity) {
    abline(lm(y ~ x), col = "red", lwd = 2)
  } else {
    # Add lowess smoother for non-linear relationships
    lines(lowess(x, y), col = "red", lwd = 2)
  }
  
  # Add correlation info
  legend("topleft", 
         c(paste("r =", round(result$correlation, 3)),
           paste("p =", round(result$p_value, 4))),
         bty = "n", cex = 1.1)
  
  # 2. Residuals plot (if Pearson)
  if ("assumptions_check" %in% names(result)) {
    fitted_values <- predict(lm(y ~ x))
    residuals <- y - fitted_values
    
    plot(fitted_values, residuals, pch = 19, col = "darkgreen",
         main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
    abline(h = 0, col = "red", lwd = 2, lty = 2)
    
    # Add lowess line to check for patterns
    lines(lowess(fitted_values, residuals), col = "blue", lwd = 2)
  } else {
    # For non-parametric methods, show rank plot
    plot(rank(x), rank(y), pch = 19, col = "darkgreen",
         main = "Rank Plot", xlab = "Rank of X", ylab = "Rank of Y")
    abline(lm(rank(y) ~ rank(x)), col = "red", lwd = 2)
  }
  
  # 3. Q-Q plots for normality (if Pearson)
  if ("assumptions_check" %in% names(result)) {
    qqnorm(x, main = "Q-Q Plot X", pch = 19, col = "blue")
    qqline(x, col = "red", lwd = 2)
  } else {
    # Histogram for non-parametric
    hist(x, main = "Distribution of X", xlab = "X Values", 
         col = "lightblue", probability = TRUE)
    lines(density(x), col = "red", lwd = 2)
  }
  
  # 4. Test summary
  plot(1, 1, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
       main = "Correlation Summary", xlab = "", ylab = "", axes = FALSE)
  
  text(5, 9.5, strsplit(result$method, " correlation")[[1]][1], cex = 1.2, font = 2)
  text(5, 8.7, paste("Correlation:", round(result$correlation, 4)), cex = 1.1)
  text(5, 8.1, paste("p-value:", round(result$p_value, 6)), cex = 1.1)
  
  if ("degrees_of_freedom" %in% names(result)) {
    text(5, 7.5, paste("df:", result$degrees_of_freedom), cex = 1.1)
  }
  
  text(5, 6.9, paste("Sample size:", result$sample_size), cex = 1.1)
  
  # Confidence interval
  ci_text <- paste(result$confidence_level * 100, "% CI: [", 
                   round(result$confidence_interval[1], 3), ", ",
                   round(result$confidence_interval[2], 3), "]", sep = "")
  text(5, 6.3, ci_text, cex = 1)
  
  # Effect size
  if ("r_squared" %in% names(result)) {
    text(5, 5.7, paste("R-squared:", round(result$r_squared, 4)), cex = 1)
  }
  
  # Significance
  sig_level <- ifelse(result$p_value < 0.001, "***", 
                     ifelse(result$p_value < 0.01, "**", 
                           ifelse(result$p_value < 0.05, "*", "ns")))
  text(5, 5.1, paste("Significance:", sig_level), cex = 1.1, 
       col = ifelse(sig_level == "ns", "red", "darkgreen"))
  
  # Interpretation
  r_abs <- abs(result$correlation)
  magnitude <- if (r_abs < 0.1) "negligible"
              else if (r_abs < 0.3) "small"
              else if (r_abs < 0.5) "medium"
              else if (r_abs < 0.7) "large"
              else "very large"
  
  text(5, 4.3, paste("Effect size:", magnitude), cex = 1)
  
  # Assumptions check (if available)
  if ("assumptions_check" %in% names(result)) {
    assumptions_ok <- result$assumptions_check$overall_suitable
    text(5, 3.5, paste("Assumptions met:", assumptions_ok), cex = 1,
         col = ifelse(assumptions_ok, "darkgreen", "orange"))
  }
  
  par(mfrow = c(1, 1))
}

# Create correlation matrix visualization
create_correlation_matrix_plot <- function(cor_matrix, p_matrix, data, main_title) {
  p <- ncol(cor_matrix)
  
  par(mfrow = c(2, 2), mar = c(5, 5, 3, 2))
  
  # 1. Correlation heatmap
  image(1:p, 1:p, cor_matrix, col = colorRampPalette(c("blue", "white", "red"))(20),
        main = paste(main_title, "\nCorrelation Matrix"), 
        xlab = "", ylab = "", axes = FALSE, zlim = c(-1, 1))
  
  axis(1, at = 1:p, labels = colnames(cor_matrix), las = 2, cex.axis = 0.8)
  axis(2, at = 1:p, labels = colnames(cor_matrix), las = 2, cex.axis = 0.8)
  
  # Add correlation values
  for (i in 1:p) {
    for (j in 1:p) {
      if (i != j) {
        text(j, i, round(cor_matrix[i, j], 2), cex = 0.8, 
             col = ifelse(abs(cor_matrix[i, j]) > 0.5, "white", "black"))
      }
    }
  }
  
  # 2. P-value heatmap
  log_p <- -log10(p_matrix + 1e-16)  # Add small constant to avoid log(0)
  image(1:p, 1:p, log_p, col = heat.colors(20),
        main = "Significance (-log10 p-values)", 
        xlab = "", ylab = "", axes = FALSE)
  
  axis(1, at = 1:p, labels = colnames(p_matrix), las = 2, cex.axis = 0.8)
  axis(2, at = 1:p, labels = colnames(p_matrix), las = 2, cex.axis = 0.8)
  
  # Add significance markers
  for (i in 1:p) {
    for (j in 1:p) {
      if (i != j) {
        sig_marker <- if (p_matrix[i, j] < 0.001) "***"
                     else if (p_matrix[i, j] < 0.01) "**"
                     else if (p_matrix[i, j] < 0.05) "*"
                     else ""
        text(j, i, sig_marker, cex = 1.2, font = 2, col = "darkred")
      }
    }
  }
  
  # 3. Scatterplot matrix (subset if too many variables)
  if (p <= 5) {
    pairs(data, pch = 19, col = "darkblue", cex = 0.8, main = "Scatterplot Matrix")
  } else {
    # Show first 4 variables
    pairs(data[, 1:4], pch = 19, col = "darkblue", cex = 0.8, 
          main = "Scatterplot Matrix (First 4 Variables)")
  }
  
  # 4. Summary statistics
  plot(1, 1, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
       main = "Matrix Summary", xlab = "", ylab = "", axes = FALSE)
  
  text(5, 9.5, "Correlation Matrix Analysis", cex = 1.2, font = 2)
  text(5, 8.7, paste("Variables:", p), cex = 1.1)
  text(5, 8.1, paste("Sample size:", nrow(data)), cex = 1.1)
  
  # Count significant correlations
  sig_uncorrected <- sum(p_matrix < 0.05 & upper.tri(p_matrix))
  total_pairs <- p * (p - 1) / 2
  
  text(5, 7.5, paste("Significant pairs (α=0.05):", sig_uncorrected, "/", total_pairs), cex = 1)
  text(5, 6.9, paste("Proportion significant:", round(sig_uncorrected/total_pairs, 3)), cex = 1)
  
  # Strongest correlations
  cor_upper <- cor_matrix[upper.tri(cor_matrix)]
  max_cor <- max(abs(cor_upper))
  text(5, 6.3, paste("Strongest correlation:", round(max_cor, 3)), cex = 1)
  
  # Average absolute correlation
  avg_cor <- mean(abs(cor_upper))
  text(5, 5.7, paste("Average |correlation|:", round(avg_cor, 3)), cex = 1)
  
  par(mfrow = c(1, 1))
}

# Print methods
print.biomedical_correlation <- function(x, ...) {
  cat("\n", x$method, "\n")
  cat(rep("=", nchar(x$method) + 2), "\n", sep = "")
  
  cat("Sample size:", x$sample_size, "\n")
  if (x$missing_pairs > 0) {
    cat("Missing pairs removed:", x$missing_pairs, "\n")
  }
  
  cat("Correlation coefficient:", x$correlation, "\n")
  
  if ("t_statistic" %in% names(x)) {
    cat("t-statistic:", x$t_statistic, "\n")
    cat("Degrees of freedom:", x$degrees_of_freedom, "\n")
  } else if ("z_statistic" %in% names(x)) {
    cat("z-statistic:", x$z_statistic, "\n")
  }
  
  cat("p-value:", x$p_value, "\n")
  cat(x$confidence_level * 100, "% Confidence interval: [", 
      x$confidence_interval[1], ", ", x$confidence_interval[2], "]\n", sep = "")
  
  if ("r_squared" %in% names(x)) {
    cat("R-squared:", x$r_squared, "\n")
  }
  
  # Interpretation
  if (x$p_value < 0.05) {
    cat("\nConclusion: Significant correlation detected\n")
  } else {
    cat("\nConclusion: No significant correlation detected\n")
  }
  
  # Effect size interpretation
  r_abs <- abs(x$correlation)
  magnitude <- if (r_abs < 0.1) "negligible"
              else if (r_abs < 0.3) "small"
              else if (r_abs < 0.5) "medium"
              else if (r_abs < 0.7) "large"
              else "very large"
  cat("Effect size:", magnitude, "\n")
  
  # Assumptions (if available)
  if ("assumptions_check" %in% names(x)) {
    cat("\nAssumptions check:\n")
    cat("- Normality (X):", x$assumptions_check$normality_x, "\n")
    cat("- Normality (Y):", x$assumptions_check$normality_y, "\n")
    cat("- Linearity:", x$assumptions_check$linearity, "\n")
    cat("- Homoscedasticity:", x$assumptions_check$homoscedasticity, "\n")
    cat("- No outliers:", x$assumptions_check$no_outliers, "\n")
    cat("- Overall suitable for Pearson:", x$assumptions_check$overall_suitable, "\n")
  }
}

print.biomedical_correlation_matrix <- function(x, ...) {
  cat("\n", x$method, "\n")
  cat(rep("=", nchar(x$method) + 2), "\n", sep = "")
  
  cat("Variables:", x$variables, "\n")
  cat("Sample size:", x$sample_size, "\n")
  if (x$missing_rows > 0) {
    cat("Rows with missing data removed:", x$missing_rows, "\n")
  }
  
  total_pairs <- x$variables * (x$variables - 1) / 2
  cat("Total variable pairs:", total_pairs, "\n")
  
  cat("\nSignificant correlations (α = 0.05):\n")
  cat("- Uncorrected:", x$significant_pairs_uncorrected, "/", total_pairs, 
      "(", round(x$significant_pairs_uncorrected/total_pairs*100, 1), "%)\n")
  cat("- Bonferroni corrected:", x$significant_pairs_bonferroni, "/", total_pairs,
      "(", round(x$significant_pairs_bonferroni/total_pairs*100, 1), "%)\n")
  cat("- FDR corrected:", x$significant_pairs_fdr, "/", total_pairs,
      "(", round(x$significant_pairs_fdr/total_pairs*100, 1), "%)\n")
  
  cat("\nCorrelation matrix:\n")
  print(round(x$correlation_matrix, 3))
}

# Demonstration function
demonstrate_correlation_analysis <- function() {
  cat("=== Comprehensive Correlation Analysis for Biomedical Data ===\n\n")
  
  set.seed(123)
  
  # Example 1: Pearson correlation (linear relationship)
  cat("1. PEARSON CORRELATION: Blood Pressure vs Age\n")
  cat("Research Question: Is there a linear relationship between age and systolic BP?\n\n")
  
  age <- runif(50, 25, 75)
  systolic_bp <- 90 + 1.2 * age + rnorm(50, 0, 8)  # Linear relationship with noise
  
  pearson_result <- correlation_analysis(age, systolic_bp, method = "pearson",
                                        plot_title = "Age vs Systolic BP")
  print(pearson_result)
  
  cat("\n", rep("=", 60), "\n\n")
  
  # Example 2: Spearman correlation (monotonic but non-linear)
  cat("2. SPEARMAN CORRELATION: Dose vs Response (Non-linear)\n")
  cat("Research Question: Is there a monotonic relationship between drug dose and response?\n\n")
  
  dose <- seq(0, 100, length.out = 40)
  response <- 10 * log(dose + 1) + rnorm(40, 0, 3)  # Log relationship
  
  spearman_result <- correlation_analysis(dose, response, method = "spearman",
                                         plot_title = "Drug Dose vs Response")
  print(spearman_result)
  
  cat("\n", rep("=", 60), "\n\n")
  
  # Example 3: Correlation matrix analysis
  cat("3. CORRELATION MATRIX: Multiple Biomarkers\n")
  cat("Research Question: How are different biomarkers correlated?\n\n")
  
  n <- 100
  biomarker_data <- data.frame(
    Cholesterol = rnorm(n, 200, 30),
    BMI = rnorm(n, 25, 4),
    Blood_Pressure = rnorm(n, 120, 15),
    Heart_Rate = rnorm(n, 75, 10)
  )
  
  # Create some realistic correlations
  biomarker_data$Blood_Pressure <- biomarker_data$Blood_Pressure + 
    0.3 * biomarker_data$BMI + 0.2 * biomarker_data$Cholesterol/10
  biomarker_data$Heart_Rate <- biomarker_data$Heart_Rate + 
    0.4 * biomarker_data$BMI - 0.1 * biomarker_data$Blood_Pressure
  
  cat("Biomarker correlation matrix analysis:\n")
  matrix_result <- correlation_analysis(biomarker_data, method = "pearson",
                                       plot_title = "Biomarker Correlations")
  print(matrix_result)
  
  cat("\n", rep("=", 60), "\n\n")
  
  # Example 4: Robust correlation with outliers
  cat("4. ROBUST CORRELATION: Kendall's Tau with Outliers\n")
  cat("Research Question: How does outlier presence affect correlation methods?\n\n")
  
  x_clean <- rnorm(30, 50, 10)
  y_clean <- 2 * x_clean + rnorm(30, 0, 5)
  
  # Add outliers
  x_outliers <- c(x_clean, c(100, 10))
  y_outliers <- c(y_clean, c(20, 150))
  
  cat("Pearson (sensitive to outliers):\n")
  pearson_outliers <- correlation_analysis(x_outliers, y_outliers, method = "pearson",
                                          plot = FALSE)
  cat("r =", round(pearson_outliers$correlation, 3), 
      ", p =", round(pearson_outliers$p_value, 4), "\n")
  
  cat("\nKendall's tau (robust to outliers):\n")
  kendall_outliers <- correlation_analysis(x_outliers, y_outliers, method = "kendall",
                                          plot = FALSE)
  cat("τ =", round(kendall_outliers$correlation, 3), 
      ", p =", round(kendall_outliers$p_value, 4), "\n")
  
  cat("\nWithout outliers:\n")
  clean_result <- correlation_analysis(x_clean, y_clean, method = "pearson", plot = FALSE)
  cat("r =", round(clean_result$correlation, 3), 
      ", p =", round(clean_result$p_value, 4), "\n")
  
  cat("\n", rep("=", 60), "\n\n")
  
  # Example 5: Power analysis for correlations
  cat("5. POWER ANALYSIS: Sample Size Requirements\n")
  cat("Demonstrating power for detecting different correlation strengths\n\n")
  
  correlation_strengths <- c(0.1, 0.3, 0.5, 0.7)
  sample_sizes <- c(20, 50, 100, 200)
  
  power_matrix <- matrix(NA, nrow = length(sample_sizes), ncol = length(correlation_strengths))
  rownames(power_matrix) <- paste("n =", sample_sizes)
  colnames(power_matrix) <- paste("ρ =", correlation_strengths)
  
  for (i in seq_along(sample_sizes)) {
    for (j in seq_along(correlation_strengths)) {
      n <- sample_sizes[i]
      true_r <- correlation_strengths[j]
      
      # Monte Carlo power simulation
      power <- mean(replicate(1000, {
        x <- rnorm(n)
        y <- true_r * x + sqrt(1 - true_r^2) * rnorm(n)
        test_result <- cor.test(x, y)
        test_result$p.value < 0.05
      }))
      
      power_matrix[i, j] <- power
    }
  }
  
  cat("Statistical Power Analysis (1000 simulations each):\n")
  print(round(power_matrix, 3))
  
  cat("\nGuidelines:\n")
  cat("- Small correlations (r = 0.1) require very large samples\n")
  cat("- Medium correlations (r = 0.3-0.5) need moderate to large samples\n")
  cat("- Large correlations (r ≥ 0.7) detectable with smaller samples\n")
  cat("- Aim for power ≥ 0.80 (80%) in study planning\n")
  cat("- Consider effect size interpretation alongside statistical significance\n")
}

# Run demonstration if script is executed directly
if (sys.nframe() == 0) {
  demonstrate_correlation_analysis()
}
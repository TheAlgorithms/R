# Comprehensive T-Tests for Biomedical Data Analysis
#
# This implementation provides multiple types of t-tests commonly used in biomedical research:
# 1. One-sample t-test (compare sample mean to known value)
# 2. Two-sample t-test (compare means of two independent groups)
# 3. Paired t-test (compare means of paired/dependent samples)
# 4. Welch's t-test (two-sample with unequal variances)
#
# Applications in biomedical research:
# - Clinical trials: comparing treatment effects
# - Laboratory studies: comparing measurements between groups
# - Epidemiological studies: comparing health outcomes
# - Pharmaceutical research: drug efficacy analysis
# - Medical device testing: performance comparisons
#
# Time Complexity: O(n) for all tests
# Space Complexity: O(1) for calculations, O(n) for visualizations

# Helper function to create high-quality plots
create_biomedical_plot <- function(data, title, subtitle = "", save_plot = FALSE, filename = NULL) {
  if (save_plot && !is.null(filename)) {
    png(filename, width = 800, height = 600, res = 100)
  }
  
  # Set up plotting parameters for professional appearance
  par(mfrow = c(1, 1), mar = c(5, 5, 4, 2), cex.main = 1.2, cex.lab = 1.1, cex.axis = 1.0)
  
  return(invisible())
}

# One-sample t-test implementation
one_sample_t_test <- function(x, mu = 0, alternative = "two.sided", conf.level = 0.95, 
                              plot = TRUE, plot_title = "One-Sample T-Test") {
  #' One-Sample T-Test
  #' 
  #' Tests whether the mean of a sample differs significantly from a hypothesized value
  #' 
  #' @param x numeric vector of sample data
  #' @param mu hypothesized population mean (null hypothesis value)
  #' @param alternative character: "two.sided", "less", or "greater"
  #' @param conf.level confidence level for confidence interval
  #' @param plot logical: whether to create visualization
  #' @param plot_title character: title for the plot
  #' @return list with test results
  
  # Input validation
  if (!is.numeric(x)) stop("x must be numeric")
  if (length(x) < 2) stop("Sample size must be at least 2")
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop("alternative must be 'two.sided', 'less', or 'greater'")
  }
  if (conf.level <= 0 || conf.level >= 1) stop("conf.level must be between 0 and 1")
  
  # Remove missing values
  x <- x[!is.na(x)]
  n <- length(x)
  
  if (n < 2) stop("Need at least 2 non-missing observations")
  
  # Calculate test statistics
  sample_mean <- mean(x)
  sample_sd <- sd(x)
  se <- sample_sd / sqrt(n)
  t_statistic <- (sample_mean - mu) / se
  df <- n - 1
  
  # Calculate p-value based on alternative hypothesis
  if (alternative == "two.sided") {
    p_value <- 2 * pt(abs(t_statistic), df, lower.tail = FALSE)
  } else if (alternative == "less") {
    p_value <- pt(t_statistic, df, lower.tail = TRUE)
  } else {  # greater
    p_value <- pt(t_statistic, df, lower.tail = FALSE)
  }
  
  # Calculate confidence interval
  alpha <- 1 - conf.level
  t_critical <- qt(1 - alpha/2, df)
  ci_lower <- sample_mean - t_critical * se
  ci_upper <- sample_mean + t_critical * se
  
  # Effect size (Cohen's d)
  cohens_d <- (sample_mean - mu) / sample_sd
  
  # Create visualization
  if (plot) {
    # Set up plot layout
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
    
    # 1. Histogram with normal overlay
    hist(x, breaks = max(5, ceiling(sqrt(n))), probability = TRUE, 
         main = paste(plot_title, "\nSample Distribution"), 
         xlab = "Values", ylab = "Density", col = "lightblue", border = "black")
    
    # Add normal curve overlay
    x_seq <- seq(min(x), max(x), length.out = 100)
    normal_curve <- dnorm(x_seq, mean = sample_mean, sd = sample_sd)
    lines(x_seq, normal_curve, col = "red", lwd = 2)
    abline(v = sample_mean, col = "blue", lwd = 2, lty = 2)
    abline(v = mu, col = "green", lwd = 2, lty = 2)
    legend("topright", c("Sample Mean", "Hypothesized Mean", "Normal Fit"), 
           col = c("blue", "green", "red"), lty = c(2, 2, 1), lwd = 2, cex = 0.8)
    
    # 2. Box plot
    boxplot(x, main = "Box Plot", ylab = "Values", col = "lightgreen")
    abline(h = sample_mean, col = "blue", lwd = 2, lty = 2)
    abline(h = mu, col = "green", lwd = 2, lty = 2)
    
    # 3. Q-Q plot for normality check
    qqnorm(x, main = "Q-Q Plot (Normality Check)")
    qqline(x, col = "red", lwd = 2)
    
    # 4. Test summary plot
    plot(1, 1, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
         main = "Test Summary", xlab = "", ylab = "", axes = FALSE)
    
    # Add test results text
    text(5, 9, paste("One-Sample T-Test Results"), cex = 1.2, font = 2)
    text(5, 8, paste("Sample Mean:", round(sample_mean, 4)), cex = 1)
    text(5, 7.3, paste("Hypothesized Mean:", mu), cex = 1)
    text(5, 6.6, paste("t-statistic:", round(t_statistic, 4)), cex = 1)
    text(5, 5.9, paste("p-value:", round(p_value, 6)), cex = 1)
    text(5, 5.2, paste("df:", df), cex = 1)
    text(5, 4.5, paste(conf.level*100, "% CI: [", round(ci_lower, 4), ", ", round(ci_upper, 4), "]", sep = ""), cex = 1)
    text(5, 3.8, paste("Cohen's d:", round(cohens_d, 4)), cex = 1)
    
    # Add significance indication
    sig_level <- ifelse(p_value < 0.001, "***", 
                       ifelse(p_value < 0.01, "**", 
                             ifelse(p_value < 0.05, "*", "ns")))
    text(5, 3.1, paste("Significance:", sig_level), cex = 1, 
         col = ifelse(sig_level == "ns", "red", "darkgreen"))
    
    # Add interpretation
    if (p_value < 0.05) {
      text(5, 2.4, "Result: Reject null hypothesis", cex = 1, col = "darkgreen", font = 2)
    } else {
      text(5, 2.4, "Result: Fail to reject null hypothesis", cex = 1, col = "red", font = 2)
    }
    
    par(mfrow = c(1, 1))  # Reset plot layout
  }
  
  # Return results
  result <- list(
    statistic = t_statistic,
    p_value = p_value,
    degrees_of_freedom = df,
    sample_mean = sample_mean,
    hypothesized_mean = mu,
    standard_error = se,
    confidence_interval = c(ci_lower, ci_upper),
    confidence_level = conf.level,
    alternative = alternative,
    effect_size_cohens_d = cohens_d,
    method = "One-sample t-test",
    sample_size = n,
    data_summary = list(
      mean = sample_mean,
      sd = sample_sd,
      min = min(x),
      max = max(x),
      median = median(x)
    )
  )
  
  class(result) <- "biomedical_ttest"
  return(result)
}

# Two-sample t-test implementation (independent samples)
two_sample_t_test <- function(x, y, alternative = "two.sided", var.equal = TRUE, 
                              conf.level = 0.95, plot = TRUE, plot_title = "Two-Sample T-Test") {
  #' Two-Sample T-Test (Independent Samples)
  #' 
  #' Tests whether the means of two independent groups differ significantly
  #' 
  #' @param x numeric vector for group 1
  #' @param y numeric vector for group 2
  #' @param alternative character: "two.sided", "less", or "greater"
  #' @param var.equal logical: assume equal variances (Student's t) or not (Welch's t)
  #' @param conf.level confidence level for confidence interval
  #' @param plot logical: whether to create visualization
  #' @param plot_title character: title for the plot
  #' @return list with test results
  
  # Input validation
  if (!is.numeric(x) || !is.numeric(y)) stop("Both x and y must be numeric")
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop("alternative must be 'two.sided', 'less', or 'greater'")
  }
  
  # Remove missing values
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  
  if (length(x) < 2 || length(y) < 2) stop("Both groups need at least 2 observations")
  
  n1 <- length(x)
  n2 <- length(y)
  mean1 <- mean(x)
  mean2 <- mean(y)
  sd1 <- sd(x)
  sd2 <- sd(y)
  var1 <- var(x)
  var2 <- var(y)
  
  # Calculate test statistic and degrees of freedom
  if (var.equal) {
    # Student's t-test (equal variances)
    pooled_var <- ((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2)
    se_diff <- sqrt(pooled_var * (1/n1 + 1/n2))
    df <- n1 + n2 - 2
    method <- "Two-sample t-test (equal variances)"
  } else {
    # Welch's t-test (unequal variances)
    se_diff <- sqrt(var1/n1 + var2/n2)
    df <- (var1/n1 + var2/n2)^2 / ((var1/n1)^2/(n1-1) + (var2/n2)^2/(n2-1))
    method <- "Welch's two-sample t-test (unequal variances)"
  }
  
  t_statistic <- (mean1 - mean2) / se_diff
  
  # Calculate p-value
  if (alternative == "two.sided") {
    p_value <- 2 * pt(abs(t_statistic), df, lower.tail = FALSE)
  } else if (alternative == "less") {
    p_value <- pt(t_statistic, df, lower.tail = TRUE)
  } else {  # greater
    p_value <- pt(t_statistic, df, lower.tail = FALSE)
  }
  
  # Confidence interval for difference in means
  alpha <- 1 - conf.level
  t_critical <- qt(1 - alpha/2, df)
  mean_diff <- mean1 - mean2
  ci_lower <- mean_diff - t_critical * se_diff
  ci_upper <- mean_diff + t_critical * se_diff
  
  # Effect size (Cohen's d)
  if (var.equal) {
    pooled_sd <- sqrt(pooled_var)
  } else {
    pooled_sd <- sqrt(((n1-1)*var1 + (n2-1)*var2) / (n1 + n2 - 2))
  }
  cohens_d <- (mean1 - mean2) / pooled_sd
  
  # Create visualization
  if (plot) {
    par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
    
    # 1. Side-by-side histograms
    x_range <- range(c(x, y))
    breaks_seq <- seq(x_range[1], x_range[2], length.out = 15)
    
    hist(x, breaks = breaks_seq, probability = TRUE, main = "Group 1 Distribution",
         xlab = "Values", ylab = "Density", col = "lightblue", alpha = 0.7)
    abline(v = mean1, col = "blue", lwd = 2, lty = 2)
    
    hist(y, breaks = breaks_seq, probability = TRUE, main = "Group 2 Distribution",
         xlab = "Values", ylab = "Density", col = "lightcoral", alpha = 0.7)
    abline(v = mean2, col = "red", lwd = 2, lty = 2)
    
    # 2. Box plots comparison
    boxplot(list("Group 1" = x, "Group 2" = y), 
            main = "Group Comparison", ylab = "Values", 
            col = c("lightblue", "lightcoral"))
    
    # 3. Density plot overlay
    plot(density(x), main = "Density Comparison", xlab = "Values", ylab = "Density",
         col = "blue", lwd = 2, xlim = x_range)
    lines(density(y), col = "red", lwd = 2)
    abline(v = mean1, col = "blue", lwd = 2, lty = 2)
    abline(v = mean2, col = "red", lwd = 2, lty = 2)
    legend("topright", c("Group 1", "Group 2", "Mean 1", "Mean 2"), 
           col = c("blue", "red", "blue", "red"), lty = c(1, 1, 2, 2), lwd = 2, cex = 0.8)
    
    # 4. Q-Q plots for normality
    qqnorm(x, main = "Q-Q Plot Group 1", col = "blue")
    qqline(x, col = "darkblue", lwd = 2)
    
    qqnorm(y, main = "Q-Q Plot Group 2", col = "red")
    qqline(y, col = "darkred", lwd = 2)
    
    # 5. Test summary
    plot(1, 1, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
         main = "Test Summary", xlab = "", ylab = "", axes = FALSE)
    
    text(5, 9.5, paste("Two-Sample T-Test Results"), cex = 1.2, font = 2)
    text(5, 8.8, paste("Group 1 Mean:", round(mean1, 4)), cex = 1)
    text(5, 8.3, paste("Group 2 Mean:", round(mean2, 4)), cex = 1)
    text(5, 7.8, paste("Mean Difference:", round(mean_diff, 4)), cex = 1)
    text(5, 7.3, paste("t-statistic:", round(t_statistic, 4)), cex = 1)
    text(5, 6.8, paste("p-value:", round(p_value, 6)), cex = 1)
    text(5, 6.3, paste("df:", round(df, 2)), cex = 1)
    text(5, 5.8, paste(conf.level*100, "% CI: [", round(ci_lower, 4), ", ", round(ci_upper, 4), "]", sep = ""), cex = 0.9)
    text(5, 5.3, paste("Cohen's d:", round(cohens_d, 4)), cex = 1)
    text(5, 4.8, paste("Method:", ifelse(var.equal, "Student's", "Welch's")), cex = 1)
    
    # Significance and interpretation
    sig_level <- ifelse(p_value < 0.001, "***", 
                       ifelse(p_value < 0.01, "**", 
                             ifelse(p_value < 0.05, "*", "ns")))
    text(5, 4.1, paste("Significance:", sig_level), cex = 1, 
         col = ifelse(sig_level == "ns", "red", "darkgreen"))
    
    if (p_value < 0.05) {
      text(5, 3.4, "Result: Significant difference", cex = 1, col = "darkgreen", font = 2)
    } else {
      text(5, 3.4, "Result: No significant difference", cex = 1, col = "red", font = 2)
    }
    
    par(mfrow = c(1, 1))
  }
  
  # Return results
  result <- list(
    statistic = t_statistic,
    p_value = p_value,
    degrees_of_freedom = df,
    mean_group1 = mean1,
    mean_group2 = mean2,
    mean_difference = mean_diff,
    standard_error = se_diff,
    confidence_interval = c(ci_lower, ci_upper),
    confidence_level = conf.level,
    alternative = alternative,
    var_equal = var.equal,
    effect_size_cohens_d = cohens_d,
    method = method,
    sample_sizes = c(n1, n2),
    group_summaries = list(
      group1 = list(mean = mean1, sd = sd1, n = n1, var = var1),
      group2 = list(mean = mean2, sd = sd2, n = n2, var = var2)
    )
  )
  
  class(result) <- "biomedical_ttest"
  return(result)
}

# Paired t-test implementation
paired_t_test <- function(x, y, alternative = "two.sided", conf.level = 0.95, 
                          plot = TRUE, plot_title = "Paired T-Test") {
  #' Paired T-Test (Dependent Samples)
  #' 
  #' Tests whether the mean difference between paired observations is significantly different from zero
  #' 
  #' @param x numeric vector for condition 1 (e.g., before treatment)
  #' @param y numeric vector for condition 2 (e.g., after treatment)
  #' @param alternative character: "two.sided", "less", or "greater"
  #' @param conf.level confidence level for confidence interval
  #' @param plot logical: whether to create visualization
  #' @param plot_title character: title for the plot
  #' @return list with test results
  
  # Input validation
  if (!is.numeric(x) || !is.numeric(y)) stop("Both x and y must be numeric")
  if (length(x) != length(y)) stop("x and y must have the same length for paired test")
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop("alternative must be 'two.sided', 'less', or 'greater'")
  }
  
  # Remove pairs with missing values
  complete_pairs <- complete.cases(x, y)
  x <- x[complete_pairs]
  y <- y[complete_pairs]
  
  if (length(x) < 2) stop("Need at least 2 complete pairs")
  
  n <- length(x)
  differences <- x - y
  mean_diff <- mean(differences)
  sd_diff <- sd(differences)
  se_diff <- sd_diff / sqrt(n)
  
  # Calculate test statistic
  t_statistic <- mean_diff / se_diff
  df <- n - 1
  
  # Calculate p-value
  if (alternative == "two.sided") {
    p_value <- 2 * pt(abs(t_statistic), df, lower.tail = FALSE)
  } else if (alternative == "less") {
    p_value <- pt(t_statistic, df, lower.tail = TRUE)
  } else {  # greater
    p_value <- pt(t_statistic, df, lower.tail = FALSE)
  }
  
  # Confidence interval for mean difference
  alpha <- 1 - conf.level
  t_critical <- qt(1 - alpha/2, df)
  ci_lower <- mean_diff - t_critical * se_diff
  ci_upper <- mean_diff + t_critical * se_diff
  
  # Effect size (Cohen's d for paired samples)
  cohens_d <- mean_diff / sd_diff
  
  # Correlation between paired observations
  correlation <- cor(x, y)
  
  # Create visualization
  if (plot) {
    par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
    
    # 1. Before-after scatter plot
    plot(x, y, main = "Before vs After", xlab = "Before (x)", ylab = "After (y)",
         pch = 19, col = "blue", cex = 1.2)
    abline(0, 1, col = "red", lwd = 2, lty = 2)  # Line of equality
    abline(lm(y ~ x), col = "darkgreen", lwd = 2)  # Regression line
    legend("topleft", c("Equality Line", "Regression Line"), 
           col = c("red", "darkgreen"), lty = c(2, 1), lwd = 2, cex = 0.8)
    
    # 2. Differences histogram
    hist(differences, breaks = max(5, ceiling(sqrt(n))), probability = TRUE,
         main = "Distribution of Differences", xlab = "Differences (x - y)", 
         ylab = "Density", col = "lightgreen")
    abline(v = mean_diff, col = "red", lwd = 2, lty = 2)
    abline(v = 0, col = "black", lwd = 2, lty = 1)
    
    # Add normal curve overlay
    diff_seq <- seq(min(differences), max(differences), length.out = 100)
    normal_curve <- dnorm(diff_seq, mean = mean_diff, sd = sd_diff)
    lines(diff_seq, normal_curve, col = "blue", lwd = 2)
    
    # 3. Box plot of differences
    boxplot(differences, main = "Differences Box Plot", ylab = "Differences (x - y)",
            col = "lightblue")
    abline(h = mean_diff, col = "red", lwd = 2, lty = 2)
    abline(h = 0, col = "black", lwd = 2, lty = 1)
    
    # 4. Q-Q plot for differences
    qqnorm(differences, main = "Q-Q Plot of Differences")
    qqline(differences, col = "red", lwd = 2)
    
    # 5. Individual trajectories
    plot(rep(1, n), x, xlim = c(0.5, 2.5), ylim = range(c(x, y)),
         main = "Individual Changes", xlab = "Time Point", ylab = "Values",
         pch = 19, col = "blue", cex = 1.2)
    points(rep(2, n), y, pch = 19, col = "red", cex = 1.2)
    
    # Draw lines connecting paired observations
    for (i in 1:n) {
      lines(c(1, 2), c(x[i], y[i]), col = "gray", lwd = 0.5)
    }
    
    # Add means
    points(1, mean(x), pch = 15, col = "darkblue", cex = 2)
    points(2, mean(y), pch = 15, col = "darkred", cex = 2)
    lines(c(1, 2), c(mean(x), mean(y)), col = "black", lwd = 3)
    
    axis(1, at = c(1, 2), labels = c("Before", "After"))
    legend("topright", c("Individual", "Group Mean"), 
           pch = c(19, 15), col = c("blue", "black"), cex = 0.8)
    
    # 6. Test summary
    plot(1, 1, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
         main = "Test Summary", xlab = "", ylab = "", axes = FALSE)
    
    text(5, 9.5, paste("Paired T-Test Results"), cex = 1.2, font = 2)
    text(5, 8.8, paste("Before Mean:", round(mean(x), 4)), cex = 1)
    text(5, 8.3, paste("After Mean:", round(mean(y), 4)), cex = 1)
    text(5, 7.8, paste("Mean Difference:", round(mean_diff, 4)), cex = 1)
    text(5, 7.3, paste("t-statistic:", round(t_statistic, 4)), cex = 1)
    text(5, 6.8, paste("p-value:", round(p_value, 6)), cex = 1)
    text(5, 6.3, paste("df:", df), cex = 1)
    text(5, 5.8, paste(conf.level*100, "% CI: [", round(ci_lower, 4), ", ", round(ci_upper, 4), "]", sep = ""), cex = 0.9)
    text(5, 5.3, paste("Cohen's d:", round(cohens_d, 4)), cex = 1)
    text(5, 4.8, paste("Correlation:", round(correlation, 4)), cex = 1)
    
    # Significance and interpretation
    sig_level <- ifelse(p_value < 0.001, "***", 
                       ifelse(p_value < 0.01, "**", 
                             ifelse(p_value < 0.05, "*", "ns")))
    text(5, 4.1, paste("Significance:", sig_level), cex = 1, 
         col = ifelse(sig_level == "ns", "red", "darkgreen"))
    
    if (p_value < 0.05) {
      text(5, 3.4, "Result: Significant change", cex = 1, col = "darkgreen", font = 2)
    } else {
      text(5, 3.4, "Result: No significant change", cex = 1, col = "red", font = 2)
    }
    
    par(mfrow = c(1, 1))
  }
  
  # Return results
  result <- list(
    statistic = t_statistic,
    p_value = p_value,
    degrees_of_freedom = df,
    mean_before = mean(x),
    mean_after = mean(y),
    mean_difference = mean_diff,
    standard_error = se_diff,
    confidence_interval = c(ci_lower, ci_upper),
    confidence_level = conf.level,
    alternative = alternative,
    effect_size_cohens_d = cohens_d,
    correlation = correlation,
    method = "Paired t-test",
    sample_size = n,
    difference_summary = list(
      mean = mean_diff,
      sd = sd_diff,
      min = min(differences),
      max = max(differences),
      median = median(differences)
    )
  )
  
  class(result) <- "biomedical_ttest"
  return(result)
}

# Print method for biomedical t-test results
print.biomedical_ttest <- function(x, ...) {
  cat("\n", x$method, "\n")
  cat(rep("=", nchar(x$method) + 2), "\n", sep = "")
  
  if (grepl("One-sample", x$method)) {
    cat("Sample mean:", x$sample_mean, "\n")
    cat("Hypothesized mean:", x$hypothesized_mean, "\n")
  } else if (grepl("Paired", x$method)) {
    cat("Mean before:", x$mean_before, "\n")
    cat("Mean after:", x$mean_after, "\n")
    cat("Mean difference:", x$mean_difference, "\n")
    cat("Correlation:", x$correlation, "\n")
  } else {  # Two-sample
    cat("Group 1 mean:", x$mean_group1, "\n")
    cat("Group 2 mean:", x$mean_group2, "\n")
    cat("Mean difference:", x$mean_difference, "\n")
    cat("Equal variances assumed:", x$var_equal, "\n")
  }
  
  cat("t-statistic:", x$statistic, "\n")
  cat("Degrees of freedom:", x$degrees_of_freedom, "\n")
  cat("p-value:", x$p_value, "\n")
  cat("Alternative hypothesis:", x$alternative, "\n")
  cat(x$confidence_level * 100, "% Confidence interval: [", 
      x$confidence_interval[1], ", ", x$confidence_interval[2], "]\n", sep = "")
  cat("Effect size (Cohen's d):", x$effect_size_cohens_d, "\n")
  
  # Interpretation
  alpha <- 1 - x$confidence_level
  if (x$p_value < alpha) {
    cat("\nConclusion: Reject the null hypothesis (significant result)\n")
  } else {
    cat("\nConclusion: Fail to reject the null hypothesis (not significant)\n")
  }
  
  # Effect size interpretation
  d <- abs(x$effect_size_cohens_d)
  effect_magnitude <- if (d < 0.2) "negligible"
                     else if (d < 0.5) "small"
                     else if (d < 0.8) "medium"
                     else "large"
  cat("Effect size magnitude:", effect_magnitude, "\n")
}

# Comprehensive demonstration function
demonstrate_t_tests <- function() {
  cat("=== Comprehensive T-Tests for Biomedical Data Analysis ===\n\n")
  
  # Set random seed for reproducibility
  set.seed(123)
  
  # Example 1: One-sample t-test (Drug efficacy study)
  cat("1. ONE-SAMPLE T-TEST: Drug Efficacy Study\n")
  cat("Research Question: Does a new drug significantly reduce blood pressure below 120 mmHg?\n")
  cat("H0: μ = 120 (no effect)\n")
  cat("H1: μ < 120 (drug reduces BP)\n\n")
  
  # Simulate blood pressure data after drug treatment
  bp_after_drug <- rnorm(25, mean = 115, sd = 8)  # Simulated BP reduction
  
  cat("Blood pressure measurements after drug treatment (n=25):\n")
  cat(paste(round(bp_after_drug, 1), collapse = ", "), "\n\n")
  
  one_sample_result <- one_sample_t_test(bp_after_drug, mu = 120, 
                                        alternative = "less", 
                                        plot_title = "Drug Efficacy: BP Reduction")
  print(one_sample_result)
  
  cat("\n" , rep("=", 60), "\n\n")
  
  # Example 2: Two-sample t-test (Treatment comparison)
  cat("2. TWO-SAMPLE T-TEST: Treatment Comparison\n")
  cat("Research Question: Is there a difference in recovery time between two treatments?\n")
  cat("H0: μ1 = μ2 (no difference)\n")
  cat("H1: μ1 ≠ μ2 (treatments differ)\n\n")
  
  # Simulate recovery times
  treatment_a <- rnorm(20, mean = 12, sd = 3)  # Treatment A: 12 days average
  treatment_b <- rnorm(22, mean = 10, sd = 2.5) # Treatment B: 10 days average (better)
  
  cat("Treatment A recovery times (days, n=20):\n")
  cat(paste(round(treatment_a, 1), collapse = ", "), "\n\n")
  cat("Treatment B recovery times (days, n=22):\n")
  cat(paste(round(treatment_b, 1), collapse = ", "), "\n\n")
  
  two_sample_result <- two_sample_t_test(treatment_a, treatment_b, 
                                        var.equal = FALSE,  # Welch's t-test
                                        plot_title = "Treatment Comparison")
  print(two_sample_result)
  
  cat("\n" , rep("=", 60), "\n\n")
  
  # Example 3: Paired t-test (Before/After study)
  cat("3. PAIRED T-TEST: Before/After Clinical Study\n")
  cat("Research Question: Does the intervention significantly reduce cholesterol levels?\n")
  cat("H0: μd = 0 (no change)\n")
  cat("H1: μd > 0 (cholesterol decreases)\n\n")
  
  # Simulate paired cholesterol data
  n_patients <- 18
  baseline_cholesterol <- rnorm(n_patients, mean = 220, sd = 25)
  # Intervention reduces cholesterol by 15-25 points on average
  reduction <- rnorm(n_patients, mean = 20, sd = 8)
  after_cholesterol <- baseline_cholesterol - reduction
  
  cat("Patient cholesterol levels (mg/dL):\n")
  cat("Before intervention (n=18):\n")
  cat(paste(round(baseline_cholesterol, 1), collapse = ", "), "\n\n")
  cat("After intervention (n=18):\n")
  cat(paste(round(after_cholesterol, 1), collapse = ", "), "\n\n")
  
  paired_result <- paired_t_test(baseline_cholesterol, after_cholesterol, 
                                alternative = "greater",  # Before > After
                                plot_title = "Cholesterol Reduction Study")
  print(paired_result)
  
  cat("\n" , rep("=", 60), "\n\n")
  
  # Example 4: Power analysis demonstration
  cat("4. STATISTICAL POWER AND SAMPLE SIZE CONSIDERATIONS\n")
  cat("Demonstrating the relationship between sample size, effect size, and statistical power\n\n")
  
  # Different sample sizes for power demonstration
  sample_sizes <- c(5, 10, 20, 50)
  power_results <- data.frame(
    sample_size = sample_sizes,
    power_small_effect = NA,
    power_medium_effect = NA,
    power_large_effect = NA
  )
  
  for (i in seq_along(sample_sizes)) {
    n <- sample_sizes[i]
    # Simulate multiple t-tests with different effect sizes
    small_effect_power <- mean(replicate(1000, {
      x <- rnorm(n, mean = 0.2, sd = 1)  # Small effect (d = 0.2)
      result <- one_sample_t_test(x, mu = 0, plot = FALSE)
      result$p_value < 0.05
    }))
    
    medium_effect_power <- mean(replicate(1000, {
      x <- rnorm(n, mean = 0.5, sd = 1)  # Medium effect (d = 0.5)
      result <- one_sample_t_test(x, mu = 0, plot = FALSE)
      result$p_value < 0.05
    }))
    
    large_effect_power <- mean(replicate(1000, {
      x <- rnorm(n, mean = 0.8, sd = 1)  # Large effect (d = 0.8)
      result <- one_sample_t_test(x, mu = 0, plot = FALSE)
      result$p_value < 0.05
    }))
    
    power_results[i, 2:4] <- c(small_effect_power, medium_effect_power, large_effect_power)
  }
  
  cat("Statistical Power Analysis (1000 simulations each):\n")
  print(round(power_results, 3))
  
  cat("\nInterpretation:\n")
  cat("- Small effect size (d=0.2) requires large samples for adequate power\n")
  cat("- Medium effect size (d=0.5) achievable with moderate samples\n")
  cat("- Large effect size (d=0.8) detectable even with small samples\n")
  cat("- Aim for power ≥ 0.80 (80%) in study design\n\n")
  
  cat("Clinical Significance Guidelines:\n")
  cat("- Always consider clinical significance alongside statistical significance\n")
  cat("- Effect sizes help interpret practical importance of findings\n")
  cat("- Confidence intervals provide range of plausible effect sizes\n")
  cat("- Consider Type I (false positive) and Type II (false negative) error rates\n")
}

# Run demonstration if script is executed directly
if (sys.nframe() == 0) {
  demonstrate_t_tests()
}